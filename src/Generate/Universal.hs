module Generate.Universal (
    generateUTM
) where

import Data.List
import Data.Map.Strict (Map, fromList)
import Text.Printf (printf)

import Machine (Transition(Transition), State, Action(..), Letter, Machine(Machine))
import Generate.Utils (Alphabet, fromMachineToJson)

-- [This is not valid haskell code]
-- Describes small bricks generators used to create the UTM

type Component = [(State, [Transition])]

type Outcome = (State, Letter, Action)
    -- An Outcome describes what a Brick must do when it's in one of it's termination state
    -- set the machine into State
    -- writes Letter
    -- execute Action
type Coherant = (State, Action)
    -- A Coherant outcome ensures that whatever was read, it won't be altered.
    -- set the machine into State
    -- execute Action

alphabet = [".", ">", "1", "0", "X", "Y", "B", "Z"]

stop = ("STOP", LEFT)
false = ("FALSE", LEFT)
true = ("TRUE", LEFT)

encapsulate :: State -> State -> State
encapsulate name sub_name = printf "%s[%s]" name sub_name

expect :: State -> [Letter] -> [Outcome] -> Coherant -> Component
    -- Scans the current case and execute specified Outcome
    -- IF scanned is in [Letter], corresponding Outcome is executed
    -- ELSE second Outcome is executed
expect name letters outcomes (c_state, c_action) =
    let diff_alpha = alphabet \\ letters in
    [ (name,
        [ Transition read to_state write todo | (read, (to_state, write, todo)) <- (zip letters outcomes) ] ++
        [ Transition read c_state read c_action | read <- diff_alpha ]
    ) ]

-- Calls the Coherant whatever Letter is read
apply :: State -> Coherant -> Component
apply name out =
    expect name [] [] out

-- Scans the tape to the left searching for a specific Letter
-- IF found Letter, first Outcome is executed
-- ELSE found '>', second Outcome is executed
search_left :: State -> Letter -> Outcome -> Coherant -> Component
search_left name letter outcome (c_state, c_action) =
    let failed_transition = (c_state, ">", c_action) in
    expect name
        [letter, ">"]
        [outcome, failed_transition]
        (name, LEFT)

-- Scans the tape to the right serching for a specific Letter
-- IF found Letter, first Outcome is executed
-- ELSE found Blank, second Outcome is executed
search_right :: State -> Letter -> Outcome -> Coherant -> Component
search_right name letter outcome (c_state, c_action) =
    let failed_transition = (c_state, ".", c_action) in
    expect name
        [letter, "."]
        [outcome, failed_transition]
        (name, RIGHT)

-- Scans the tape to the right serching for other than Letter
-- IF found other than Letter, Outcome is executed
search_not_right :: State -> Letter -> Outcome -> Component
search_not_right name letter outcome =
    let diff_alpha = alphabet \\ [letter] in
    expect name
        diff_alpha
        [ outcome | _ <- diff_alpha ]
        (name, RIGHT)

-- Does a both way search of 'e' starting with Left
both_way_search_left :: State -> Letter -> Outcome -> Coherant -> Component
both_way_search_left name e out fail =
    let name' = encapsulate name in
    search_left name -- Search for X
        e
        out
        (name' "search_right", RIGHT)
    ++
    search_right (name' "search_right")
        e
        out
        fail

-- Does a both way search of 'e' starting with Left
both_way_search_right :: State -> Letter -> Outcome -> Coherant -> Component
both_way_search_right name e out fail =
    let name' = encapsulate name in
    search_right name -- Search for X
        e
        out
        (name' "search_left", LEFT)
    ++
    search_left (name' "search_left")
        e
        out
        fail

-- Replaces all of [Letter] by provided Letter and stops at one of provided [Letter]
-- Calls Coherant when found one of stops letters
replace_to :: State -> [Letter] -> [Letter] -> Letter -> Coherant -> Component
replace_to name es stops replace out =
    let not_stops = alphabet \\ ("." : stops) in
    expect name
        (es ++ not_stops)
        ([ (name, replace, RIGHT) | _ <- es ] ++ [ (name, nt, RIGHT) | nt <- not_stops ])
        out

replace_at_left_to :: State -> Letter -> [Letter] -> [Letter] -> Letter -> Coherant -> Component
replace_at_left_to name at es stops replace out =
    let name' = encapsulate name in
    search_left name at (name' "finish_replace", at, RIGHT) stop ++
    replace_to (name' "finish_replace") es stops replace out

-- Check if Letters are all the same stoping at one of provided [Letter].
-- Calls first Coherant if valid, second if invalid. Replaces all valid letters by provided Letter
-- ex: is_only _ e ["0"] 1 true false ->
--      eeeeee0000 -> true ->   1111110000
--      eeee1e0000 -> false ->  1111110000
is_only :: State -> Letter -> [Letter] -> Letter -> Coherant -> Coherant -> Component
is_only name e stops replace out_true out_false =
    let name' = encapsulate name in
    let not_stops = alphabet \\ ("." : stops) in
    expect name
        (e : not_stops)
        ((name, replace, RIGHT) : [ (name' "finish", replace, RIGHT) | a <- not_stops ])
        out_true
    ++
    replace_to (name' "finish")
        [e]
        ("." : stops)
        replace
        out_false

-- Moves 1 case right everything after current position
-- First case is filled by provided e. Calls provided Coherant at the end of the tape
--      1000111010
--      e1000111010
shift_right :: State -> Letter -> Coherant -> Component
shift_right name e (to_state, action) =
    let name' = encapsulate name in
    let no_blank = delete "." alphabet in
    let name_list = [ name' $ "shift_" ++ a | a <- no_blank ] in
    let generate_outcomes = (\a -> [ (n, a, RIGHT) | n <- name_list ]) in
    expect name -- First element placement
        ("." : no_blank)
        ((to_state, e, action) : generate_outcomes e)
        (to_state, action)
    ++
    [ head $ expect (name' $ "shift_" ++ a) -- Other folowing elements
        ("." : no_blank)
        ((to_state, a, action) : generate_outcomes a)
        (to_state, action)
    | a <- no_blank ]

-- Moves 1 case left everything after current position
-- Calls provided Coherant at the provided Letter
--      1000111010
--      000111010.
shift_left :: State -> Letter -> Coherant -> Component
shift_left name l_stop (to_state, action) =
    let name' = encapsulate name in
    let no_stop = delete l_stop $ delete ">" alphabet in
    let name_list = [ name' $ "shift_" ++ a | a <- no_stop ] in
    let generate_outcomes = (\a -> [ (n, a, LEFT) | n <- name_list ]) in
    search_right name "." (name' "shift_.", ".", LEFT) (name' "shift_.", LEFT)
    ++
    [ head $ expect (name' $ "shift_" ++ a) -- Other folowing elements
        (l_stop : ">" : no_stop)
        ((to_state, l_stop, action) : (to_state, ">", action) : generate_outcomes a)
        (to_state, action)
    | a <- no_stop ]

-- Removes every element between two Letter, shifting the right side of the tape over it.
-- Calls Coherant on the first letter
--      X11110Y1111
--      X0Y1111
collapse_to :: State -> Letter -> Letter -> Coherant -> Component
collapse_to name from to out =
    let name' = encapsulate name in
    let no_stop = alphabet \\ [from, to, "."] in
    expect name
        no_stop
        [(name' "collapse_one", a, RIGHT) | a <- no_stop ]
        out
    ++
    shift_left (name' "collapse_one")
        from
        (name, RIGHT)

-- Copy every specified 'e' from after specified 'X' to after specified 'Y'
-- Calls provided Coherant at X
-- ex: e = 1.
--      X111100Y01011.......
--  --> X111100Y111101011..
--      ↑
copy :: State -> Letter -> Letter -> Letter -> Coherant -> Component
copy name e x y out =
    let name' = encapsulate name in
    both_way_search_left name x (name' "copy_e_from_X_to_Y", x, RIGHT) stop ++ -- search x
    expect (name' "copy_e_from_X_to_Y") -- Search next e ignoring B, replace this e by B
        [e, "B"]
        [
            (name' "copy_e_from_X_to_Y[search_Y_right]", "B", RIGHT),
            (name' "copy_e_from_X_to_Y", "B", RIGHT)
        ]
        (name' "finished", LEFT)
    ++
    both_way_search_right (name' "copy_e_from_X_to_Y[search_Y_right]") -- Search for Y
        y
        (name' "copy_e_from_X_to_Y[shift_right]", y, RIGHT)
        stop
    ++
    shift_right (name' "copy_e_from_X_to_Y[shift_right]") -- Shift everything to right and put e at the beginning
        e
        (name, LEFT)
    ++
    search_left (name' "finished")
        "B"
        (name' "finished[replace_B]", e, LEFT)
        stop
    ++
    expect (name' "finished[replace_B]")
        ["B"]
        [(name' "finished[replace_B]", e, LEFT)]
        out

-- Substitute Symbol of second Letter by symbol of the first Letter stopping at the third letter
-- Calls Coherant at unknown place
--      X1110Y10
--      X1110Y1110
substitute :: State -> Letter -> Letter -> Letter -> Letter -> Coherant -> Component
substitute name e from to l_stop out =
    let name' = encapsulate name in
    both_way_search_right name to (name' "collapse", to, RIGHT) stop ++
    collapse_to (name' "collapse") to l_stop (name' "copy", RIGHT) ++
    copy (name' "copy") e from to out

-- Check if the number of specified 'e's are the same for provided X and Y.
-- Calls first Coherant if True, calls the second Coherant if False.
match :: State -> Letter -> Letter -> Letter -> Coherant -> Coherant -> Component
match name e x y valid invalid =
    let name' = encapsulate name in
    both_way_search_left name x (name' "match", x, RIGHT) stop ++
    expect (name' "match") -- Search next e ignoring B replace this e by B
        [e, "B"]
        [
            (name' "match[search_Y_right]", "B", RIGHT),
            (name' "match", "B", RIGHT)
        ]
        (name' "match[final_X]", RIGHT)
    ++
    search_right (name' "match[search_Y_right]") -- Goto Y
        y
        (name' "match[find_end_Y]", y, RIGHT)
        stop
    ++
    expect (name' "match[find_end_Y]") -- Search next e ignoring B replace this e by B
        [e, "B"]
        [
            (name, "B", RIGHT),
            (name' "match[find_end_Y]", "B", RIGHT)
        ]
        (name' "match[final_Y_invalid]", RIGHT)
    ++
    replace_at_left_to (name' "match[final_Y_invalid]")
        x ["B", e] ["0"] e (name' "match[invalidate_right]", RIGHT) ++
    both_way_search_left (name' "match[final_Y]")
        x
        (name' "match[final_Y[check_X]]", x, RIGHT) stop
    ++
    is_only (name' "match[final_Y[check_X]]")
        "B"
        ["0", x, y]
        e
        (name' "match[validate_right]", LEFT)
        (name' "match[invalidate_right]", LEFT)
    ++
    both_way_search_right (name' "match[final_X]")
        y
        (name' "match[final_X[check_Y]]", y, RIGHT) stop
    ++
    is_only (name' "match[final_X[check_Y]]")
        "B"
        ["0", x, y]
        e
        (name' "match[validate_left]", LEFT)
        (name' "match[invalidate_left]", LEFT)
    ++
    search_right (name' "match[validate_right]")
        y
        ((name' "match[validate_right[replace]]"), y, RIGHT)
        stop
    ++
    replace_to (name' "match[validate_right[replace]]")
        ["B"]
        ["0"]
        e
        valid
    ++
    search_left (name' "match[validate_left]")
        x
        ((name' "match[validate_left[replace]]"), x, RIGHT)
        stop
    ++
    replace_to (name' "match[validate_left[replace]]")
        ["B"]
        ["0"]
        e
        valid
    ++
    search_right (name' "match[invalidate_right]")
        y
        ((name' "match[invalidate_right[replace]]"), y, RIGHT)
        stop
    ++
    replace_to (name' "match[invalidate_right[replace]]")
        ["B"]
        ["0"]
        e
        invalid
    ++
    search_left (name' "match[invalidate_left]")
        x
        ((name' "match[invalidate_left[replace]]"), x, RIGHT)
        stop
    ++
    replace_to (name' "match[invalidate_left[replace]]")
        ["B"]
        ["0"]
        e
        invalid

generateUTM =
    let transitions_list = copy "utm" "1" "Y" "X" stop in
    let transitions = fromList transitions_list in
    let finals = ["STOP", "TRUE", "FALSE"] in
    let states = (map (\(name, _) -> name) transitions_list) ++ finals in
    Machine
        "UTM"              -- name
        alphabet           -- alphabet
        "."                -- blank
        states             -- states
        "utm"              -- initial
        finals             -- finals
        transitions        -- transitions
