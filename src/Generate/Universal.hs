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
        (name' "search_X_right", RIGHT)
    ++
    search_right (name' "search_X_right")
        e
        out
        fail

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

-- Copy every specified 'e' from after specified 'X' to after specified 'Y'
-- Calls provided Coherant at X
-- ex: e = 1.
--      X111100Y01011.......
--  --> X111100Y111101011..
--      ↑
copy_from_to :: State -> Letter -> Letter -> Letter -> Coherant -> Component
copy_from_to name e x y out =
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
    search_right (name' "copy_e_from_X_to_Y[search_Y_right]") -- Search for Y
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

generateUTM =
    let transitions_list = copy_from_to "utm" "1" "X" "Y" stop in
    let transitions = fromList transitions_list in
    let finals = ["STOP"] in
    let states = (map (\(name, _) -> name) transitions_list) ++ finals in
    Machine
        "UTM"              -- name
        alphabet           -- alphabet
        "."                -- blank
        states             -- states
        "utm"              -- initial
        finals             -- finals
        transitions        -- transitions
