module Generate.Universal (
    generateUTM
) where

import Data.List

import Machine (Transition(Transition), State, Action(..), Letter)
import Generate.Utils (Alphabet)

-- [This is not valid haskell code]
-- Describes small bricks generators used to create the UTM

type Name = String
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

expect :: Name -> [Letter] -> [Outcome] -> Coherant -> Component
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
search_left :: Name -> Letter -> Outcome -> Coherant -> Component
search_left name letter outcome (c_state, c_action) =
    let failed_transition = (c_state, ">", c_action) in
    expect name [letter, ">"] [outcome, failed_transition] (name, LEFT)

-- Scans the tape to the right serching for a specific Letter
-- IF found Letter, first Outcome is executed
-- ELSE found Blank, second Outcome is executed
search_right :: Name -> Letter -> Outcome -> Coherant -> Component
search_right name letter outcome (c_state, c_action) =
    let failed_transition = (c_state, ".", c_action) in
    expect name [letter, "."] [outcome, failed_transition] (name, RIGHT)

generateUTM = show $ search_left "search_Y_left" "Y" ("found", "Y", RIGHT) ("notfound", LEFT)

{-
search_not_right :: Name -> Letter -> Outcome -> Component
    -- Scans the tape to the right serching for other than Letter
    -- IF found other than Letter, Outcome is executed


copy_from_to :: Name -> Letter -> Letter -> Letter -> Outcome -> Component
copy_from_to name e x y out =
    --  [unknown]
    --  XBBeeee000Yee.........
    search_left "search_X_left"
        x
        ("copy_e_from_X_to_Y" , x, RIGHT)
        ("search_X_right", RIGHT) ++
    search_right "search_X_right"
        x
        ("copy_e_from_X_to_Y" , x, RIGHT)
        stop ++
    --   ⇣
    --  XBBeeee000Yee.......
    expect "copy_e_from_X_to_Y"
        [e, "B"]
        [("copy_e_from_X_to_Y[search_Y_right]", "B", RIGHT), ("copy_e_from_X_to_Y", "B", RIGHT)]
        ("finished", LEFT) ++
    --     ⇣
    --  XBBBeee000Yee.......
    search_right "copy_e_from_X_to_Y[search_Y_right]"
        y
        ("copy_e_from_X_to_Y[search_not_e_right]", y)
        stop ++
    --             ⇣
    --  XBBBeee000Yee.......
    search_not_right "copy_e_from_X_to_Y[search_not_e_right]"
        e
        ("search_X_left", e, "LEFT") ++
    --               ⇣
    --  XBBBeee000Yeee......

    --  [unknown]
    --  XBBBBBB000Yeeeeee......
    search_left "finished"
        x
        ("finished[replace_B]", x, RIGHT)
        stop ++
    --   ⇣
    --  XBBBBBB000Yeeeeee......
    expect "finished[replace_B]"
        ["B"]
        ("finished[replace_B]", e, RIGHT)
        out
    --         ⇣
    --  Xeeeeee000Yeeeeee......

-}

