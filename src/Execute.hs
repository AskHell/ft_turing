module Execute (
    MachineState(..),
    step,
    Tape,
    Index
) where

import Data.Map.Strict as M
import Data.List as L
import Data.Map.Strict (fromList, Map(..))
import Text.Printf (printf)

import Machine (Machine(..), Letter, State, Transition(..), Action(..))
import Prelude hiding (read)

type Tape = String

data MachineState = MachineState {
    position    :: Int,
    state       :: State,
    input       :: Tape
} deriving (Eq, Ord)

type Index = Map MachineState Bool

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

change_by_id :: Int -> Letter -> (Int, Char) -> Char
change_by_id id new (i, _)
    | id == i =
        head new
change_by_id _ _ (_, a) = a

change_pos :: Action -> Int -> Int
change_pos LEFT i = i - 1
change_pos RIGHT i = i + 1

itemOf :: Int -> [a] -> Maybe a
itemOf _ [] = Nothing
itemOf 0 (head : _) = Just head
itemOf i (_ : tail) = itemOf (i - 1) tail

execute :: Machine -> MachineState -> Maybe Transition -> MachineState
execute machine machine_state (Just transition) =
    let partial_change_by_id = change_by_id (position machine_state) (write transition) in
    let new_input = L.map partial_change_by_id $ enumerate $ input machine_state in
    let new_pos = change_pos (action transition) (position machine_state) in
    let new_state = to_state transition in
    MachineState new_pos new_state new_input
execute machine machine_state Nothing = machine_state

default_list :: Maybe [a] -> [a]
default_list (Just a) = a
default_list (Nothing) = []

default_read :: Maybe Char -> Letter -> Letter
default_read (Just c) _ = [c]
default_read (Nothing) def = def

extandList :: Letter -> Int -> Tape -> (Letter, Tape)
extandList blank index tape
    | index < 0 = (blank, tape)
    | length tape > index = ([tape !! index], tape)
    | length tape <= index =
        let curr = length tape in
        let to_add = index - curr in
        (blank, tape ++ [ blank !! 0 | _ <- [0..to_add] ])

step :: Machine -> MachineState -> MachineState
step machine machine_state
    -- current state is part of [finals]
    | elem (state machine_state) $ finals machine =
        machine_state
step machine machine_state =
    -- lookup list: (read, transition). Ex: (".", {read: ".", to_state: "scanright", ...})
    let transition_list = default_list $ M.lookup (state machine_state) $ transitions machine in
    let possible_actions = [ (read x, x) | x <- transition_list ] in
    let (current_read, new_tape) = extandList (blank machine) (position machine_state) (input machine_state) in
    let new_machine_state = MachineState (position machine_state) (state machine_state) new_tape in
    execute machine new_machine_state $ L.lookup current_read possible_actions

showMachineState :: MachineState -> String
showMachineState ms =
    let pos_string = [ ' ' | _ <- [0..position ms - 1] ] in
    printf "┏ %s\t- [%s]\n┗ %s⇣" (input ms) (state ms) pos_string

instance Show MachineState where
    show ms = showMachineState ms
