module Execute (
    MachineState(..),
    step
) where

import Machine (Machine(..), Letter, State, Transition(..), Action(..))
import Prelude hiding (read)
import Data.Map.Strict as M
import Data.List as L
import Data.Map.Strict (fromList)
import Text.Printf (printf)

data MachineState = MachineState {
    position    :: Int,
    state       :: State,
    input       :: String,
    machine     :: Machine
}

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

execute :: MachineState -> Maybe Transition -> MachineState
execute machine_state (Just transition) =
    let partial_change_by_id = change_by_id (position machine_state) (write transition) in
    let new_input = L.map partial_change_by_id $ enumerate $ input machine_state in
    let new_pos = change_pos (action transition) (position machine_state) in
    let new_state = to_state transition in
    MachineState new_pos new_state new_input (machine machine_state)
execute machine_state Nothing = machine_state

default_list :: Maybe [a] -> [a]
default_list (Just a) = a
default_list (Nothing) = []

default_read :: Maybe Char -> Letter -> Letter
default_read (Just c) _ = [c]
default_read (Nothing) def = def

step :: MachineState -> MachineState
step machine_state
    -- current state is part of [finals]
    | elem (state machine_state) $ finals $ machine machine_state =
        machine_state
step machine_state =
    -- lookup list: (read, transition). Ex: (".", {read: ".", to_state: "scanright", ...})
    let transition_list = default_list $ M.lookup (state machine_state) $ transitions $ machine machine_state in
    let possible_actions = [ (read x, x) | x <- transition_list ] in
    let current_read = default_read (itemOf (position machine_state) (input machine_state)) $ blank $ machine machine_state in
    execute machine_state $ L.lookup current_read possible_actions

showMachineState :: MachineState -> String
showMachineState ms =
    let pos_string = [ ' ' | _ <- [0..position ms - 1] ] in
    printf "┏ %s\t- [%s]\n┗ %s⇣" (input ms) (state ms) pos_string

instance Show MachineState where
    show ms = showMachineState ms
