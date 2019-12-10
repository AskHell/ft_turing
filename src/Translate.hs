module Translate (
    translate
) where
    
import Prelude hiding (lookup, read)

import Data.Map.Strict (Map, fromList, toList, lookup)
import Text.Printf (printf)

import Machine (State, Letter, Machine(..), Transition(..), Action(..))
import Execute (MachineState(..), Tape)
import Utils (enumerate, unwrap')

type TranslateTable a = Map a String

translateSymbols :: [String] -> TranslateTable String
translateSymbols l = 
    let list = map (\(i, x) -> (x, [ '1' | _ <- [0..i] ])) $ enumerate l in
    fromList list

reassemble :: (State, [Transition]) -> [(State, Transition)]
reassemble (s, l) =
    map (\t -> (s, t)) l

translateTransition :: TranslateTable Letter -> TranslateTable State -> (State, Transition) -> String
translateTransition t_alphabet t_states (s, t) =
    let t_s = unwrap' "" $ lookup s t_states in
    let t_read = unwrap' "" $ lookup (read t) t_alphabet in
    let t_to_state = unwrap' "" $ lookup (to_state t) t_states in
    let t_write = unwrap' "" $ lookup (write t) t_alphabet in
    let t_action = case action t of LEFT -> "1"
                                    RIGHT -> "11"
    in
    printf "%s0%s0%s0%s0%s00" t_s t_read t_to_state t_write t_action

translateState :: TranslateTable Letter -> TranslateTable State -> Map State [Transition] -> [String]
translateState t_alphabet t_states states =
    let states_list = toList states in
    let transition_list = foldl (\acc x -> acc ++ reassemble x) [] states_list in
    map (translateTransition t_alphabet t_states) transition_list

translateSymbol :: TranslateTable Letter -> Letter -> String
translateSymbol t_alphabet a =
    unwrap' "" $ lookup a t_alphabet

translateTape :: TranslateTable Letter -> Tape -> Tape
translateTape t_alphabet tape =
    let t_tape = map (\x -> translateSymbol t_alphabet [x]) tape in
    foldl (\a x -> a ++ x ++ "0") "" t_tape

translate :: Machine -> MachineState -> Tape
translate m ms =
    let t_alphabet = translateSymbols $ alphabet m in
    let t_states = translateSymbols $ states m in
    let t_transitions = translateState t_alphabet t_states $ transitions m in
    let t_initial = unwrap' "" $ lookup (initial m) t_states in
    let t_head = translateSymbol t_alphabet [head $ input ms] in
    let t_tape = translateTape t_alphabet $ input ms in
    ">" ++
    [ '0' | _ <- [0..(length t_alphabet + length t_states + 1)]] ++ "Y" ++
    foldl (\a x -> x ++ a) "" t_transitions ++ "0Z" ++
    t_tape


