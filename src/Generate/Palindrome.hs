module Generate.Palindrome (
    generatePalindrome
) where

import Data.Map.Strict (fromList)
import Data.Char (toUpper)
import Data.ByteString (ByteString)

import Machine as M
import Generate.Utils (Alphabet, fromMachineToJson)

stringToUpper :: String -> String
stringToUpper = map toUpper

alphaset = "abcdefghijklmnopqrstuvwxyz"
lowerAlphabet = [ [x] | x <- alphaset ]
upperAlphabet = [ [toUpper x] | x <- alphaset ]
goto_end current = "goto_end_" ++ current
check current = "check_" ++ current

generateInit :: (Alphabet, Alphabet) -> (M.State, [M.Transition])
generateInit (alpha, sub_alpha) =
    ( "init",
    [ Transition "." "True_phase_1" "." M.LEFT] ++
    [ Transition x (goto_end x) (stringToUpper x) M.RIGHT | x <- alpha ] ++
    [ Transition x "init" x M.RIGHT | x <- sub_alpha ])

generateGotoEnd :: (Alphabet, Alphabet) -> M.Letter -> (M.State, [M.Transition])
generateGotoEnd (alpha, sub_alpha) current =
    ( goto_end current,
    [ M.Transition "." (check current) "." M.LEFT ] ++
    [ M.Transition x (check current) x M.LEFT | x <- sub_alpha ] ++
    [ M.Transition x (goto_end current) x M.RIGHT | x <- alpha ])

generateGotoStart :: (Alphabet, Alphabet) -> (M.State, [M.Transition])
generateGotoStart (alpha, sub_alpha) =
    ("goto_start",
    [ Transition x "init" x M.RIGHT | x <- sub_alpha ++ ["."] ] ++
    [ Transition x "goto_start" x M.LEFT | x <- alpha])

generateCheck :: (Alphabet, Alphabet) -> M.Letter -> (M.State, [M.Transition])
generateCheck (alpha, sub_alpha) current =
    let sub_current = stringToUpper current in
    let clean_alpha = filter (\x -> not $ x == current) alpha in
    ( check current,
    [ Transition current "goto_start" sub_current M.LEFT] ++
    [ Transition x "False_phase_1" x M.LEFT | x <- clean_alpha ] ++
    [ Transition x "True_phase_1" x M.LEFT | x <- sub_alpha ])

generatePhase1 :: (Alphabet, Alphabet) -> Bool -> (M.State, [M.Transition])
generatePhase1 (alpha, sub_alpha) valid =
    let name = show valid ++ "_phase_1" in
    ( name,
    [ Transition "." (show valid ++ "_phase_2") "." M.RIGHT ] ++
    [ Transition x name x M.LEFT | x <- alpha ++ sub_alpha ])

generatePhase2 :: (Alphabet, Alphabet) -> Bool -> (M.State, [M.Transition])
generatePhase2 (alpha, sub_alpha) valid =
    let letter = case valid of  False -> "n"
                                True -> "y"
    in
    let name = show valid ++ "_phase_2" in
    ( name,
    [ Transition "." (show valid) letter M.RIGHT ] ++
    [ Transition y name x M.RIGHT | (x, y) <- zip alpha sub_alpha ] ++
    [ Transition x name x M.RIGHT | x <- alpha ]
    )

generateStates :: (Alphabet, Alphabet) -> [(M.State, [M.Transition])]
generateStates (l, u) =
    [generateInit (l, u)] ++
    (map (generateGotoEnd (l, u)) l) ++
    (map (generateCheck (l, u)) l) ++
    [generatePhase1 (l, u) True] ++
    [generatePhase1 (l, u) False] ++
    [generatePhase2 (l, u) True] ++
    [generatePhase2 (l, u) False] ++
    [generateGotoStart (l, u)]

generatePalindrome :: String
generatePalindrome =
    let transitions_list = generateStates (lowerAlphabet, upperAlphabet) in
    let transitions = fromList transitions_list in
    let finals = [ "True", "False" ] in
    let states = finals ++ [ name | (name, _) <- transitions_list ] in
    let alphaset = [ ".", "y", "n" ] ++ lowerAlphabet ++ upperAlphabet in
    fromMachineToJson $ Machine "palindrome" alphaset "." states "init" finals transitions
