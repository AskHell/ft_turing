module Main where

import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs, getProgName)
import Data.Map.Strict (fromList, member, insert)
import System.IO
import Data.List hiding (insert)
import Text.Printf (printf)

import Machine ( Machine(..), encode, eitherDecode )
import Execute ( MachineState(..), step, Index, Tape )
import Generate ( generatePalindrome )

execute :: Machine -> MachineState -> String -> Index -> String
execute m ms acc i
    | elem (state ms) (finals m) =
        printf "%s\n%s" acc $ show ms
    | member ms i =
       printf "%s\n%s\nmachine killed (machine is stuck)" acc $ show ms
execute m ms acc i =
    let new_index = insert ms True i in
    let new_ms = step m ms in
    let new_acc = printf "%s\n%s" acc $ show ms in
    execute m new_ms new_acc new_index

ft_turing :: String -> Either String Machine -> String
ft_turing _ (Left s) = s
ft_turing tape (Right m) =
    let machine_state = MachineState 0 (initial m) tape in
    printf "%s\n" $ execute m machine_state "" $ fromList []

usage :: IO ()
usage = putStrLn "usage: ft_turing [-h] jsonfile input\n\npositional arguments:\n  jsonfile\t\tjson description of the machine\n  input\t\t\tinput of the machine\n\noptional arguments:\n  -h, --help\t\tshow this help message and exit"

display_error :: String -> IO ()
display_error s = putStrLn $ printf "\x1b[31mError\x1b[0m: %s" s

dispatch :: [String] -> IO ()
dispatch ("generate" : machine : _)
    | machine == "palindrome" =
        putStrLn $ generatePalindrome
dispatch (help : _)
    | help == "-h" || help == "--help" =
        usage
dispatch (machine : input : _)
    | (not $ machine == []) && (not $ input == []) = do -- when both are valid
        file_content <- B.readFile machine
        putStrLn $ ft_turing input $ eitherDecode file_content
    | machine == [] =
        display_error "jsonfile is an empty string"
    | input == [] =
        display_error "input is an empty string"
dispatch _ =
    usage

main = do
    args <- getArgs
    dispatch args
