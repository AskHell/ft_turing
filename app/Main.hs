module Main where

import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs, getProgName)
--import System.Directory
import System.IO
import Data.List
import Text.Printf (printf)

import Machine ( Machine(..), encode, eitherDecode )
import Execute ( MachineState(..), step )
import Generate ( generatePalindrome )

execute :: MachineState -> String -> Int -> String
execute m acc i
    | elem (state m) (finals $ machine m) =
        printf "%s\n%s" acc $ show m
    | i == 0 =
       printf "%s\n%s" acc $ show m
execute m acc i =
    let new_m = step m in
    let new_acc = printf "%s\n%s" acc $ show m in
    execute new_m new_acc (i - 1)

ft_turing :: String -> Either String Machine -> String
ft_turing _ (Left s) = s
ft_turing tape (Right m) =
    let machine_state = MachineState 0 (initial m) tape m in
    printf "%s\n" $ execute machine_state "" 10000

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
