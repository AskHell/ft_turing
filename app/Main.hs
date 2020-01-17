module Main where

import qualified Data.ByteString.Lazy          as B
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           Data.Map.Strict                ( fromList
                                                , member
                                                , insert
                                                )
import           System.IO
import           Data.List               hiding ( insert )
import           Text.Printf                    ( printf )

import           Machine                        ( Machine(..)
                                                , encode
                                                , eitherDecode
                                                )
import           Execute                        ( MachineState(..)
                                                , step
                                                , Index
                                                , Tape
                                                )
import           Generate.Palindrome            ( generatePalindrome )
import           Generate.Universal             ( generateUTM )
import           BigO                           ( bigO )
import           Translate                     as T

execute :: Machine -> MachineState -> String -> Index -> String
execute m ms acc i
    | elem (state ms) (finals m) = printf "%s\n%s" acc $ show ms
    | member ms i = printf "%s\n%s\nmachine killed (machine is stuck)" acc
    $ show ms
execute m ms acc i =
    let new_index = insert ms True i
    in  let new_ms = step m ms
        in  let new_acc = printf "%s\n%s" acc $ show ms
            in  execute m new_ms new_acc new_index

ft_turing :: String -> Either String Machine -> String
ft_turing _ (Left s) = s
ft_turing tape (Right m) =
    let machine_state = MachineState 0 (initial m) tape
    in  printf "%s\n" $ execute m machine_state "" $ fromList []

ft_translate :: String -> Either String Machine -> String
ft_translate _ (Left s) = s
ft_translate tape (Right m) =
    let machine_state = MachineState 0 (initial m) tape
    in  printf "%s\n" $ T.translate m machine_state

usage :: IO ()
usage =
    putStrLn
        "usage: ft_turing [-h] jsonfile input\n\npositional arguments:\n  jsonfile\t\tjson description of the machine\n  input\t\t\tinput of the machine\n\noptional arguments:\n  -h, --help\t\tshow this help message and exit"

display_error :: String -> IO ()
display_error s = putStrLn $ printf "\x1b[31mError\x1b[0m: %s" s

dispatch :: [String] -> IO ()
dispatch ("generate" : machine : _)
    | machine == "palindrome" = putStrLn $ generatePalindrome
    | machine == "UTM"        = putStrLn $ show $ generateUTM
dispatch ("run" : "UTM" : tape : _) =
    putStrLn $ ft_turing tape $ Right generateUTM
dispatch (help : _) | help == "-h" || help == "--help" = usage
dispatch (subprogram : machine : tape : _)
    | (not $ machine == []) && (not $ tape == []) && subprogram == "run" = do
        file_content <- B.readFile machine
        putStrLn $ ft_turing tape $ eitherDecode file_content
    | (not $ machine == []) && (not $ tape == []) && subprogram == "translate" = do
        file_content <- B.readFile machine
        putStrLn $ ft_translate tape $ eitherDecode file_content
    | machine == [] = display_error "jsonfile is an empty string"
    | tape == [] = display_error "input is an empty string"
dispatch ("bigO" : machine : _) = do
    file_content <- B.readFile machine
    case eitherDecode file_content of
        Right machine -> putStrLn $ bigO $ transitions machine
        Left  err     -> display_error err
dispatch _ = usage

main = do
    args <- getArgs
    dispatch args
