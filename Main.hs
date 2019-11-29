import Control.Exception (catch, displayException)
import System.Environment (getArgs)

import TuringMachine (eitherDecode)
import IOParsing (parseArgs, usage)

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do
  args <- getArgs
  action <- parseArgs args
  case action of
    Left invalid -> usage invalid
    Right (description, input) -> putStrLn "Hooray"

handler :: IOError -> IO ()
handler e = putStrLn $ displayException e
