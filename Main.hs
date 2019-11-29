import Control.Exception (catch, displayException)
import System.Environment (getArgs)

import TuringMachine (eitherDecode)
import IOParsing (parseArgs, usage)

import qualified Data.ByteString.Lazy as B (readFile, ByteString)

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do
  args <- getArgs
  let result = parseArgs args in
    case result of
      Left invalid -> usage invalid
      Right (description, input) ->
        do
          description_file <- B.readFile description
          putStrLn "Hooray"

handler :: IOError -> IO ()
handler e = putStrLn $ displayException e
