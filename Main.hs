import Control.Exception (catch, displayException)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B (readFile, ByteString)

import ArgsParsing (parseArgs, usage)
import TuringMachine (eitherDecode, parseTape, ft_turing)

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do
  args <- getArgs
  let result = parseArgs args in
    case result of
      Left invalid -> usage invalid
      Right (description, input) -> do
          description_file <- B.readFile description
          case ft_turing description_file input of
            Right s -> putStrLn s
            Left err -> putStrLn err

handler :: IOError -> IO ()
handler e = putStrLn $ displayException e
