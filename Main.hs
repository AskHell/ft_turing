import TuringMachine (eitherDecode)
import Control.Exception (catch, displayException)
import System.IO
import System.IO.Error
import System.Environment
import qualified Data.ByteString.Lazy as B (readFile, ByteString)

import Debug.Trace (trace)

-- import qualified Data.Map.Strict as Map
-- main = putStrLn $ show $ encode
  -- Turing.Machine {
    -- name="Test",
    -- alphabet=["a"],
    -- blank=".",
    -- states=["state1", "state2"],
    -- initial="state1",
    -- finals=["state2"],
    -- transitions=Map.fromList [("t1", Turing.Transition {
      -- Turing.read="state1",
      -- to_state="state2",
      -- write="a",
      -- action=Turing.Left
    -- })]
  -- }

-- getOutput :: Either String Machine -> String
-- getOutput (Prelude.Left s) = s
-- getOutput (Prelude.Right m) = show $ encode m

usage :: IO()
usage = putStrLn $ "Usage: ./ft_turing <description_file> <input>"

parseArgs :: [String] -> IO (Either String (B.ByteString, String))
parseArgs (filename:input:[]) = do
  description <- B.readFile filename
  return $ Right (description, input)
parseArgs _ = return $ Left "Missing args"

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do
  args <- getArgs
  action <- parseArgs args
  case action of
    Left error -> usage
    Right (description, input) -> putStrLn "Hooray"

  -- let result = ft_turing description input
  -- final result

handler :: IOError -> IO ()
handler e = putStrLn $ displayException e
