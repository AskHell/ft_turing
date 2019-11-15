import TuringMachine

import qualified Data.ByteString.Lazy as B

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

getOutput :: Either String Machine -> String
getOutput (Prelude.Left s) = s
getOutput (Prelude.Right m) = show $ encode m

main = do
  file_content <- B.readFile "test.json"
  putStrLn $ getOutput $ eitherDecode file_content
