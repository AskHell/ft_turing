import Turing (Machine(..))

import qualified Data.ByteString.Lazy as B

import Data.Aeson (encode)

main = putStrLn $ show $ encode
  Machine {
    name="Test",
    alphabet=["a"],
    blank=".",
    states=["state1", "state2"],
    initial="state1",
    finals=["state2"]
  }
