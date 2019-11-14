{-# LANGUAGE
    DeriveGeneric
#-}

module Turing
(
  Machine (..)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

type Letter = String
type State = String

data Machine =
  Machine {
    name :: String,
    alphabet :: [Letter],
    blank :: Letter,
    states :: [State],
    initial :: State,
    finals :: [State]
  } deriving (Generic)

instance FromJSON Machine
instance ToJSON Machine
