{-# LANGUAGE
    DeriveGeneric
#-}

module Turing
(
  Machine,
  encode,
  eitherDecode
) where

import GHC.Generics (Generic)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (encode, eitherDecode)

import Data.Map.Strict (Map)

type Letter = String
type State = String

data Action = LEFT | RIGHT deriving (Generic)
instance FromJSON Action
instance ToJSON Action

data Transition =
  Transition {
    read :: State,
    to_state :: State,
    write :: Letter,
    action :: Action
  } deriving (Generic)

instance FromJSON Transition
instance ToJSON Transition

data Machine =
  Machine {
    name :: String,
    alphabet :: [Letter],
    blank :: Letter,
    states :: [State],
    initial :: State,
    finals :: [State],
    transitions :: Map String [Transition]
  } deriving (Generic)

instance FromJSON Machine
instance ToJSON Machine

encode :: Machine -> ByteString
encode = Aeson.encode

eitherDecode :: ByteString -> Either String Machine
eitherDecode = Aeson.eitherDecode
