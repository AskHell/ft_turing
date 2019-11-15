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

valid :: Machine -> Either String Machine
valid m
  | not $ all (\l -> length l == 1) $ alphabet m =
    Left "Error: Every character from the alphabet must be a string of size 1"
  | notElem (blank m) (alphabet m) =
    Left "Error: The blank character must be part of the alphabet"
  | notElem (initial m) (states m) =
    Left "Error: The initial state must be part of the states"
  | not $ all (\x -> elem x $ states m) $ finals m =
    Left "Error: All the final states must be part of the states"
  -- TODO Test if all to_state from every transition is in Machine states
  -- TODO Test if all write from every transition is in Machine alphabet
valid m = return m

encode :: Machine -> ByteString
encode = Aeson.encode

eitherDecode :: ByteString -> Either String Machine
eitherDecode bs = Aeson.eitherDecode bs >>= valid
