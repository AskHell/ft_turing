module Generate.Utils (
    Alphabet,
    fromMachineToJson
) where

import Machine (Letter, Machine, encode)

type Alphabet = [Letter]

fromMachineToJson :: Machine -> String
fromMachineToJson machine = filter (\x -> not $ x == '\\') $ show $ encode machine
