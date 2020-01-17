module Polynomial
    ( multP
    , addP
    , degree
    , Polynomial
    )
where

import           Data.List                     as List

type Polynomial = [Int]

addP :: Polynomial -> Polynomial -> Polynomial
addP poly1     []        = poly1
addP []        poly2     = poly2
addP (h1 : t1) (h2 : t2) = (h1 + h2) : addP t1 t2

enumerateAux :: [Int] -> [(Int, Int)] -> Int -> [(Int, Int)]
enumerateAux []      acc _ = acc
enumerateAux (h : t) acc i = enumerateAux t (acc ++ [(h, i)]) (i + 1)

enumerate :: [Int] -> [(Int, Int)]
enumerate array = enumerateAux array [] 0

pad :: Int -> [Int]
pad i | i <= 0 = []
pad i          = 0 : pad (i - 1)

multSingleAux :: [Int] -> Int -> [Int]
multSingleAux []      _   = []
multSingleAux (h : t) val = (h * val) : multSingleAux t val

multSingle :: (Int, Int) -> [Int] -> [Int]
multSingle (value, n) array = padded ++ multSingleAux array value
    where padded = pad n

multP :: Polynomial -> Polynomial -> Polynomial
multP poly1 []    = poly1
multP []    poly2 = poly2
multP poly1 poly2 = standard  where
    standard =
        foldl (\acc enu -> addP acc (multSingle enu poly2)) [] enumeratedPoly1
    enumeratedPoly1 = enumerate poly1

degree :: Polynomial -> Int
degree poly = length poly - 1
