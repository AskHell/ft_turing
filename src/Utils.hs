module Utils (
    enumerate,
    unwrap'
) where

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

unwrap' :: a -> Maybe a -> a
unwrap' _ (Just a) = a
unwrap' d Nothing = d
