module Reduce (
    reduce',
) where

stack_either :: ([a], [b]) -> Either a b -> ([a], [b])
stack_either (l, r) (Left a) =
    (a : l, r)
stack_either (l, r) (Right b) =
    (l, b : r)

reduce' :: [Either a b] -> Either [a] [b]
reduce' le =
    let (l, r) = foldl stack_either ([], []) le in
    case (l, r) of
        ([], r) -> return r
        (l, _) -> Left l
