module Utils where

import qualified Data.Ord            as Ord(comparing)
import qualified Data.List           as List(find, sortBy)
import qualified Data.Maybe          as Maybe(isJust, fromJust)

-- | returns list if not empty or error otherwise
tryList :: String -> [a]
        -> Either String [a]
tryList error [] = Left error
tryList _ xs = Right xs

-- | Matches elements from one collection to the element to other one
--   with the specified matching function
matchBy :: (Eq b) => (a -> b) -> [a] -> [a]
        -> [(a, a)]
matchBy f is1 is2 = map extractValue $ filter isMatched $ map match is1
  where
    match item = (item, List.find (\other -> (f item) == (f other)) is2)
    isMatched (item, m) = Maybe.isJust m
    extractValue (item, m) = (item, Maybe.fromJust m)

-- | Sorts list by the specified function
sortBy :: (Ord a) => (b -> a) -> [b] -> [b]
sortBy f items = List.sortBy (Ord.comparing f) items

