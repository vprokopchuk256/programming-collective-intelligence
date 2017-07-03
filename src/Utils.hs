module Utils where

import      Data.Ord        as Ord(comparing)
import      Data.List       as List(find, groupBy, sortBy, reverse)
import      Data.Maybe      as Maybe(isJust, fromJust)

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
    match item = (item, find (\other -> (f item) == (f other)) is2)
    isMatched (item, m) = isJust m
    extractValue (item, m) = (item, fromJust m)

-- | Sorts list by the specified function
sortBy :: (Ord a) => (b -> a) -> [b] -> [b]
sortBy f items = List.sortBy (comparing f) items

-- | sorts collection by the specified function in reverse order
reverseBy :: (Ord a) => (b -> a) -> [b] -> [b]
reverseBy f = (reverse . (Utils.sortBy f))

-- | returns specified list except items with the property from other list
except :: (Eq b) => (a -> b) -> [b] -> [a]
       -> [a]
except f xs = filter (\i -> notElem (f i) xs)

-- | groups specified list the the specified property
groupBy :: (Eq a, Ord a) => (b -> a) -> [b]
        -> [[b]]
groupBy f items = List.groupBy (\i1 i2 -> f i1 == f i2) $ Utils.sortBy f items

