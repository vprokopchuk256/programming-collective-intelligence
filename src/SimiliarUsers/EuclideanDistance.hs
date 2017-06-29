module SimiliarUsers.EuclideanDistance
    ( distance
    ) where

import Data.HashMap.Strict  (elems, intersectionWith)

import Data                 (PersonName, prefs)

distance :: PersonName -> PersonName
         -> Either String Float
distance pn1 pn2 = do
    p1 <- prefs pn1
    p2 <- prefs pn2

    return $ balanced . sum $ squareDiffIntersection p1 p2
  where
    squareDiff v1 v2 = (v1 - v2) ^ 2
    squareDiffIntersection m1 m2 = elems $ intersectionWith squareDiff m1 m2
    balanced v = 1 / (1 + v)
