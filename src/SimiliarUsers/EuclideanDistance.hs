module SimiliarUsers.EuclideanDistance(distance) where

import Data                 (PersonName, ratePairs)

distance :: PersonName -> PersonName
         -> Either String Float
distance pn1 pn2 = do
    si <- ratePairs pn1 pn2

    return $ (1/) . (1+) . sum $ map ((^2) . (uncurry (-))) si
