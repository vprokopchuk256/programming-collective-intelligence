module SimiliarUsers.EuclideanDistance(distance) where

import qualified Data.HashMap.Strict as Map
import qualified Data as Data

distance :: Data.PersonName -> Data.PersonName -> Maybe Float
distance pn1 pn2 = do
    -- ** get users data
    p1Pref <- Map.lookup pn1 Data.prefs
    p2Pref <- Map.lookup pn2 Data.prefs

    -- calculate map with square diffs as value
    let squareDiffs = Map.elems $ Map.intersectionWith squareDiff p1Pref p2Pref

    return $ 1 / (1 + (sum squareDiffs))
  where
    squareDiff v1 v2 = (v1 - v2) ^ 2


