module SimiliarUsers.PearsonDistance
    ( distance
    ) where

import Data.HashMap.Strict  (elems, intersectionWith)

import Data                 (PersonName, prefs)

distance :: PersonName -> PersonName
         -> Either String Float
distance pn1 pn2 = do
    p1 <- prefs pn1
    p2 <- prefs pn2

    let si = intersectionWithPair p1 p2

    let sum1 = sum $ map fst si
    let sum2 = sum $ map snd si

    let sum1Sq = sum $ map ((^2) . fst) si
    let sum2Sq = sum $ map ((^2) . snd) si

    let pSum = sum $ map (uncurry (*)) si

    let n = fromIntegral $ length si
    let num = pSum - ((sum1 * sum2) / n)
    let den = sqrt ((sum1Sq - ((sum1^2)/n)) *(sum2Sq - ((sum2^2)/n)))

    return $ num / den
  where
    intersectionWithPair m1 m2 = elems $ intersectionWith ((,)) m1 m2
