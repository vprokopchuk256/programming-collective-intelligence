module Recommendations(recommendedMoviesFor) where

import qualified Data.Ord                as Ord(comparing)
import qualified Data.HashMap.Strict     as Map(HashMap, fromList, (!))
import qualified Data.List               as List(nub)

import           Data                    as Data

recommendedMoviesFor :: PersonName -> (PersonName -> PersonName -> Either String Float)
                     -> Either String [(String, Float)]
recommendedMoviesFor pn simf = do
    titles <- map title <$> tryFindByName pn allItems

    otherItems <- try "Has not other users" $
                  ((exceptTitles titles) . (exceptNames [pn])) allItems

    let groups = groupBy title otherItems

    rates <- mapM (calculate . summarize . titleize) groups

    return $ reverseBy snd rates
  where
    titleize items = (title $ head items, items)
    summarizeItem item = do
      sim <- simf pn $ name item
      return (sim, rate item * sim)
    summarize (title, items) = ((,) title) <$> mapM summarizeItem items
    calculate (Right (title, summaries)) = Right (title, total / simSum)
      where
        simSum = (sum . (map fst)) summaries
        total = (sum . (map snd)) summaries
