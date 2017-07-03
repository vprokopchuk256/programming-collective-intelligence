module Recommendations(recommendedMoviesFor) where

import      Data          as Data(PersonName, title, name, rate, allItems, tryFindByName)
import      Utils         as Utils(tryList, reverseBy, except, groupBy)

recommendedMoviesFor :: PersonName -> (PersonName -> PersonName -> Either String Float)
                     -> Either String [(String, Float)]
recommendedMoviesFor pn simf = do
    titles <- map title <$> tryFindByName pn allItems

    otherItems <- tryList "Has not other users" $
                  ((except title titles) . (except name [pn])) allItems

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
