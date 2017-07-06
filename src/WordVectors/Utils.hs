{-# LANGUAGE OverloadedStrings #-}

module WordVectors.Utils where

import                Data.ByteString                           (ByteString)
import                Data.ByteString.Char8                     as BS(pack, length)
import                Data.Maybe                                (isJust, fromJust)
import                Data.List                                 (sort, group)
import                Data.Map                                  (Map, fromList)
import                Text.Regex.PCRE.ByteString.Utils          (splitCompile)
import                Text.Feed.Import                          (parseFeedFromFile)
import                Text.Feed.Query                           (getFeedItems, getItemSummary)

countWords :: String
           -> IO (Either String (Map ByteString Int))
countWords fn = do
    feed <- parseFeedFromFile fn

    res <- mapM split $ map (pack . fromJust) $ filter isJust $ map getItemSummary $ getFeedItems feed

    return $ (fromList . count . titleize . group . non_empty . sort . concat) <$> sequence res
  where
    split = splitCompile "\\W"
    non_empty = filter (\s -> BS.length s > 1)
    titleize = map (\xs -> (head xs, xs))
    count = map (fmap Prelude.length)
