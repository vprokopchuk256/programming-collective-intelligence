{-# LANGUAGE OverloadedStrings #-}

module Main where

import                WordVectors.Utils                         (countWords)

main :: IO ()
main = do
    counts <- countWords "/projects/programming-collective-intelligence/test/Data/FeedData.xml"

    putStrLn $ show counts
