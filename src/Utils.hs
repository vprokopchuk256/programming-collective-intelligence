module Utils where

tryList :: String -> [a]
             -> Either String [a]
tryList error [] = Left error
tryList _ xs = Right xs
