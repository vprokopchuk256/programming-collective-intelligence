module Data
    ( PersonName
    , ratePairs
    ) where

import Data.List           (find)
import Data.Maybe          (isJust, fromJust)

-- ----------------------------------------------------------------------------------
-- Private Part. Data and environment
-- ----------------------------------------------------------------------------------

type PersonName        = String
type MovieTitle        = String

data Item = Item
    { name :: PersonName
    , title :: MovieTitle
    , rate :: Float
    } deriving(Show)

items = map (\(name, title, rate) -> Item name title rate)
    [ ("Lisa Rose",        "Lady in the Water",  2.5)
    , ("Lisa Rose",        "Snakes on a Plane",  3.5)
    , ("Lisa Rose",        "Just My Luck",       3.0)
    , ("Lisa Rose",        "Superman Returns",   3.5)
    , ("Lisa Rose",        "You, Me and Dupree", 2.5)
    , ("Lisa Rose",        "The Night Listener", 3.0)

    , ("Gene Seymour",     "Lady in the Water",  3.0)
    , ("Gene Seymour",     "Snakes on a Plane",  3.5)
    , ("Gene Seymour",     "Just My Luck",       1.5)
    , ("Gene Seymour",     "Superman Returns",   5.0)
    , ("Gene Seymour",     "The Night Listener", 3.0)
    , ("Gene Seymour",     "You, Me and Dupree", 3.5)

    , ("Michael Phillips", "Lady in the Water",  2.5)
    , ("Michael Phillips", "Snakes on a Plane",  3.0)
    , ("Michael Phillips", "Superman Returns",   3.5)
    , ("Michael Phillips", "The Night Listener", 4.0)

    , ("Claudia Puig",     "Snakes on a Plane",  3.5)
    , ("Claudia Puig",     "Just My Luck",       3.0)
    , ("Claudia Puig",     "The Night Listener", 4.5)
    , ("Claudia Puig",     "Superman Returns",   4.0)
    , ("Claudia Puig",     "You, Me and Dupree", 2.5)

    , ("Mick LaSalle",     "Lady in the Water",  3.0)
    , ("Mick LaSalle",     "Snakes on a Plane",  4.0)
    , ("Mick LaSalle",     "Just My Luck",       2.0)
    , ("Mick LaSalle",     "Superman Returns",   3.0)
    , ("Mick LaSalle",     "The Night Listener", 3.0)
    , ("Mick LaSalle",     "You, Me and Dupree", 2.0)

    , ("Jack Matthews",    "Lady in the Water",  3.0)
    , ("Jack Matthews",    "Snakes on a Plane",  4.0)
    , ("Jack Matthews",    "The Night Listener", 3.0)
    , ("Jack Matthews",    "Superman Returns",   5.0)
    , ("Jack Matthews",    "You, Me and Dupree", 3.5)

    , ("Toby",             "Snakes on a Plane",  4.5)
    , ("Toby",             "You, Me and Dupree", 1.0)
    , ("Toby",             "Superman Returns",   4.0)
    ]

-- ----------------------------------------------------------------------------------
-- Private Part. Service Functions
-- ----------------------------------------------------------------------------------

listToEither :: String -> [a]
             -> Either String [a]
listToEither error [] = Left error
listToEither _ xs = Right xs

findBy :: (Item -> Bool)
       -> [Item]
findBy f = filter f items

findByname :: String -> [Item]
findByname pn = findBy (\i -> name i == pn)

matchBy :: (Eq a) => (Item -> a) -> [Item] -> [Item]
        -> [(Item, Item)]
matchBy f is1 is2 = map extractValue $ filter isMatched $ map match is1
  where
    match item = (item, find (\other -> (f item) == (f other)) is2)
    isMatched (item, m) = isJust m
    extractValue (item, m) = (item, fromJust m)

pairBy :: (Item -> a) -> (Item, Item)
       -> (a, a)
pairBy f (i1, i2) = (f i1, f i2)

-- ----------------------------------------------------------------------------------
-- Public Part
-- ----------------------------------------------------------------------------------

-- | finds data by person name
tryFindByName :: PersonName
              -> Either String [Item]
tryFindByName pn = listToEither error $ findByname pn
  where
    error = "Unknown user " ++ pn

-- | mathces two specified items list by the specified item attribute
matchByTitle :: [Item] -> [Item]
             -> Either String [(Item, Item)]
matchByTitle is1 is2 = listToEither error $ matchBy title is1 is2
  where
    error = "Users do not have common interests"

-- | return list of rate pairs for each movie that were seen by both persons
ratePairs :: PersonName -> PersonName
          -> Either String [(Float, Float)]
ratePairs pn1 pn2 = do
    p1 <- tryFindByName pn1
    p2 <- tryFindByName pn2

    m <- matchByTitle p1 p2

    return $ map (pairBy rate) m
