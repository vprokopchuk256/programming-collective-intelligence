module Data
    ( PersonName
    , MovieTitle
    , Item(..)
    , allItems
    , tryMatchRates
    , groupBy
    , sortBy
    , reverseBy
    , try
    , tryFindByName
    , exceptNames
    , exceptTitles
    ) where

import qualified Data.Ord            as Ord(comparing)
import qualified Data.List           as List(find, groupBy, sortBy, reverse)
import qualified Data.Maybe          as Maybe(isJust, fromJust)

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

allItems = map (\(name, title, rate) -> Item name title rate)
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

try :: String -> [a]
             -> Either String [a]
try error [] = Left error
try _ xs = Right xs

matchBy :: (Eq a) => (Item -> a) -> [Item] -> [Item]
        -> [(Item, Item)]
matchBy f is1 is2 = map extractValue $ filter isMatched $ map match is1
  where
    match item = (item, List.find (\other -> (f item) == (f other)) is2)
    isMatched (item, m) = Maybe.isJust m
    extractValue (item, m) = (item, Maybe.fromJust m)

sortBy :: (Ord a) => (b -> a) -> [b] -> [b]
sortBy f items = List.sortBy (Ord.comparing f) items

reverseBy :: (Ord a) => (b -> a) -> [b] -> [b]
reverseBy f = (List.reverse . (sortBy f))

groupBy :: (Eq a, Ord a) => (Item -> a) -> [Item]
        -> [[Item]]
groupBy f items = List.groupBy (\i1 i2 -> f i1 == f i2) $ sortBy f items

exceptTitles :: [MovieTitle] -> [Item] -> [Item]
exceptTitles ts = filter (\i -> notElem (title i) ts)

exceptNames :: [PersonName] -> [Item] -> [Item]
exceptNames pns = filter (\i -> notElem (name i) pns)

pair :: (Item -> a) -> (Item, Item)
     -> (a, a)
pair f (i1, i2) = (f i1, f i2)

-- ----------------------------------------------------------------------------------
-- Public Part
-- ----------------------------------------------------------------------------------

-- | finds data by person name
tryFindByName :: PersonName -> [Item]
              -> Either String [Item]
tryFindByName pn items = try error $ filter (\i -> name i == pn) items
  where
    error = "Unknown user " ++ pn

-- | mathces two specified items list by the specified item attribute
tryMatchByTitle :: [Item] -> [Item]
                -> Either String [(Item, Item)]
tryMatchByTitle is1 is2 = try error $ matchBy title is1 is2
  where
    error = "Users do not have common interests"

-- | return list of rate pairs for each movie that were seen by both persons
tryMatchRates :: PersonName -> PersonName
              -> Either String [(Float, Float)]
tryMatchRates pn1 pn2 = do
    p1 <- tryFindByName pn1 allItems
    p2 <- tryFindByName pn2 allItems

    m <- tryMatchByTitle p1 p2

    return $ map (pair rate) m
