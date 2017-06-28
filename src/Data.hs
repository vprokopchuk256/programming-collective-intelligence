module Data where

import Data.HashMap.Strict as Map

type PersonName = String
type MovieName = String

prefs :: HashMap PersonName (HashMap MovieName Float)
prefs = fromList [
    ("Lisa Rose", fromList [
        ("Lady in the Water", 2.5),
        ("Snakes on a Plane", 3.5),
        ("Just My Luck", 3.0),
        ("Superman Returns", 3.5),
        ("You, Me and Dupree", 2.5)
    ]),
    ("Gene Seymour", fromList [
        ("Lady in the Water", 3.0),
        ("Snakes on a Plane", 3.5),
        ("Just My Luck", 1.5),
        ("Superman Returns", 5.0),
        ("The Night Listener", 3.0),
        ("You, Me and Dupree", 3.5)
    ]),
    ("Michael Phillips", fromList [
        ("Lady in the Water", 2.5),
        ("Snakes on a Plane", 3.0),
        ("Superman Returns", 3.5),
        ("The Night Listener", 4.0)
    ]),
    ("Claudia Puig", fromList [
        ("Snakes on a Plane", 3.5),
        ("Just My Luck", 3.0),
        ("The Night Listener", 4.5),
        ("Superman Returns", 4.0),
        ("You, Me and Dupree", 2.5)
    ]),
    ("Mick LaSalle", fromList [
        ("Lady in the Water", 3.0),
        ("Snakes on a Plane", 4.0),
        ("Just My Luck", 2.0),
        ("Superman Returns", 3.0),
        ("The Night Listener", 3.0),
        ("You, Me and Dupree", 2.0)
    ]),
    ("Jack Matthews", fromList [
        ("Lady in the Water", 3.0),
        ("Snakes on a Plane", 4.0),
        ("The Night Listener", 3.0),
        ("Superman Returns", 5.0),
        ("You, Me and Dupree", 3.5)
    ]),
    ("Toby", fromList [
        ("Snakes on a Plane", 4.5),
        ("You, Me and Dupree", 1.0),
        ("Superman Returns", 4.0)
    ])
  ]

