module RecommendationsSpec (main, spec) where

import Test.Hspec

import Recommendations(recommendedMoviesFor)
import SimiliarUsers.PearsonDistance(distance)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "desc" $ do
    it "works" $ do
      recommendedMoviesFor "Toby" distance `shouldBe` Right [("The Night Listener", 3.1192014),
                                                             ("Lady in the Water", 3.0022347),
                                                             ("Just My Luck", 2.5309806)]
