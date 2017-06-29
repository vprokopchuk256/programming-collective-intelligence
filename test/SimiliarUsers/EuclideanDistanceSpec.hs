module SimiliarUsers.EuclideanDistanceSpec (main, spec) where

import Test.Hspec

import SimiliarUsers.EuclideanDistance

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "distance" $ do
    it "works" $ do
      distance "Lisa Rose" "Gene Seymour" `shouldBe` Just 0.14814815
