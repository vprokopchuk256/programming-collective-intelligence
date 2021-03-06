module SimiliarUsers.PearsonDistanceSpec (main, spec) where

import Test.Hspec

import SimiliarUsers.PearsonDistance

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "distance" $ do
    it "works" $ do
      distance "Lisa Rose" "Gene Seymour" `shouldBe` Right 0.39605904
    it "returns an error when first user is not defined" $ do
      distance "unknown" "Gene Seymour" `shouldBe` Left "Unknown user unknown"
    it "returns an error when first user is not defined" $ do
      distance "Lisa Rose" "unknown" `shouldBe` Left "Unknown user unknown"
