module Core.MaybeSpec where

import           Test.Hspec
import           Core.Maybe
import           Core.Basics                    ( Person(..) )

-- TODO: write more tests

spec :: Spec
spec = parallel $ do
  describe "mkPerson" $ do
    it "should create Person" $ mkPerson "Joe" 30 `shouldBe` Just
      (Person "Joe" 30)
    it "should return `Nothing` if age is < 0"
      $          mkPerson "Joe" (-1)
      `shouldBe` Nothing
    it "should return `Nothing` if name is empty string"
      $          mkPerson "" 10
      `shouldBe` Nothing
