module Core.EitherSpec where

import           Test.Hspec
import           Core.Either
import           Core.Basics                    ( Person(..) )

-- TODO: write more tests

spec :: Spec
spec = parallel $ do
  describe "mkPerson" $ do
    it "should create Person" $ mkPerson "Joe" 30 `shouldBe` Right
      (Person "Joe" 30)
    it "should return `[EmptyName]` if name is empty"
      $          mkPerson "" 30
      `shouldBe` Left [EmptyName]
    it "should create Person with age = 0" $ mkPerson "Joe" 0 `shouldBe` Right
      (Person "Joe" 0)
    it "should return `[LowAge] if age is < 0"
      $          mkPerson "Joe" (-1)
      `shouldBe` Left [LowAge]
    it
        "should return `[EmptyName, LowAge] if name is empty string and age is < 0"
      $          mkPerson "" (-1)
      `shouldBe` Left [EmptyName, LowAge]
