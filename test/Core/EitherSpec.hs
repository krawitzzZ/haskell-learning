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
