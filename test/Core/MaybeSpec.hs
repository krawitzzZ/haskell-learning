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
