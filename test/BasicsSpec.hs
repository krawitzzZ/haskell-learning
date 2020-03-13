module BasicsSpec where

import           Test.Hspec

spec :: Spec
spec = describe "first test case" $ it "passes first test" $ "a" `shouldBe` "b"
