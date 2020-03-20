module Core.MaybeSpec where

import           Test.Hspec
import           Core.Maybe
import           Core.Basics                    ( Person(..) )

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


  describe "notThe" $ do
    it "should return `Just blahthe`" $ notThe "blahthe" `shouldBe` Just
      "blahthe"
    it "should return `Just woot`" $ notThe "woot" `shouldBe` Just "woot"
    it "should return `Nothing`" $ notThe "the" `shouldBe` Nothing


  describe "replaceThe" $ do
    it "should return `a cow loves you`" $ do
      replaceThe "the cow loves you" `shouldBe` "a cow loves you"
    it "should return `what is a question?`" $ do
      replaceThe "what is the question?" `shouldBe` "what is a question?"
    it "should return the same string" $ do
      replaceThe "how are you doing?" `shouldBe` "how are you doing?"
