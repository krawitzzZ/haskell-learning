module Exercises.ExSpec where

import           Test.Hspec
import           Exercises.Ex

spec :: Spec
spec = parallel $ do
  describe "isSubsequenceOf" $ do
    it "returns `True` for `blah` and `blah`"
      $          isSubsequenceOf "blah" "blah"
      `shouldBe` True
    it "returns `True` for `blah` and `blahwoot`"
      $          isSubsequenceOf "blah" "blahwoot"
      `shouldBe` True
    it "returns `True` for `blah` and `wootblah`"
      $          isSubsequenceOf "blah" "wootblah"
      `shouldBe` True
    it "returns `True` for `blah` and `wboloath`"
      $          isSubsequenceOf "blah" "wboloath"
      `shouldBe` True
    it "returns `True` for `blah` and `hwboloat`"
      $          isSubsequenceOf "blah" "hwboloat"
      `shouldBe` True
    it "returns `False` for `blah` and `wootbla`"
      $          isSubsequenceOf "blah" "wootbla"
      `shouldBe` False
  describe "capitalizeWords" $ do
    it "returns `[('hello', 'Hello'), ('world', 'World')]` for `hello world`"
      $          capitalizeWords "hello world"
      `shouldBe` [("hello", "Hello"), ("world", "World")]
    it "returns list of 5 items for `hello my name is joe`"
      $          capitalizeWords "hello my name is joe"
      `shouldBe` [ ("hello", "Hello")
                 , ("my"   , "My")
                 , ("name" , "Name")
                 , ("is"   , "Is")
                 , ("joe"  , "Joe")
                 ]
    it "returns `[('', '')]` for empty string"
      $          capitalizeWords ""
      `shouldBe` [("", "")]
