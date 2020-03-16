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


  describe "capitalizeWord" $ do
    it "returns `Hello` for `hello`" $ capitalizeWord "hello" `shouldBe` "Hello"
    it "returns `Yo` for `yo`" $ capitalizeWord "yo" `shouldBe` "Yo"
    it "returns `A` for `a`" $ capitalizeWord "a" `shouldBe` "A"
    it "returns empty string for empty string" $ capitalizeWord "" `shouldBe` ""


  describe "capitalizeParagraph" $ do
    it "returns `Blah. Woot ha.` for `blah. woot ha.`"
      $          capitalizeParagraph "blah. woot ha."
      `shouldBe` "Blah. Woot ha."
    it "returns `Blah. Ha. Ho ho ho. Sigh.` for `blah. ha. ho ho ho. sigh.`"
      $          capitalizeParagraph "blah. ha. ho ho ho. sigh."
      `shouldBe` "Blah. Ha. Ho ho ho. Sigh."
    it "returns `Blah.    Ha.` for `blah.    ha.`"
      $          capitalizeParagraph "blah.    ha."
      `shouldBe` "Blah.    Ha."
