module Exercises.DaPhoneSpec where

import           Test.Hspec
import           Exercises.DaPhone

phone :: DaPhone
phone = DaPhone { one     = Button ['1', '.', ',']
                , two     = Button ['2', 'a', 'b', 'c']
                , three   = Button ['3', 'd', 'e', 'f']
                , four    = Button ['4', 'g', 'h', 'i']
                , five    = Button ['5', 'j', 'k', 'l']
                , six     = Button ['6', 'm', 'n', 'o']
                , seven   = Button ['7', 'p', 'q', 'r', 's']
                , eight   = Button ['8', 't', 'u', 'v']
                , nine    = Button ['9', 'w', 'x', 'y', 'z']
                , zero    = Button ['0', '+', ' ']
                , hashtag = Button ['#']
                , asterix = Button ['*']
                }

spec :: Spec
spec = parallel $ do
  describe "parseSentence" $ do
    it "correctly converts sentence `147` to series of KeyPresses"
      $          parseSentence phone "147"
      `shouldBe` [('1', 3), ('4', 4), ('7', 5)]
    it "correctly converts sentence `ya` to series of KeyPresses"
      $          parseSentence phone "ya"
      `shouldBe` [('9', 3), ('2', 1)]
    it "correctly converts sentence `#love` to series of KeyPresses"
      $          parseSentence phone "#love"
      `shouldBe` [('#', 1), ('5', 3), ('6', 3), ('8', 3), ('3', 2)]
    it "correctly converts sentence `A` to series of KeyPresses"
      $          parseSentence phone "A"
      `shouldBe` [('*', 1), ('2', 1)]
    it "correctly converts sentence `Wanna play 20` to series of KeyPresses"
      $          parseSentence phone "Wanna play 20"
      `shouldBe` [ ('*', 1)
                 , ('9', 1)
                 , ('2', 1)
                 , ('6', 2)
                 , ('6', 2)
                 , ('2', 1)
                 , ('0', 2)
                 , ('7', 1)
                 , ('5', 3)
                 , ('2', 1)
                 , ('9', 3)
                 , ('0', 2)
                 , ('2', 4)
                 , ('0', 3)
                 ]
    it "correctly converts sentence `U 1st haha` to series of KeyPresses"
      $          parseSentence phone "U 1st haha"
      `shouldBe` [ ('*', 1)
                 , ('8', 2)
                 , ('0', 2)
                 , ('1', 3)
                 , ('7', 4)
                 , ('8', 1)
                 , ('0', 2)
                 , ('4', 2)
                 , ('2', 1)
                 , ('4', 2)
                 , ('2', 1)
                 ]


  describe "fingerTaps" $ do
    it "correctly calculates cost of taps for the `KeyPresses`" $ do
      fingerTaps [('2', 2)] `shouldBe` 2
      fingerTaps [('9', 4)] `shouldBe` 4


  describe "mostPopularLetter" $ do
    it "correctly calculates most commonly used letter" $ do
      mostPopularLetter "Lol ok. Have u ever tasted alcohol lol" `shouldBe` 'l'
      mostPopularLetter "Wow ur cool haha. Ur turn" `shouldBe` 'r'


  describe "mostPopularLetterCost" $ do
    it "correctly calculates cost of taps for the letter" $ do
      mostPopularLetterCost 'a' `shouldBe` 1
      mostPopularLetterCost 'c' `shouldBe` 3
      mostPopularLetterCost 'Z' `shouldBe` 5
