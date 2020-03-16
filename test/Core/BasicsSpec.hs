module Core.BasicsSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Core.Basics

spec :: Spec
spec = parallel $ do
  describe "WeekDay:" $ do
    it "`Mon` and `Mon` should be equal" $ Mon `shouldBe` Mon
    it "`Mon` and `Tue` should not be equal" $ Mon `shouldNotBe` Tue


  describe "Date:" $ do
    it "`Mon 5` and `Mon 5` should be equal" $ Date Mon 5 `shouldBe` Date Mon 5
    it "`Mon 5` and `Tue 5` should not be equal" $ Date Mon 5 `shouldNotBe` Date
      Tue
      5


  describe "sum':" $ do
    it "of 1 should be 1" $ sum' 1 `shouldBe` 1
    it "of 2 should be 3" $ sum' 2 `shouldBe` 3
    it "of 8673 should be 381501" $ sum' 873 `shouldBe` 381501
    it "correctly sums numbers" $ property $ \x ->
      sum' x == sum (if x > 0 then [0 .. x] else [x .. 0])


  describe "product':" $ do
    it "of 12 and 0 should be 0" $ product' 12 0 `shouldBe` 0
    it "of 1 and 1 should be 1" $ product' 1 1 `shouldBe` 1
    it "of 2 and (-1) should be -2" $ product' 2 (-1) `shouldBe` -2
    it "of (-1) and 2 should be -2" $ product' (-1) 2 `shouldBe` -2
    it "of 2 and 1 should be 2" $ product' 2 1 `shouldBe` 2
    it "of 2 and 2 should be 4" $ product' 2 2 `shouldBe` 4
    it "of 8 and 12 should be 96" $ product' 8 12 `shouldBe` 96
    it "of (-4) and 6 should be -24" $ product' (-4) 6 `shouldBe` -24
    it "of 8 and (-12) should be -96" $ product' 8 (-12) `shouldBe` -96
    it "correctly multiplies Int values" $ property $ \x y ->
      product' x y == x * y


  describe "devision':" $ do
    it "of 12 and 0 should be `DividedByZero`"
      $          division 12 0
      `shouldBe` DividedByZero
    it "of 0 and 14 should be (0, 0)" $ division 0 14 `shouldBe` DividedResult
      (0, 0)
    it "of 12 and 1 should be (12, 0)" $ division 12 1 `shouldBe` DividedResult
      (12, 0)
    it "of 11 and (-1) should be (-11, 0)"
      $          division 11 (-1)
      `shouldBe` DividedResult (-11, 0)
    it "of (-15) and (-1) should be (15, 0)"
      $          division (-15) (-1)
      `shouldBe` DividedResult (15, 0)
    it "of (-15) and 1 should be (-15, 0)"
      $          division (-15) 1
      `shouldBe` DividedResult (-15, 0)
    it "of 15 and 4 should be (3, 3)" $ division 15 4 `shouldBe` DividedResult
      (3, 3)
    it "of 13 and (-3) should be (-4, 1)"
      $          division 13 (-3)
      `shouldBe` DividedResult (-4, 1)
