module Exercises.HuttonsRazorSpec where

import           Test.Hspec

import           Exercises.HuttonsRazor

spec :: Spec
spec = parallel $ do
  describe "eval" $ do
    it "should evaluate `Lit 9001` to `9001`" $ do
      eval (Lit 9001) `shouldBe` 9001
    it "should evaluate `Add (Lit 1) (Lit 9001)` to `9002`" $ do
      eval (Add (Lit 1) (Lit 9001)) `shouldBe` 9002
    it "should evaluate `Add (Lit 50) (Add (Lit 20) (Lit 15))` to `85`" $ do
      eval (Add (Lit 50) (Add (Lit 20) (Lit 15))) `shouldBe` 85


  describe "printExpr" $ do
    it "should print `9001` for `Lit 9001`" $ do
      printExpr (Lit 9001) `shouldBe` "9001"
    it "should print `1 + 9001` for `(Add (Lit 1) (Lit 9001))`" $ do
      printExpr (Add (Lit 1) (Lit 9001)) `shouldBe` "1 + 9001"
    it "should print correct result for deep expression" $ do
      let a1 = Add (Lit 9001) (Lit 1)
      let a2 = Add a1 (Lit 20001)
      let a3 = Add (Lit 1) a2
      printExpr a3 `shouldBe` "1 + 9001 + 1 + 20001"
