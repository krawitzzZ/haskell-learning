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
