module CaesarCipherSpec where

import           Test.Hspec
import           CaesarCipher

spec :: Spec
spec = parallel $ do
  describe "caesar:" $ do
    it "correctly encodes `a` for 1 symbol to the left"
      $          caesar L 1 "a"
      `shouldBe` "z"
    it "correctly encodes `a` for 1 symbol to the right"
      $          caesar R 1 "a"
      `shouldBe` "b"
    it "correctly encodes `a` for 26 symbol to the left"
      $          caesar L 26 "a"
      `shouldBe` "a"
    it "correctly encodes `a` for 26 symbol to the right"
      $          caesar R 26 "a"
      `shouldBe` "a"
    it "correctly encodes `a` for 52 symbol to the left"
      $          caesar L 52 "a"
      `shouldBe` "a"
    it "correctly encodes `a` for 52 symbol to the right"
      $          caesar R 52 "a"
      `shouldBe` "a"
    it "correctly encodes special symbols" $ do
      caesar R 1 " " `shouldBe` "}"
      caesar R 1 "." `shouldBe` "."
      caesar R 1 "}" `shouldBe` " "
      caesar R 1 "," `shouldBe` "{"
      caesar R 1 "{" `shouldBe` ","
      caesar R 1 "!" `shouldBe` "/"
      caesar R 1 "/" `shouldBe` "!"
      caesar R 1 "?" `shouldBe` "^"
      caesar R 1 "^" `shouldBe` "?"
      caesar R 1 "'" `shouldBe` "~"
      caesar R 1 "~" `shouldBe` "'"
      caesar R 1 "(" `shouldBe` "*"
      caesar R 1 "*" `shouldBe` "("
      caesar R 1 ")" `shouldBe` "%"
      caesar R 1 "%" `shouldBe` ")"
      caesar R 1 ":" `shouldBe` "_"
      caesar R 1 "_" `shouldBe` ":"
      caesar R 1 ";" `shouldBe` "="
      caesar R 1 "=" `shouldBe` ";"
    it "correctly encodes phrase for 857281 symbols to the right"
      $ caesar
          R
          857281
          "Hey guys! What's up? How are you doing today? Let's get some fun!! :)"
      `shouldBe` "Qnh}pdhb/}Fqjc~b}dy^}Qxf}jan}hxd}mxrwp}cxmjh^}Unc~b}pnc}bxvn}odw//}_%"


  describe "uncaesar:" $ do
    it "correctly decodes `a` for 1 symbol to the left"
      $          uncaesar L 1 "a"
      `shouldBe` "b"
    it "correctly decodes `a` for 1 symbol to the right"
      $          uncaesar R 1 "a"
      `shouldBe` "z"
    it "correctly decodes `a` for 26 symbol to the left"
      $          uncaesar L 26 "a"
      `shouldBe` "a"
    it "correctly decodes `a` for 26 symbol to the right"
      $          uncaesar R 26 "a"
      `shouldBe` "a"
    it "correctly decodes `a` for 52 symbol to the left"
      $          uncaesar L 52 "a"
      `shouldBe` "a"
    it "correctly decodes `a` for 52 symbol to the right"
      $          uncaesar R 52 "a"
      `shouldBe` "a"
    it "correctly decodes special symbols" $ do
      uncaesar R 1 " " `shouldBe` "}"
      uncaesar R 1 "." `shouldBe` "."
      uncaesar R 1 "}" `shouldBe` " "
      uncaesar R 1 "," `shouldBe` "{"
      uncaesar R 1 "{" `shouldBe` ","
      uncaesar R 1 "!" `shouldBe` "/"
      uncaesar R 1 "/" `shouldBe` "!"
      uncaesar R 1 "?" `shouldBe` "^"
      uncaesar R 1 "^" `shouldBe` "?"
      uncaesar R 1 "'" `shouldBe` "~"
      uncaesar R 1 "~" `shouldBe` "'"
      uncaesar R 1 "(" `shouldBe` "*"
      uncaesar R 1 "*" `shouldBe` "("
      uncaesar R 1 ")" `shouldBe` "%"
      uncaesar R 1 "%" `shouldBe` ")"
      uncaesar R 1 ":" `shouldBe` "_"
      uncaesar R 1 "_" `shouldBe` ":"
      uncaesar R 1 ";" `shouldBe` "="
      uncaesar R 1 "=" `shouldBe` ";"
    it "correctly decodes phrase for 857281 symbols to the right"
      $ uncaesar
          R
          857281
          "Qnh}pdhb/}Fqjc~b}dy^}Qxf}jan}hxd}mxrwp}cxmjh^}Unc~b}pnc}bxvn}odw//}_%"
      `shouldBe` "Hey guys! What's up? How are you doing today? Let's get some fun!! :)"
