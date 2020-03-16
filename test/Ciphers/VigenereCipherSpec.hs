module Ciphers.VigenereCipherSpec where

import           Test.Hspec
import           Ciphers.VigenereCipher

spec :: Spec
spec = do
  parallel $ do
    describe "vigenere" $ do
      it "should correctly encode phrase `meet at dawn` with key-word `ally`"
        $          vigenere "meet at dawn" "ally"
        `shouldBe` "mppr}le}dlhl"
      it
          "should correctly encode phrase `Hey guys! What's up? How are you doing today? Let's get some fun!! :)` with key-word `top-secret`"
        $ vigenere
            "Hey guys! What's up? How are you doing today? Let's get some fun!! :)"
            "top-secret"
        `shouldBe` "Asn}yyaj/}Pvpt~w}lt^}Vdw}etv}rhi}dgmpx}mhrpy^}Nvx~l}vel}ufqx}tjn//}_%"


    describe "devigenere" $ do
      it "should correctly decode phrase `mppr}le}dlhl` with key-word `ally`"
        $          devigenere "mppr}le}dlhl" "ally"
        `shouldBe` "meet at dawn"
      it
          "should correctly decode phrase `Asn}yyaj/}Pvpt~w}lt^}Vdw}etv}rhi}dgmpx}mhrpy^}Nvx~l}vel}ufqx}tjn//}_%` with key-word `top-secret`"
        $ devigenere
            "Asn}yyaj/}Pvpt~w}lt^}Vdw}etv}rhi}dgmpx}mhrpy^}Nvx~l}vel}ufqx}tjn//}_%"
            "top-secret"
        `shouldBe` "Hey guys! What's up? How are you doing today? Let's get some fun!! :)"
