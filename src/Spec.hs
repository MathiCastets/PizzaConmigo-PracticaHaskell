module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "La grande de muzza debe tener un nivel de satisfaccoin de 240" $ do
     nivelDeSatisfaccion  grandeDeMuzza`shouldBe` 240

