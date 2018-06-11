module InputFileSpec where

import Input
import Life
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Input suite test" $ do
    it "should create an alive element" $ do
      loadChar '1' 0 0 `shouldBe` life 0 0
      loadChar '1' 1 (-1) `shouldBe` life 1 (-1)
      loadChar '1' 2 3 `shouldBe` life 2 3

    it "should create a dead element" $ do
      loadChar '0' 0 0 `shouldBe` dead 0 0
      loadChar '0' 1 (-1) `shouldBe` dead 1 (-1)
      loadChar '0' 2 3 `shouldBe` dead 2 3

    it "should create a list of element" $ do
      loadString "0101" 0 0 `shouldBe` dead 0 0 : life 0 1 : dead 0 2 : life 0 3 : []
      loadString "1010" 38 0 `shouldBe` life 38 0 : dead 38 1 : life 38 2 : dead 38 3 : []
      loadString "00011" 2 4 `shouldBe` dead 2 4 : dead 2 5 : dead 2 6 : life 2 7 : life 2 8 : []

    it "should create a whole from string" $ do
      loadText "010\n101\n010" 0 0 `shouldBe` dead 0 0 : life 0 1 : dead 0 2 :
                                              life 1 0 : dead 1 1 : life 1 2 :
                                              dead 2 0 : life 2 1 : dead 2 2 : []