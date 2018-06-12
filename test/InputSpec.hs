module InputSpec where

import Input
import Life
import Test.Hspec
import System.IO

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

    it "should create a whole world from string" $ do
      loadText "" 0 0 `shouldBe` []
      loadText "010\n101\n010" 0 0 `shouldBe` dead 0 0 : life 0 1 : dead 0 2 :
                                              life 1 0 : dead 1 1 : life 1 2 :
                                              dead 2 0 : life 2 1 : dead 2 2 : []

      loadText "0100\n1101\n1010\n1100" 0 0 `shouldBe` dead 0 0 : life 0 1 : dead 0 2 : dead 0 3 :
                                                       life 1 0 : life 1 1 : dead 1 2 : life 1 3 :
                                                       life 2 0 : dead 2 1 : life 2 2 : dead 2 3 :
                                                       life 3 0 : life 3 1 : dead 3 2 : dead 3 3 : []

    it "should load world from file" $ do
      writeFile "temp.txt" "0100\n1101\n1010\n1100"
      content <- loadFile "temp.txt"
      loadText content 0 0 `shouldBe` dead 0 0 : life 0 1 : dead 0 2 : dead 0 3 :
                                      life 1 0 : life 1 1 : dead 1 2 : life 1 3 :
                                      life 2 0 : dead 2 1 : life 2 2 : dead 2 3 :
                                      life 3 0 : life 3 1 : dead 3 2 : dead 3 3 : []

    it "should print humanly the world" $ do
      show' world'    `shouldBe` "01\n11\n"

      show' world''   `shouldBe` "0100\n1101\n1010\n1100\n"

        where world'    = dead 0 0 : life 0 1 :
                          life 1 0 : life 1 1 : []
              world''   = dead 0 0 : life 0 1 : dead 0 2 : dead 0 3 :
                          life 1 0 : life 1 1 : dead 1 2 : life 1 3 :
                          life 2 0 : dead 2 1 : life 2 2 : dead 2 3 :
                          life 3 0 : life 3 1 : dead 3 2 : dead 3 3 : []