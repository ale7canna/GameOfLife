module OutputSpec where

import Test.Hspec
import Output
import Life
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "output should print in an human way the world" $ do
    it "should print humanly the world" $ do
      show' (life 0 0 : dead 0 1 : dead 1 0 : life 1 1 : []) `shouldBe` "10\n01\n"
      show' world'    `shouldBe` "01\n11\n"
      show' world''   `shouldBe` "0100\n1101\n1010\n1100\n"

    it "should get rows number" $ do
      getRows (life 1 1 : life 1 0 : life 0 1 : life 2 0 : []) `shouldBe` [0, 1, 2]
      getRows (dead (-1) 1 : life 1 0 : life 3 1 : life 2 0 : []) `shouldBe` [-1, 1, 2, 3]

    it "should print row" $ do
      showRow (life 0 1 : dead 0 0 : dead 0 2 : life 0 3 : []) `shouldBe` "0101"
      showRow (dead 2 5 : life 2 4 : life 2 1 : dead 2 0 : dead 2 2 : life 2 3 : []) `shouldBe` "010110"

  describe "utility function test" $ do
    it "should remove duplicate inside list" $ do
      sort (remDup [1,2,3,4,4,3,2,1])   `shouldBe` [1,2,3,4]
      sort (remDup [1,2,2,3,3,4,4,2,1]) `shouldBe` [1,2,3,4]

    it "should return life row" $ do
      getRow (life 1 1) `shouldBe` 1
      getRow (life 0 1) `shouldBe` 0
      getRow (life 3 1) `shouldBe` 3

    it "should return column row" $ do
      getColumn (life 0 1) `shouldBe` 1
      getColumn (life 0 0) `shouldBe` 0
      getColumn (life 0 3) `shouldBe` 3

    it "should split world" $ do
      splitRows (life 1 1 : life 1 0 : life 0 1 : life 2 0 : []) `shouldBe` [[life 0 1], (life 1 0 : life 1 1 : []), [life 2 0]]
      splitRows (life 3 1 : life 1 0 : life 0 1 : life 2 0 : life 1 2 : life 0 3 : [])
        `shouldBe` [
          life 0 1 : life 0 3 : [],
          life 1 0 : life 1 2 : [],
          life 2 0 : [],
          [life 3 1]
          ]

world' :: [Life]
world'    = dead 0 0 : life 0 1 :
            life 1 0 : life 1 1 : []

world'' :: [Life]
world''   = dead 0 0 : life 0 1 : dead 0 2 : dead 0 3 :
            life 1 0 : life 1 1 : dead 1 2 : life 1 3 :
            life 2 0 : dead 2 1 : life 2 2 : dead 2 3 :
            life 3 0 : life 3 1 : dead 3 2 : dead 3 3 : []