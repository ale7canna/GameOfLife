module LifeSpec where

import Life
import Test.Hspec
import Cell

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "life handling" $ do
    it "should create life" $ do
      life 10 10 `shouldBe` Alive (Cell 10 10)

    it "should create dead" $ do
      dead 2 2 `shouldBe` Dead (Cell 2 2)

    it "should not have neighbours" $ do
      neighbours (life 10 10) [] `shouldBe` []

    it "should have all neighbours" $ do
      let mates = life 1 1 : life 1 0 : life 0 1 : [] in
        neighbours (life 0 0) mates `shouldBe` mates

    it "should filter neighbours among all lifes" $ do
      let mates = life 0 2 : life 1 0 : life 2 1 : [] in
        neighbours (life 0 0) mates `shouldBe` life 1 0 : []
