module LifeSpec where

import Life
import Test.Hspec

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

    it "should be alone if less then 2 living cell around" $ do
      alone (life 0 0) (life 1 1 : life 1 0:[]) `shouldBe` False
      alone (life 0 0) (life 1 1 : dead 1 0:[]) `shouldBe` True
      alone (life 0 0) (life 1 1:[]) `shouldBe` True
      alone (life 0 0) (dead 1 0:[]) `shouldBe` True

    it "should be overpopulated if more then 3 living cell around" $ do
      overPop (life 0 0) (life 1 1 : life 1 0 : life 0 1 : life 2 0       :[]) `shouldBe` False
      overPop (life 0 0) (life 1 1 : life 1 0 : life 0 1 : life (-1) 0    :[]) `shouldBe` True
      overPop (life 0 0) (life 1 1 : dead 1 0 : life 0 1 : life (-1) 0    :[]) `shouldBe` False
      overPop (life 0 0) (life 1 1 : life 1 0 : life 0 1 : life (-1) (-1) :[]) `shouldBe` True

    it "should be alone if less then 2 living cell around" $ do
      evolve (life 0 0) (life 1 1 : life 1 0 : []) `shouldBe` life 0 0
      evolve (life 0 0) (life 1 1 : dead 1 0 : []) `shouldBe` dead 0 0
      evolve (life 0 0) (life 1 1 : []) `shouldBe` dead 0 0
      evolve (life 0 0) (dead 1 0 : []) `shouldBe` dead 0 0

    it "should be overpopulated if more then 3 living cell around" $ do
      evolve (life 0 0) (life 1 1 : life 1 0 : life 0 1 : life 2 0      : []) `shouldBe` life 0 0
      evolve (life 0 0) (life 1 1 : life 1 0 : life 0 1 : life (-1) 0   : []) `shouldBe` dead 0 0
      evolve (life 0 0) (life 1 1 : dead 1 0 : life 0 1 : life (-1) 0   : []) `shouldBe` life 0 0
      evolve (life 0 0) (life 1 1 : life 1 0 : life 0 1 : life (-1) (-1): []) `shouldBe` dead 0 0

    it "should get rows number" $ do
      getRows (life 1 1 : life 1 0 : life 0 1 : life 2 0 : []) `shouldBe` [0, 1, 2]
      getRows (dead (-1) 1 : life 1 0 : life 3 1 : life 2 0 : []) `shouldBe` [-1, 1, 2, 3]

    it "should split world" $ do
      splitRows (life 1 1 : life 1 0 : life 0 1 : life 2 0 : []) `shouldBe` [[life 0 1], (life 1 0 : life 1 1 : []), [life 2 0]]

  describe "Cell methods" $ do
    it "should return true if two cell are close" $ do
      (Cell 0 0) `isNear` (Cell (-1) 0) `shouldBe` True
      (Cell 0 0) `isNear` (Cell 1 1) `shouldBe` True
      (Cell 0 0) `isNear` (Cell 0 1) `shouldBe` True
      (Cell 0 0) `isNear` (Cell 0 (-1)) `shouldBe` True
      (Cell 0 0) `isNear` (Cell (-1) (-1)) `shouldBe` True

    it "should return false if two cells are not close" $ do
      (Cell 0 0) `isNear` (Cell 10 (-12)) `shouldBe` False
      (Cell 0 0) `isNear` (Cell (-2) (-2)) `shouldBe` False
      (Cell 0 0) `isNear` (Cell (2) (-1)) `shouldBe` False
      (Cell 0 0) `isNear` (Cell 2 0) `shouldBe` False

    it "should return Cell coordinates" $ do
      getCoord (Cell 0 0) `shouldBe` (0, 0)
      getCoord (Cell 0 1) `shouldBe` (0, 1)
      getCoord (Cell 1 0) `shouldBe` (1, 0)
      getCoord (Cell (-1) (-1)) `shouldBe` ((-1), (-1))
