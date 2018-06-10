module LifeSpec where

import Life
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "life handling" $ do
    it "should create life" $ do
      createLife (Cell 10 10) `shouldBe` Alive (Cell 10 10)

    it "should create dead" $ do
      createDead (Cell 2 2) `shouldBe` Dead (Cell 2 2)

    it "should not have neighbours" $ do
      neighbours (createLife (Cell 10 10)) [] `shouldBe` []

    it "should have all neighbours" $ do
      let mates = createLife (Cell 1 1) : createLife (Cell 1 0) : createLife (Cell 0 1) : [] in
        neighbours (createLife (Cell 0 0)) mates `shouldBe` mates

    it "should filter neighbours among all lifes" $ do
      let mates = createLife (Cell 0 2) : createLife (Cell 1 0) : createLife (Cell 2 1) : [] in
        neighbours (createLife (Cell 0 0)) mates `shouldBe` createLife (Cell 1 0) : []


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

