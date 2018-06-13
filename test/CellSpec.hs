module CellSpec where

import Cell
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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


