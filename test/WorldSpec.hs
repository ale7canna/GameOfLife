module WorldSpec where

import Test.Hspec
import World
import Life

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "World must handle life cycle" $ do
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
      evolve (life 1 1 : life 1 0 : []) (life 0 0)  `shouldBe` life 0 0
      evolve (life 1 1 : dead 1 0 : []) (life 0 0)  `shouldBe` dead 0 0
      evolve (life 1 1 : []) (life 0 0) `shouldBe` dead 0 0
      evolve (dead 1 0 : []) (life 0 0) `shouldBe` dead 0 0

    it "should be overpopulated if more then 3 living cell around" $ do
      evolve (life 1 1 : life 1 0 : life 0 1 : life 2 0      : []) (life 0 0) `shouldBe` life 0 0
      evolve (life 1 1 : life 1 0 : life 0 1 : life (-1) 0   : []) (life 0 0) `shouldBe` dead 0 0
      evolve (life 1 1 : dead 1 0 : life 0 1 : life (-1) 0   : []) (life 0 0) `shouldBe` life 0 0
      evolve (life 1 1 : life 1 0 : life 0 1 : life (-1) (-1): []) (life 0 0) `shouldBe` dead 0 0

    it "should evolve a cell" $ do
      evolve world1 (dead 0 0) `shouldBe` life 0 0
      evolve world1 (life 1 1) `shouldBe` life 1 1
      evolve world2 (life 1 0) `shouldBe` life 1 0
      evolve world2 (life 2 2) `shouldBe` life 2 2
      evolve world2 (life 1 1) `shouldBe` dead 1 1
      evolve world2 (dead 0 3) `shouldBe` dead 0 3
      evolve world2 (life 3 0) `shouldBe` life 3 0

    it "should evolve the entire world" $ do
      evolveWorld world1 `shouldBe` world1'
      evolveWorld world2 `shouldBe` world2'


world1 :: [Life]
world1    = dead 0 0 : life 0 1 :
            life 1 0 : life 1 1 : []

world1' :: [Life]
world1'   = life 0 0 : life 0 1 :
            life 1 0 : life 1 1 : []

world2 :: [Life]
world2    = dead 0 0 : life 0 1 : dead 0 2 : dead 0 3 :
            life 1 0 : life 1 1 : dead 1 2 : life 1 3 :
            life 2 0 : dead 2 1 : life 2 2 : dead 2 3 :
            life 3 0 : life 3 1 : dead 3 2 : dead 3 3 : []

world2' :: [Life]
world2'   = life 0 0 : life 0 1 : life 0 2 : dead 0 3 :
            life 1 0 : dead 1 1 : dead 1 2 : dead 1 3 :
            dead 2 0 : dead 2 1 : life 2 2 : dead 2 3 :
            life 3 0 : life 3 1 : dead 3 2 : dead 3 3 : []