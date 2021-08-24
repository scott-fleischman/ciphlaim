{-# LANGUAGE OverloadedLists #-}

module AndTest where

import Ciphlaim.And
import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import TestCommon

data Combo =
  Combo
  { sizes :: Vector FinSize
  , values :: Vector Natural
  }
  deriving stock (Generic, Eq, Show)

data AndAssoc = AndAssoc
  { fin :: Fin
  , combo :: Combo
  , dir :: FoldDirection
  }
  deriving stock (Generic, Eq, Show)

andAssocs :: Vector AndAssoc
andAssocs =
  [ AndAssoc Fin {size=1, value=0} Combo {sizes=[], values=[]} LeftToRight

  , AndAssoc Fin {size=1, value=0} Combo {sizes=[1], values=[0]} LeftToRight

  , AndAssoc Fin {size=2, value=0} Combo {sizes=[2], values=[0]} LeftToRight
  , AndAssoc Fin {size=2, value=1} Combo {sizes=[2], values=[1]} LeftToRight

  , AndAssoc Fin {size=6, value=0} Combo {sizes=[2,3], values=[0,0]} LeftToRight
  , AndAssoc Fin {size=6, value=1} Combo {sizes=[2,3], values=[0,1]} LeftToRight
  , AndAssoc Fin {size=6, value=2} Combo {sizes=[2,3], values=[0,2]} LeftToRight
  , AndAssoc Fin {size=6, value=3} Combo {sizes=[2,3], values=[1,0]} LeftToRight
  , AndAssoc Fin {size=6, value=4} Combo {sizes=[2,3], values=[1,1]} LeftToRight
  , AndAssoc Fin {size=6, value=5} Combo {sizes=[2,3], values=[1,2]} LeftToRight

  , AndAssoc Fin {size=6, value=5} Combo {sizes=[2,3,1], values=[1,2,0]} LeftToRight

  , AndAssoc Fin {size=30, value=0} Combo {sizes=[2,3,5], values=[0,0,0]} LeftToRight
  , AndAssoc Fin {size=30, value=1} Combo {sizes=[2,3,5], values=[0,0,1]} LeftToRight
  , AndAssoc Fin {size=30, value=2} Combo {sizes=[2,3,5], values=[0,0,2]} LeftToRight
  , AndAssoc Fin {size=30, value=3} Combo {sizes=[2,3,5], values=[0,0,3]} LeftToRight
  , AndAssoc Fin {size=30, value=4} Combo {sizes=[2,3,5], values=[0,0,4]} LeftToRight
  , AndAssoc Fin {size=30, value=5} Combo {sizes=[2,3,5], values=[0,1,0]} LeftToRight
  , AndAssoc Fin {size=30, value=6} Combo {sizes=[2,3,5], values=[0,1,1]} LeftToRight
  , AndAssoc Fin {size=30, value=7} Combo {sizes=[2,3,5], values=[0,1,2]} LeftToRight
  , AndAssoc Fin {size=30, value=8} Combo {sizes=[2,3,5], values=[0,1,3]} LeftToRight
  , AndAssoc Fin {size=30, value=9} Combo {sizes=[2,3,5], values=[0,1,4]} LeftToRight
  , AndAssoc Fin {size=30, value=10} Combo {sizes=[2,3,5], values=[0,2,0]} LeftToRight
  , AndAssoc Fin {size=30, value=11} Combo {sizes=[2,3,5], values=[0,2,1]} LeftToRight
  , AndAssoc Fin {size=30, value=12} Combo {sizes=[2,3,5], values=[0,2,2]} LeftToRight
  , AndAssoc Fin {size=30, value=13} Combo {sizes=[2,3,5], values=[0,2,3]} LeftToRight
  , AndAssoc Fin {size=30, value=14} Combo {sizes=[2,3,5], values=[0,2,4]} LeftToRight
  , AndAssoc Fin {size=30, value=15} Combo {sizes=[2,3,5], values=[1,0,0]} LeftToRight
  , AndAssoc Fin {size=30, value=16} Combo {sizes=[2,3,5], values=[1,0,1]} LeftToRight
  , AndAssoc Fin {size=30, value=17} Combo {sizes=[2,3,5], values=[1,0,2]} LeftToRight
  , AndAssoc Fin {size=30, value=18} Combo {sizes=[2,3,5], values=[1,0,3]} LeftToRight
  , AndAssoc Fin {size=30, value=19} Combo {sizes=[2,3,5], values=[1,0,4]} LeftToRight
  , AndAssoc Fin {size=30, value=20} Combo {sizes=[2,3,5], values=[1,1,0]} LeftToRight
  , AndAssoc Fin {size=30, value=21} Combo {sizes=[2,3,5], values=[1,1,1]} LeftToRight
  , AndAssoc Fin {size=30, value=22} Combo {sizes=[2,3,5], values=[1,1,2]} LeftToRight
  , AndAssoc Fin {size=30, value=23} Combo {sizes=[2,3,5], values=[1,1,3]} LeftToRight
  , AndAssoc Fin {size=30, value=24} Combo {sizes=[2,3,5], values=[1,1,4]} LeftToRight
  , AndAssoc Fin {size=30, value=25} Combo {sizes=[2,3,5], values=[1,2,0]} LeftToRight
  , AndAssoc Fin {size=30, value=26} Combo {sizes=[2,3,5], values=[1,2,1]} LeftToRight
  , AndAssoc Fin {size=30, value=27} Combo {sizes=[2,3,5], values=[1,2,2]} LeftToRight
  , AndAssoc Fin {size=30, value=28} Combo {sizes=[2,3,5], values=[1,2,3]} LeftToRight
  , AndAssoc Fin {size=30, value=29} Combo {sizes=[2,3,5], values=[1,2,4]} LeftToRight

  , AndAssoc Fin {size=1, value=0} Combo {sizes=[], values=[]} RightToLeft

  , AndAssoc Fin {size=1, value=0} Combo {sizes=[1], values=[0]} RightToLeft

  , AndAssoc Fin {size=2, value=0} Combo {sizes=[2], values=[0]} RightToLeft
  , AndAssoc Fin {size=2, value=1} Combo {sizes=[2], values=[1]} RightToLeft

  , AndAssoc Fin {size=6, value=0} Combo {sizes=[2,3], values=[0,0]} RightToLeft
  , AndAssoc Fin {size=6, value=1} Combo {sizes=[2,3], values=[1,0]} RightToLeft
  , AndAssoc Fin {size=6, value=2} Combo {sizes=[2,3], values=[0,1]} RightToLeft
  , AndAssoc Fin {size=6, value=3} Combo {sizes=[2,3], values=[1,1]} RightToLeft
  , AndAssoc Fin {size=6, value=4} Combo {sizes=[2,3], values=[0,2]} RightToLeft
  , AndAssoc Fin {size=6, value=5} Combo {sizes=[2,3], values=[1,2]} RightToLeft

  , AndAssoc Fin {size=6, value=5} Combo {sizes=[2,3,1], values=[1,2,0]} RightToLeft

  , AndAssoc Fin {size=30, value=0} Combo {sizes=[2,3,5], values=[0,0,0]} RightToLeft
  , AndAssoc Fin {size=30, value=1} Combo {sizes=[2,3,5], values=[1,0,0]} RightToLeft
  , AndAssoc Fin {size=30, value=2} Combo {sizes=[2,3,5], values=[0,1,0]} RightToLeft
  , AndAssoc Fin {size=30, value=3} Combo {sizes=[2,3,5], values=[1,1,0]} RightToLeft
  , AndAssoc Fin {size=30, value=4} Combo {sizes=[2,3,5], values=[0,2,0]} RightToLeft
  , AndAssoc Fin {size=30, value=5} Combo {sizes=[2,3,5], values=[1,2,0]} RightToLeft
  , AndAssoc Fin {size=30, value=6} Combo {sizes=[2,3,5], values=[0,0,1]} RightToLeft
  , AndAssoc Fin {size=30, value=7} Combo {sizes=[2,3,5], values=[1,0,1]} RightToLeft
  , AndAssoc Fin {size=30, value=8} Combo {sizes=[2,3,5], values=[0,1,1]} RightToLeft
  , AndAssoc Fin {size=30, value=9} Combo {sizes=[2,3,5], values=[1,1,1]} RightToLeft
  , AndAssoc Fin {size=30, value=10} Combo {sizes=[2,3,5], values=[0,2,1]} RightToLeft
  , AndAssoc Fin {size=30, value=11} Combo {sizes=[2,3,5], values=[1,2,1]} RightToLeft
  , AndAssoc Fin {size=30, value=12} Combo {sizes=[2,3,5], values=[0,0,2]} RightToLeft
  , AndAssoc Fin {size=30, value=13} Combo {sizes=[2,3,5], values=[1,0,2]} RightToLeft
  , AndAssoc Fin {size=30, value=14} Combo {sizes=[2,3,5], values=[0,1,2]} RightToLeft
  , AndAssoc Fin {size=30, value=15} Combo {sizes=[2,3,5], values=[1,1,2]} RightToLeft
  , AndAssoc Fin {size=30, value=16} Combo {sizes=[2,3,5], values=[0,2,2]} RightToLeft
  , AndAssoc Fin {size=30, value=17} Combo {sizes=[2,3,5], values=[1,2,2]} RightToLeft
  , AndAssoc Fin {size=30, value=18} Combo {sizes=[2,3,5], values=[0,0,3]} RightToLeft
  , AndAssoc Fin {size=30, value=19} Combo {sizes=[2,3,5], values=[1,0,3]} RightToLeft
  , AndAssoc Fin {size=30, value=20} Combo {sizes=[2,3,5], values=[0,1,3]} RightToLeft
  , AndAssoc Fin {size=30, value=21} Combo {sizes=[2,3,5], values=[1,1,3]} RightToLeft
  , AndAssoc Fin {size=30, value=22} Combo {sizes=[2,3,5], values=[0,2,3]} RightToLeft
  , AndAssoc Fin {size=30, value=23} Combo {sizes=[2,3,5], values=[1,2,3]} RightToLeft
  , AndAssoc Fin {size=30, value=24} Combo {sizes=[2,3,5], values=[0,0,4]} RightToLeft
  , AndAssoc Fin {size=30, value=25} Combo {sizes=[2,3,5], values=[1,0,4]} RightToLeft
  , AndAssoc Fin {size=30, value=26} Combo {sizes=[2,3,5], values=[0,1,4]} RightToLeft
  , AndAssoc Fin {size=30, value=27} Combo {sizes=[2,3,5], values=[1,1,4]} RightToLeft
  , AndAssoc Fin {size=30, value=28} Combo {sizes=[2,3,5], values=[0,2,4]} RightToLeft
  , AndAssoc Fin {size=30, value=29} Combo {sizes=[2,3,5], values=[1,2,4]} RightToLeft
  ]

andTest :: IO ()
andTest = do
  let makeLabel AndAssoc {fin, combo = Combo {sizes, values}, dir}
        = show dir
        <> " " <> show fin
        <> " sizes:" <> show sizes
        <> " values:" <> show values

  Vector.forM_ andAssocs \andAssoc@AndAssoc {fin, combo = Combo {sizes, values}, dir} ->
    do
      putStrLn ("createAnd " <> makeLabel andAssoc)
      createAnd dir (zipSizesAndValues sizes values) `shouldBe` fin

  Vector.forM_ andAssocs \andAssoc@AndAssoc {fin, combo = Combo {sizes, values}, dir} ->
    do
      putStrLn ("splitAnd " <> makeLabel andAssoc)
      splitAnd dir sizes fin `shouldBe` values
