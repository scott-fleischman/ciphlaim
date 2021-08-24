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

combine :: FinSize -> Vector FinSize -> FoldDirection -> Vector (Natural, Vector Natural) -> Vector AndAssoc
combine FinSize {size} sizes dir pairs =
  (\(value, values) -> AndAssoc Fin {size, value} Combo {sizes, values} dir) <$> pairs

andAssocs :: Vector AndAssoc
andAssocs =
  combine 1 [] LeftToRight
    [ (0, [])
    ]
  <>
  combine 1 [1] LeftToRight
    [ (0, [0])
    ]
  <>
  combine 2 [2] LeftToRight
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 6 [2,3] LeftToRight
    [ (0, [0,0])
    , (1, [0,1])
    , (2, [0,2])
    , (3, [1,0])
    , (4, [1,1])
    , (5, [1,2])
    ]
  <>
  combine 6 [2,3,1] LeftToRight
    [ (5, [1,2,0])
    ]
  <>
  combine 30 [2,3,5] LeftToRight
    [ (0, [0,0,0])
    , (1, [0,0,1])
    , (2, [0,0,2])
    , (3, [0,0,3])
    , (4, [0,0,4])
    , (5, [0,1,0])
    , (6, [0,1,1])
    , (7, [0,1,2])
    , (8, [0,1,3])
    , (9, [0,1,4])
    , (10, [0,2,0])
    , (11, [0,2,1])
    , (12, [0,2,2])
    , (13, [0,2,3])
    , (14, [0,2,4])
    , (15, [1,0,0])
    , (16, [1,0,1])
    , (17, [1,0,2])
    , (18, [1,0,3])
    , (19, [1,0,4])
    , (20, [1,1,0])
    , (21, [1,1,1])
    , (22, [1,1,2])
    , (23, [1,1,3])
    , (24, [1,1,4])
    , (25, [1,2,0])
    , (26, [1,2,1])
    , (27, [1,2,2])
    , (28, [1,2,3])
    , (29, [1,2,4])
    ]
  <>

  combine 1 [] RightToLeft
    [ (0, [])
    ]
  <>
  combine 1 [1] RightToLeft
    [ (0, [0])
    ]
  <>
  combine 2 [2] RightToLeft
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 6 [2,3] RightToLeft
    [ (0, [0,0])
    , (1, [1,0])
    , (2, [0,1])
    , (3, [1,1])
    , (4, [0,2])
    , (5, [1,2])
    ]
  <>
  combine 6 [2,3,1] RightToLeft
    [ (5, [1,2,0])
    ]
  <>
  combine 30 [2,3,5] RightToLeft
    [ (0, [0,0,0])
    , (1, [1,0,0])
    , (2, [0,1,0])
    , (3, [1,1,0])
    , (4, [0,2,0])
    , (5, [1,2,0])
    , (6, [0,0,1])
    , (7, [1,0,1])
    , (8, [0,1,1])
    , (9, [1,1,1])
    , (10, [0,2,1])
    , (11, [1,2,1])
    , (12, [0,0,2])
    , (13, [1,0,2])
    , (14, [0,1,2])
    , (15, [1,1,2])
    , (16, [0,2,2])
    , (17, [1,2,2])
    , (18, [0,0,3])
    , (19, [1,0,3])
    , (20, [0,1,3])
    , (21, [1,1,3])
    , (22, [0,2,3])
    , (23, [1,2,3])
    , (24, [0,0,4])
    , (25, [1,0,4])
    , (26, [0,1,4])
    , (27, [1,1,4])
    , (28, [0,2,4])
    , (29, [1,2,4])
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
