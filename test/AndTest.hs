{-# LANGUAGE OverloadedLists #-}

module AndTest where

import Ciphlaim.And
import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)
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
  , dir :: DirectionSignificance
  }
  deriving stock (Generic, Eq, Show)

combine :: FinSize -> Vector FinSize -> DirectionSignificance -> Vector (Natural, Vector Natural) -> Vector AndAssoc
combine FinSize {size} sizes dir pairs =
  (\(value, values) -> AndAssoc Fin {size, value} Combo {sizes, values} dir) <$> pairs

andAssocs :: Vector AndAssoc
andAssocs =
  combine 1 [] LowIndexMostSignificant
    [ (0, [])
    ]
  <>
  combine 1 [1] LowIndexMostSignificant
    [ (0, [0])
    ]
  <>
  combine 2 [2] LowIndexMostSignificant
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 6 [2,3] LowIndexMostSignificant
    [ (0, [0,0])
    , (1, [0,1])
    , (2, [0,2])
    , (3, [1,0])
    , (4, [1,1])
    , (5, [1,2])
    ]
  <>
  combine 6 [2,3,1] LowIndexMostSignificant
    [ (5, [1,2,0])
    ]
  <>
  combine 30 [2,3,5] LowIndexMostSignificant
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

  combine 1 [] HighIndexMostSignificant
    [ (0, [])
    ]
  <>
  combine 1 [1] HighIndexMostSignificant
    [ (0, [0])
    ]
  <>
  combine 2 [2] HighIndexMostSignificant
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 6 [2,3] HighIndexMostSignificant
    [ (0, [0,0])
    , (1, [1,0])
    , (2, [0,1])
    , (3, [1,1])
    , (4, [0,2])
    , (5, [1,2])
    ]
  <>
  combine 6 [2,3,1] HighIndexMostSignificant
    [ (5, [1,2,0])
    ]
  <>
  combine 30 [2,3,5] HighIndexMostSignificant
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

andTests :: [(PropertyName, Property)]
andTests =
  do
    vectorFor andAssocs \andAssoc@AndAssoc {fin, combo = Combo {sizes, values}, dir} ->
      makeTest ("createAnd " <> show andAssoc) do
        createAnd dir (zipSizesAndValues sizes values) === fin
  <> do
    vectorFor andAssocs \andAssoc@AndAssoc {fin, combo = Combo {sizes, values}, dir} ->
      makeTest ("splitAnd " <> show andAssoc) do
        splitAnd dir sizes fin === values
