{-# LANGUAGE OverloadedLists #-}

module ListTest where

import Ciphlaim.And
import Ciphlaim.List
import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import TestCommon

data ListAssoc = ListAssoc
  { fin :: Fin
  , size :: FinSize
  , values :: Vector Natural
  , dir :: DirectionSignificance
  }
  deriving stock (Generic, Eq, Show)

combine :: FinSize -> FinSize -> DirectionSignificance -> Vector (Natural, Vector Natural) -> Vector ListAssoc
combine FinSize {size = combinedSize} itemSize dir pairs =
  (\(value, values) -> ListAssoc Fin {size = combinedSize, value} itemSize values dir) <$> pairs

listAssocs :: Vector ListAssoc
listAssocs =
  combine 1 0 LowIndexMostSignificant
    [ (0, [])
    ]
  <>
  combine 1 1 LowIndexMostSignificant
    [ (0, [0])
    ]
  <>
  combine 2 2 LowIndexMostSignificant
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 4 2 LowIndexMostSignificant
    [ (0, [0,0])
    , (1, [0,1])
    , (2, [1,0])
    , (3, [1,1])
    ]
  <>
  combine 8 2 LowIndexMostSignificant
    [ (0, [0,0,0])
    , (1, [0,0,1])
    , (2, [0,1,0])
    , (3, [0,1,1])
    , (4, [1,0,0])
    , (5, [1,0,1])
    , (6, [1,1,0])
    , (7, [1,1,1])
    ]
  <>
  combine 1 0 HighIndexMostSignificant
    [ (0, [])
    ]
  <>
  combine 1 1 HighIndexMostSignificant
    [ (0, [0])
    ]
  <>
  combine 2 2 HighIndexMostSignificant
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 4 2 HighIndexMostSignificant
    [ (0, [0,0])
    , (1, [1,0])
    , (2, [0,1])
    , (3, [1,1])
    ]
  <>
  combine 8 2 HighIndexMostSignificant
    [ (0, [0,0,0])
    , (1, [1,0,0])
    , (2, [0,1,0])
    , (3, [1,1,0])
    , (4, [0,0,1])
    , (5, [1,0,1])
    , (6, [0,1,1])
    , (7, [1,1,1])
    ]

listTest :: IO ()
listTest = do
  Vector.forM_ listAssocs \listAssoc@ListAssoc {fin, size, values, dir} ->
    do
      putStrLn ("createList " <> show listAssoc)
      createList dir size values `shouldBe` fin

  Vector.forM_ listAssocs \listAssoc@ListAssoc {fin, size, values, dir} ->
    do
      putStrLn ("splitList " <> show listAssoc)
      splitList dir size fin `shouldBe` values
