{-# LANGUAGE OverloadedLists #-}

module OrTest where

import Ciphlaim.Fin
import Ciphlaim.Or
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import TestCommon

data OrAssoc = OrAssoc
  { fin :: Fin
  , orRef :: OrRef
  , sizes :: Vector FinSize
  , dir :: IndexDirection
  }
  deriving stock (Generic, Eq, Show)

combine :: FinSize -> Vector FinSize -> IndexDirection -> Vector (Natural, OrRef) -> Vector OrAssoc
combine FinSize {size} sizes dir pairs =
  (\(value, orRef) -> OrAssoc {fin = Fin {size, value}, orRef, sizes, dir}) <$> pairs

orAssocs :: Vector OrAssoc
orAssocs =
  combine 1 [1] LowerIndexFirst
    [ (0, OrRef {index=0, value=0})
    ]
  <>
  combine 2 [2] LowerIndexFirst
    [ (0, OrRef {index=0, value=0})
    , (1, OrRef {index=0, value=1})
    ]
  <>
  combine 5 [2,3] LowerIndexFirst
    [ (0, OrRef {index=0, value=0})
    , (1, OrRef {index=0, value=1})
    , (2, OrRef {index=1, value=0})
    , (3, OrRef {index=1, value=1})
    , (4, OrRef {index=1, value=2})
    ]
  <>
  combine 9 [4,2,3] LowerIndexFirst
    [ (0, OrRef {index=0, value=0})
    , (1, OrRef {index=0, value=1})
    , (2, OrRef {index=0, value=2})
    , (3, OrRef {index=0, value=3})
    , (4, OrRef {index=1, value=0})
    , (5, OrRef {index=1, value=1})
    , (6, OrRef {index=2, value=0})
    , (7, OrRef {index=2, value=1})
    , (8, OrRef {index=2, value=2})
    ]
  <>
  combine 1 [1] HigherIndexFirst
    [ (0, OrRef {index=0, value=0})
    ]
  <>
  combine 2 [2] HigherIndexFirst
    [ (0, OrRef {index=0, value=0})
    , (1, OrRef {index=0, value=1})
    ]
  <>
  combine 5 [2,3] HigherIndexFirst
    [ (0, OrRef {index=1, value=0})
    , (1, OrRef {index=1, value=1})
    , (2, OrRef {index=1, value=2})
    , (3, OrRef {index=0, value=0})
    , (4, OrRef {index=0, value=1})
    ]
  <>
  combine 9 [4,2,3] HigherIndexFirst
    [ (0, OrRef {index=2, value=0})
    , (1, OrRef {index=2, value=1})
    , (2, OrRef {index=2, value=2})
    , (3, OrRef {index=1, value=0})
    , (4, OrRef {index=1, value=1})
    , (5, OrRef {index=0, value=0})
    , (6, OrRef {index=0, value=1})
    , (7, OrRef {index=0, value=2})
    , (8, OrRef {index=0, value=3})
    ]

orTests :: Spec
orTests = do
  describe "createOr" do
    Vector.forM_ orAssocs \orAssoc@OrAssoc {fin, orRef, sizes, dir} ->
      it ("createOr " <> show orAssoc) do
        createOr dir orRef sizes `shouldBe` Right fin
  describe "splitOr" do
    Vector.forM_ orAssocs \orAssoc@OrAssoc {fin, orRef, sizes, dir} ->
      it ("splitOr " <> show orAssoc) do
        splitOr dir sizes fin `shouldBe` Right orRef
