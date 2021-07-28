{-# LANGUAGE OverloadedLists #-}

module Main where

import Ciphlaim.Integer
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Test.Sandwich

data OrAssoc = OrAssoc
  { fin :: Fin
  , orRef :: OrRef
  , sizes :: Vector FinSize
  , dir :: IndexDirection
  }
  deriving stock (Generic, Eq, Show)

orAssocs :: Vector OrAssoc
orAssocs =
  [ OrAssoc Fin {size=1, value=0} OrRef {index=0, value=0} [1] LowerIndexFirst

  , OrAssoc Fin {size=2, value=0} OrRef {index=0, value=0} [2] LowerIndexFirst
  , OrAssoc Fin {size=2, value=1} OrRef {index=0, value=1} [2] LowerIndexFirst
 
  , OrAssoc Fin {size=5, value=0} OrRef {index=0, value=0} [2,3] LowerIndexFirst
  , OrAssoc Fin {size=5, value=1} OrRef {index=0, value=1} [2,3] LowerIndexFirst
  , OrAssoc Fin {size=5, value=2} OrRef {index=1, value=0} [2,3] LowerIndexFirst
  , OrAssoc Fin {size=5, value=3} OrRef {index=1, value=1} [2,3] LowerIndexFirst
  , OrAssoc Fin {size=5, value=4} OrRef {index=1, value=2} [2,3] LowerIndexFirst

  , OrAssoc Fin {size=9, value=0} OrRef {index=0, value=0} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=1} OrRef {index=0, value=1} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=2} OrRef {index=0, value=2} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=3} OrRef {index=0, value=3} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=4} OrRef {index=1, value=0} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=5} OrRef {index=1, value=1} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=6} OrRef {index=2, value=0} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=7} OrRef {index=2, value=1} [4,2,3] LowerIndexFirst
  , OrAssoc Fin {size=9, value=8} OrRef {index=2, value=2} [4,2,3] LowerIndexFirst

  , OrAssoc Fin {size=5, value=0} OrRef {index=1, value=0} [2,3] HigherIndexFirst
  , OrAssoc Fin {size=5, value=1} OrRef {index=1, value=1} [2,3] HigherIndexFirst
  , OrAssoc Fin {size=5, value=2} OrRef {index=1, value=2} [2,3] HigherIndexFirst
  , OrAssoc Fin {size=5, value=3} OrRef {index=0, value=0} [2,3] HigherIndexFirst
  , OrAssoc Fin {size=5, value=4} OrRef {index=0, value=1} [2,3] HigherIndexFirst

  , OrAssoc Fin {size=9, value=0} OrRef {index=2, value=0} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=1} OrRef {index=2, value=1} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=2} OrRef {index=2, value=2} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=3} OrRef {index=1, value=0} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=4} OrRef {index=1, value=1} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=5} OrRef {index=0, value=0} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=6} OrRef {index=0, value=1} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=7} OrRef {index=0, value=2} [4,2,3] HigherIndexFirst
  , OrAssoc Fin {size=9, value=8} OrRef {index=0, value=3} [4,2,3] HigherIndexFirst
  ]

top :: TopSpec
top = do
  let makeLabel OrAssoc {fin, orRef, sizes, dir}
        = show dir
        <> " " <> show sizes
        <> " fin:" <> show (fin ^. #value)
        <> " (i:" <> show (orRef ^. #index)
        <> ",v:" <> show (orRef ^. #value)
        <> ")"
  describe "createOr" $ do
    Vector.forM_ orAssocs \orAssoc@OrAssoc {fin, orRef, sizes, dir} ->
      it
        ("createOr " <> makeLabel orAssoc)
        (createOr dir orRef sizes `shouldBe` Right fin)
  describe "splitOr" $ do
    Vector.forM_ orAssocs \orAssoc@OrAssoc {fin, orRef, sizes, dir} ->
      it
        ("splitOr " <> makeLabel orAssoc)
        (splitOr dir sizes fin `shouldBe` Right orRef)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions top
