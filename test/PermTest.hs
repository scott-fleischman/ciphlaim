{-# LANGUAGE OverloadedLists #-}

module PermTest where

import Ciphlaim.Perm
import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import TestCommon

data PermAssoc = PermAssoc
  { fin :: Fin
  , values :: Vector Int
  }
  deriving stock (Generic, Eq, Show)

permAssocs :: Vector PermAssoc
permAssocs =
  [ PermAssoc Fin {size=1, value=0} []

  , PermAssoc Fin {size=1, value=0} [0]

  , PermAssoc Fin {size=2, value=0} [1, 0]
  , PermAssoc Fin {size=2, value=1} [0, 1]

  , PermAssoc Fin {size=6, value=3} [0, 1, 2]
  , PermAssoc Fin {size=6, value=2} [0, 2, 1]
  , PermAssoc Fin {size=6, value=2} [1, 0, 2]
  , PermAssoc Fin {size=6, value=1} [1, 2, 0]
  , PermAssoc Fin {size=6, value=1} [2, 0, 1]
  , PermAssoc Fin {size=6, value=0} [2, 1, 0]
  ]

permTest :: IO ()
permTest = do
  Vector.forM_ permAssocs \permAssoc@PermAssoc {fin, values} ->
    do
      putStrLn ("createPerm " <> show permAssoc)
      createPerm values `shouldBe` fin
