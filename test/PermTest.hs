{-# LANGUAGE OverloadedLists #-}

module PermTest where

import Ciphlaim.And
import Ciphlaim.Perm
import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import TestCommon

data PermVecAssoc = PermVecAssoc
  { values :: Vector Int
  , perms :: Vector Fin
  }
  deriving stock (Generic, Eq, Show)

permVecAssocs :: Vector PermVecAssoc
permVecAssocs =
  [ PermVecAssoc [] []

  , PermVecAssoc [0] [Fin {size=1, value=0}]

  , PermVecAssoc [0, 1] [Fin {size=2, value=0}, Fin {size=1, value=0}]
  , PermVecAssoc [1, 0] [Fin {size=2, value=1}, Fin {size=1, value=0}]

  , PermVecAssoc [0, 1, 2] [Fin {size=3, value=0}, Fin {size=2, value=0}, Fin {size=1, value=0}]
  , PermVecAssoc [0, 2, 1] [Fin {size=3, value=0}, Fin {size=2, value=1}, Fin {size=1, value=0}]
  , PermVecAssoc [1, 0, 2] [Fin {size=3, value=1}, Fin {size=2, value=0}, Fin {size=1, value=0}]
  , PermVecAssoc [1, 2, 0] [Fin {size=3, value=1}, Fin {size=2, value=1}, Fin {size=1, value=0}]
  , PermVecAssoc [2, 0, 1] [Fin {size=3, value=2}, Fin {size=2, value=0}, Fin {size=1, value=0}]
  , PermVecAssoc [2, 1, 0] [Fin {size=3, value=2}, Fin {size=2, value=1}, Fin {size=1, value=0}]
  ]

data PermAssoc = PermAssoc
  { fin :: Fin
  , values :: Vector Int
  , dir :: DirectionSignificance
  }
  deriving stock (Generic, Eq, Show)

permAssocs :: Vector PermAssoc
permAssocs =
  [ PermAssoc Fin {size=1, value=0} [] LowIndexMostSignificant

  , PermAssoc Fin {size=1, value=0} [0] LowIndexMostSignificant

  , PermAssoc Fin {size=2, value=0} [0, 1] LowIndexMostSignificant
  , PermAssoc Fin {size=2, value=1} [1, 0] LowIndexMostSignificant
 
  -- , PermAssoc Fin {size=6, value=0} [0, 1, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=6, value=1} [0, 2, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=6, value=2} [1, 0, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=6, value=3} [1, 2, 0] LowIndexMostSignificant
  -- , PermAssoc Fin {size=6, value=4} [2, 0, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=6, value=5} [2, 1, 0] LowIndexMostSignificant

  -- , PermAssoc Fin {size=24, value=0} [0, 1, 2, 3] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=1} [0, 1, 3, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=2} [0, 2, 1, 3] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=3} [0, 2, 3, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=4} [0, 3, 1, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=5} [0, 3, 2, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=6} [1, 0, 2, 3] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=7} [1, 0, 3, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=8} [1, 2, 0, 3] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=9} [1, 2, 3, 0] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=10} [1, 3, 0, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=11} [1, 3, 2, 0] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=12} [2, 0, 1, 3] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=13} [2, 0, 3, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=14} [2, 1, 0, 3] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=15} [2, 1, 3, 0] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=16} [2, 3, 0, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=17} [2, 3, 1, 0] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=18} [3, 0, 1, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=19} [3, 0, 2, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=20} [3, 1, 0, 2] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=21} [3, 1, 2, 0] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=22} [3, 2, 0, 1] LowIndexMostSignificant
  -- , PermAssoc Fin {size=24, value=23} [3, 2, 1, 0] LowIndexMostSignificant

  , PermAssoc Fin {size=1, value=0} [] HighIndexMostSignificant

  , PermAssoc Fin {size=1, value=0} [0] HighIndexMostSignificant

  , PermAssoc Fin {size=2, value=0} [0, 1] HighIndexMostSignificant
  , PermAssoc Fin {size=2, value=1} [1, 0] HighIndexMostSignificant
 
  , PermAssoc Fin {size=6, value=0} [0, 1, 2] HighIndexMostSignificant
  , PermAssoc Fin {size=6, value=1} [1, 0, 2] HighIndexMostSignificant
  , PermAssoc Fin {size=6, value=2} [2, 0, 1] HighIndexMostSignificant
  , PermAssoc Fin {size=6, value=3} [0, 2, 1] HighIndexMostSignificant
  , PermAssoc Fin {size=6, value=4} [1, 2, 0] HighIndexMostSignificant
  , PermAssoc Fin {size=6, value=5} [2, 1, 0] HighIndexMostSignificant
  ]

permTest :: IO ()
permTest = do
  Vector.forM_ permVecAssocs \permVecAssoc@PermVecAssoc {values, perms} ->
    do
      putStrLn ("createPermVector " <> show permVecAssoc)
      createPermVector values `shouldBe` perms

  -- Vector.forM_ permAssocs \permAssoc@PermAssoc {fin, values, dir} ->
  --   do
  --     putStrLn ("createPerm " <> show permAssoc)
  --     createPerm dir values `shouldBe` fin

  Vector.forM_ permAssocs \permAssoc@PermAssoc {fin, values} ->
    do
      putStrLn ("splitPerm " <> show permAssoc)
      splitPerm (FinSize (fromIntegral (Vector.length values))) fin `shouldBe` values
