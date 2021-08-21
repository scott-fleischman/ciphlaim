{-# LANGUAGE OverloadedLists #-}

module AndTest where

import Ciphlaim.And
import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import TestCommon

data AndAssoc = AndAssoc
  { fin :: Fin
  , broken :: Vector Fin
  , dir :: FoldDirection
  }
  deriving stock (Generic, Eq, Show)

andAssocs :: Vector AndAssoc
andAssocs =
  [ AndAssoc Fin {size=1, value=0} [] LeftToRight

  , AndAssoc Fin {size=1, value=0} [Fin {size=1, value=0}] LeftToRight

  , AndAssoc Fin {size=2, value=0} [Fin {size=2, value=0}] LeftToRight
  , AndAssoc Fin {size=2, value=1} [Fin {size=2, value=1}] LeftToRight

  , AndAssoc Fin {size=6, value=0} [Fin {size=2, value=0}, Fin {size=3, value=0}] LeftToRight
  , AndAssoc Fin {size=6, value=1} [Fin {size=2, value=0}, Fin {size=3, value=1}] LeftToRight
  , AndAssoc Fin {size=6, value=2} [Fin {size=2, value=0}, Fin {size=3, value=2}] LeftToRight
  , AndAssoc Fin {size=6, value=3} [Fin {size=2, value=1}, Fin {size=3, value=0}] LeftToRight
  , AndAssoc Fin {size=6, value=4} [Fin {size=2, value=1}, Fin {size=3, value=1}] LeftToRight
  , AndAssoc Fin {size=6, value=5} [Fin {size=2, value=1}, Fin {size=3, value=2}] LeftToRight

  , AndAssoc Fin {size=6, value=5} [Fin {size=2, value=1}, Fin {size=3, value=2}, Fin {size=1, value=0}] LeftToRight

  , AndAssoc Fin {size=30, value=0} [Fin {size=2, value=0}, Fin {size=3, value=0}, Fin {size=5, value=0}] LeftToRight
  , AndAssoc Fin {size=30, value=1} [Fin {size=2, value=0}, Fin {size=3, value=0}, Fin {size=5, value=1}] LeftToRight
  , AndAssoc Fin {size=30, value=2} [Fin {size=2, value=0}, Fin {size=3, value=0}, Fin {size=5, value=2}] LeftToRight
  , AndAssoc Fin {size=30, value=3} [Fin {size=2, value=0}, Fin {size=3, value=0}, Fin {size=5, value=3}] LeftToRight
  , AndAssoc Fin {size=30, value=4} [Fin {size=2, value=0}, Fin {size=3, value=0}, Fin {size=5, value=4}] LeftToRight
  , AndAssoc Fin {size=30, value=5} [Fin {size=2, value=0}, Fin {size=3, value=1}, Fin {size=5, value=0}] LeftToRight
  , AndAssoc Fin {size=30, value=6} [Fin {size=2, value=0}, Fin {size=3, value=1}, Fin {size=5, value=1}] LeftToRight
  , AndAssoc Fin {size=30, value=7} [Fin {size=2, value=0}, Fin {size=3, value=1}, Fin {size=5, value=2}] LeftToRight
  , AndAssoc Fin {size=30, value=8} [Fin {size=2, value=0}, Fin {size=3, value=1}, Fin {size=5, value=3}] LeftToRight
  , AndAssoc Fin {size=30, value=9} [Fin {size=2, value=0}, Fin {size=3, value=1}, Fin {size=5, value=4}] LeftToRight
  , AndAssoc Fin {size=30, value=10} [Fin {size=2, value=0}, Fin {size=3, value=2}, Fin {size=5, value=0}] LeftToRight
  , AndAssoc Fin {size=30, value=11} [Fin {size=2, value=0}, Fin {size=3, value=2}, Fin {size=5, value=1}] LeftToRight
  , AndAssoc Fin {size=30, value=12} [Fin {size=2, value=0}, Fin {size=3, value=2}, Fin {size=5, value=2}] LeftToRight
  , AndAssoc Fin {size=30, value=13} [Fin {size=2, value=0}, Fin {size=3, value=2}, Fin {size=5, value=3}] LeftToRight
  , AndAssoc Fin {size=30, value=14} [Fin {size=2, value=0}, Fin {size=3, value=2}, Fin {size=5, value=4}] LeftToRight
  , AndAssoc Fin {size=30, value=15} [Fin {size=2, value=1}, Fin {size=3, value=0}, Fin {size=5, value=0}] LeftToRight
  , AndAssoc Fin {size=30, value=16} [Fin {size=2, value=1}, Fin {size=3, value=0}, Fin {size=5, value=1}] LeftToRight
  , AndAssoc Fin {size=30, value=17} [Fin {size=2, value=1}, Fin {size=3, value=0}, Fin {size=5, value=2}] LeftToRight
  , AndAssoc Fin {size=30, value=18} [Fin {size=2, value=1}, Fin {size=3, value=0}, Fin {size=5, value=3}] LeftToRight
  , AndAssoc Fin {size=30, value=19} [Fin {size=2, value=1}, Fin {size=3, value=0}, Fin {size=5, value=4}] LeftToRight


  , AndAssoc Fin {size=1, value=0} [] RightToLeft

  , AndAssoc Fin {size=1, value=0} [Fin {size=1, value=0}] RightToLeft

  , AndAssoc Fin {size=2, value=0} [Fin {size=2, value=0}] RightToLeft
  , AndAssoc Fin {size=2, value=1} [Fin {size=2, value=1}] RightToLeft

  , AndAssoc Fin {size=6, value=0} [Fin {size=2, value=0}, Fin {size=3, value=0}] RightToLeft
  , AndAssoc Fin {size=6, value=1} [Fin {size=2, value=1}, Fin {size=3, value=0}] RightToLeft
  , AndAssoc Fin {size=6, value=2} [Fin {size=2, value=0}, Fin {size=3, value=1}] RightToLeft
  , AndAssoc Fin {size=6, value=3} [Fin {size=2, value=1}, Fin {size=3, value=1}] RightToLeft
  , AndAssoc Fin {size=6, value=4} [Fin {size=2, value=0}, Fin {size=3, value=2}] RightToLeft
  , AndAssoc Fin {size=6, value=5} [Fin {size=2, value=1}, Fin {size=3, value=2}] RightToLeft

  , AndAssoc Fin {size=6, value=5} [Fin {size=2, value=1}, Fin {size=3, value=2}, Fin {size=1, value=0}] RightToLeft

  , AndAssoc Fin {size=30, value=0} [Fin {size=2, value=0}, Fin {size=3, value=0}, Fin {size=5, value=0}] RightToLeft
  , AndAssoc Fin {size=30, value=1} [Fin {size=2, value=1}, Fin {size=3, value=0}, Fin {size=5, value=0}] RightToLeft
  , AndAssoc Fin {size=30, value=2} [Fin {size=2, value=0}, Fin {size=3, value=1}, Fin {size=5, value=0}] RightToLeft
  , AndAssoc Fin {size=30, value=3} [Fin {size=2, value=1}, Fin {size=3, value=1}, Fin {size=5, value=0}] RightToLeft
  , AndAssoc Fin {size=30, value=4} [Fin {size=2, value=0}, Fin {size=3, value=2}, Fin {size=5, value=0}] RightToLeft
  , AndAssoc Fin {size=30, value=5} [Fin {size=2, value=1}, Fin {size=3, value=2}, Fin {size=5, value=0}] RightToLeft
  , AndAssoc Fin {size=30, value=6} [Fin {size=2, value=0}, Fin {size=3, value=0}, Fin {size=5, value=1}] RightToLeft
  , AndAssoc Fin {size=30, value=7} [Fin {size=2, value=1}, Fin {size=3, value=0}, Fin {size=5, value=1}] RightToLeft
  , AndAssoc Fin {size=30, value=8} [Fin {size=2, value=0}, Fin {size=3, value=1}, Fin {size=5, value=1}] RightToLeft
  , AndAssoc Fin {size=30, value=9} [Fin {size=2, value=1}, Fin {size=3, value=1}, Fin {size=5, value=1}] RightToLeft
  ]

andTest :: IO ()
andTest = do
  let makeLabel AndAssoc {fin, broken, dir}
        = show dir
        <> " " <> show fin
        <> " " <> show broken

  Vector.forM_ andAssocs \andAssoc@AndAssoc {fin, broken, dir} ->
    do
      putStrLn ("createAnd " <> makeLabel andAssoc)
      createAnd dir broken `shouldBe` fin

  Vector.forM_ andAssocs \andAssoc@AndAssoc {fin, broken, dir} ->
    do
      putStrLn ("splitAnd " <> makeLabel andAssoc)
      splitAnd dir (getSizes broken) fin `shouldBe` broken
