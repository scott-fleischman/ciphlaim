{-# LANGUAGE MagicHash #-}

-- | A list is a collection of a certain number of items all of which have the same size
module Ciphlaim.List where

import Ciphlaim.And
import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Exts (Int (..))
import GHC.Integer.Logarithms (integerLogBase#)
import GHC.Natural (Natural, naturalToInteger)

createList :: DirectionSignificance -> FinSize -> Vector Natural -> Fin
createList dir FinSize {size} values =
  let makeFin value = Fin {size, value}
  in createAnd dir (makeFin <$> values)

splitList :: DirectionSignificance -> FinSize -> Fin -> Vector Natural
splitList _ inputSize _ | inputSize == 0 = Vector.empty -- error ?
splitList _ inputSize _ | inputSize == 1 = Vector.singleton 0 -- error ?
splitList dir inputSize@FinSize {size = itemSize} fin@Fin {size} =
  let count = I# (integerLogBase# (naturalToInteger itemSize) (naturalToInteger size))
      sizes = Vector.replicate count inputSize
  in splitAnd dir sizes fin

-- Apply a list as if it were a function
apply :: DirectionSignificance -> Fin -> Fin -> Fin
apply dir table Fin {size, value} =
  let tableVector = splitList dir FinSize {size} table
      newValue = tableVector Vector.! (fromIntegral @Natural @Int value)
  in Fin {size, value = newValue}
