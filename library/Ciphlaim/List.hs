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
apply :: FinSize -> Fin -> Fin -> Natural
apply outputSize table Fin {value} =
  -- we could solve for outputSize, since tableSize = outputSize ^ inputSize
  -- but this involves finding the inputSize root of the tableSize, which is not straightforward
  let tableVector = splitList LowIndexMostSignificant outputSize table
      newValue = tableVector Vector.! (fromIntegral @Natural @Int value)
  in newValue

-- composeLeftFirst :: FinSize -> Fin -> Fin -> Fin
-- composeLeftFirst
--   leftInputSize
--   Fin {size = leftSize, value = leftValue}
--   Fin {size = rightSize, value = rightValue}
--   =
--   let 
--   in _
