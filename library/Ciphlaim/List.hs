{-# LANGUAGE MagicHash #-}

-- | A list is a collection of a certain number of items all of which have the same size
module Ciphlaim.List where

import Ciphlaim.And
import Ciphlaim.Fin
import Control.Lens.Operators
import Data.Functor.Identity (Identity (..))
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
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
applyCalculateOutputSize :: Fin -> Fin -> Fin
applyCalculateOutputSize table Fin {size = inputSize, value} =
  let tableSize = table ^. #size
      outputSize =
        fromMaybe
          (error $ "cannot find outputSize for tableSize=" <> show tableSize <> ", inputSize=" <> show inputSize)
          (findNthRoot inputSize tableSize)
      resultValue =
        runIdentity $
          applyMemoryManyKnownOutputSize FinSize {size=outputSize} table
            (Identity value)
  in Fin {size=outputSize, value=resultValue}

applyMemoryManyKnownOutputSize :: Functor f => FinSize -> Fin -> f Natural -> f Natural
applyMemoryManyKnownOutputSize outputSize table =
  let tableVector = splitList LowIndexMostSignificant outputSize table
  in fmap ((tableVector Vector.!) . fromIntegral @Natural @Int)

findNthRoot :: Natural -> Natural -> Maybe Natural
findNthRoot p r | p <= 1 || r <= 1 = Nothing
findNthRoot p r = guess 2
  where
  guess d =
    case compare (d ^ p) r of
      LT -> guess (d + 1)
      EQ -> Just d
      GT -> Nothing

-- composeLeftFirst :: FinSize -> Fin -> Fin -> Fin
-- composeLeftFirst
--   leftInputSize
--   Fin {size = leftSize, value = leftValue}
--   Fin {size = rightSize, value = rightValue}
--   =
--   let 
--   in _
