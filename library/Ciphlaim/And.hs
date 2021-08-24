-- FoldDirection LeftToRight means left (lower indexes) is the most significant digit
-- FoldDirection LeftToRight means right (higher indexes) is the least significant digit
module Ciphlaim.And where

import Ciphlaim.Fin
import Control.Lens.Operators
import Control.Monad.Trans.State.Strict qualified as State
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data DirectionSignificance
  = LowIndexMostSignificant
  | HighIndexMostSignificant
  deriving stock (Generic, Eq, Show)

significanceAsDirection :: DirectionSignificance -> FoldDirection
significanceAsDirection LowIndexMostSignificant = LeftToRight
significanceAsDirection HighIndexMostSignificant = RightToLeft

createAnd :: DirectionSignificance -> Vector Fin -> Fin
createAnd directionSignificance = directedFold foldDirection go Fin {size=1, value=0}
  where
  foldDirection = significanceAsDirection directionSignificance
  go :: Fin -> Int -> Fin -> Fin
  go
    Fin
    { size = currentSize,
      value = currentValue
    }
    _
    Fin
    { size = newSize,
      value = newValue
    }
    =
    Fin
    { size = currentSize * newSize,
      value = currentValue * newSize + newValue
    }

splitAnd :: DirectionSignificance -> Vector FinSize -> Fin -> Vector Natural
splitAnd directionSignificance sizes input =
  let sizesLength = Vector.length sizes
  in if sizesLength == 0
      then Vector.empty
      else
        let adjustIndex =
              case directionSignificance of
                LowIndexMostSignificant -> \index -> sizesLength - 1 - index
                HighIndexMostSignificant -> id
            go :: Int -> State.State Natural Natural
            go index = do
              nat <- State.get
              let FinSize {size} = sizes Vector.! (adjustIndex index)
              let (rest, value) = nat `divMod` size
              State.put rest
              pure value
            stateResult = Vector.generateM sizesLength go
            result = State.evalState stateResult (input ^. #value)
        in case directionSignificance of
            LowIndexMostSignificant -> Vector.reverse result
            HighIndexMostSignificant -> result
