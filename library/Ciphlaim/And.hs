-- FoldDirection LeftToRight means left (lower indexes) is the most significant digit
-- FoldDirection LeftToRight means right (higher indexes) is the least significant digit
module Ciphlaim.And where

import Ciphlaim.Fin
import Control.Lens.Operators
import Control.Monad.Trans.State.Strict qualified as State
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Numeric.Natural (Natural)

createAnd :: FoldDirection -> Vector Fin -> Fin
createAnd foldDirection = directedFold foldDirection go Fin {size=1, value=0}
  where
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

splitAnd :: FoldDirection -> Vector FinSize -> Fin -> Vector Fin
splitAnd foldDirection sizes input =
  let sizesLength = Vector.length sizes
  in if sizesLength == 0
      then Vector.empty
      else
        let adjustIndex =
              case foldDirection of
                LeftToRight -> \index -> sizesLength - 1 - index
                RightToLeft -> id
            go :: Int -> State.State Natural Fin
            go index = do
              nat <- State.get
              let FinSize {size} = sizes Vector.! (adjustIndex index)
              let (rest, value) = nat `divMod` size
              State.put rest
              pure Fin {size, value}
            stateResult = Vector.generateM sizesLength go
            result = State.evalState stateResult (input ^. #value)
        in case foldDirection of
            LeftToRight -> Vector.reverse result
            RightToLeft -> result
