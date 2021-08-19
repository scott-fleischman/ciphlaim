module Ciphlaim.And where

import Ciphlaim.Fin
import Data.Generics.Labels ()
import Data.Vector (Vector)

createAnd :: FoldDirection -> Vector Fin -> Fin
createAnd foldDirection = directedFold foldDirection go Fin {size=1, value=0}
  where
  go :: Fin -> Int -> Fin -> Fin
  go Fin {size = currentSize, value = currentValue} _ Fin {size = newSize, value = newValue} =
    Fin {size = currentSize * newSize, value = currentValue * newSize + newValue}
