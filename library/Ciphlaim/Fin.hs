module Ciphlaim.Fin where

import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data Fin = Fin
  { size :: Natural
  , value :: Natural
  }
  deriving stock (Generic, Eq, Show)

newtype FinSize = FinSize { size :: Natural }
  deriving stock (Generic, Eq)
  deriving newtype (Num, Show)

-- | Increase the size of the Fin without changing its value
increaseSize :: FinSize -> Fin -> Fin
increaseSize FinSize {size = increase} Fin {size, value} =
  Fin
    { size = size + increase
    , value
    }

-- | Increase the value and the size of the Fin
increaseValue :: FinSize -> Fin -> Fin
increaseValue FinSize {size = increase} Fin {size, value} =
  Fin
    { size = size + increase
    , value = value + increase
    }

data FoldDirection = LeftToRight | RightToLeft
  deriving stock (Generic, Eq, Show)

directedFold :: FoldDirection -> (a -> Int -> b -> a) -> a -> Vector b -> a
directedFold foldDirection go =
  case foldDirection of
    LeftToRight -> Vector.ifoldl' go
    RightToLeft -> Vector.ifoldr' (\i e r -> go r i e)
