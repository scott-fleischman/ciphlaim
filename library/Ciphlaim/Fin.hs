module Ciphlaim.Fin where

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

data IndexDirection = LowerIndexFirst | HigherIndexFirst
  deriving stock (Generic, Eq, Show)
