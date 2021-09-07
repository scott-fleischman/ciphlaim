module Ciphlaim.Checked where

import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (lookup)

data Fin = Fin { size :: Natural, value :: Natural }
  deriving stock (Generic, Eq, Show)

data Error = Error String
  deriving stock (Generic, Eq, Show)

data Ops =
  Ops
  { orShiftValue :: Natural -> Fin -> Either Error Fin
  , orExpandSize :: Natural -> Fin -> Either Error Fin
  , combineAnd :: Fin -> Fin -> Either Error Fin
  , splitAnd :: Natural -> Fin -> Either Error (Fin, Fin)
  }
