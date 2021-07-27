module Ciphlaim.Integer where

import qualified Control.Lens as Lens
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import qualified Data.List as List

data Fin = Fin
  { size :: Natural
  , value :: Natural
  }
  deriving stock (Generic, Eq, Show)

newtype FinSize = FinSize { size :: Natural }
  deriving stock (Generic, Eq, Show)
  deriving newtype (Num)

createOr :: Int -> Natural -> [FinSize] -> Fin
createOr i value xs =
  let (ls, FinSize size : rs) = List.splitAt i xs
      fin = Fin {size, value}
      newValue = increaseValue (orSize rs) fin
  in increaseSize (orSize ls) newValue  

-- | Increase the size of the Fin without changing its value
increaseSize :: FinSize -> Fin -> Fin
increaseSize FinSize {size = extra} Fin {size, value} =
  Fin
    { size = size + extra
    , value
    }

-- | Increase the value and the size of the Fin
increaseValue :: FinSize -> Fin -> Fin
increaseValue FinSize {size = increase} Fin {size, value} =
  Fin
    { size = size + increase
    , value = increase + value
    }

orSize :: [FinSize] -> FinSize
orSize = sum

orSizeFin :: [Fin] -> FinSize
orSizeFin = orSize . fmap (FinSize . Lens.view #size)
