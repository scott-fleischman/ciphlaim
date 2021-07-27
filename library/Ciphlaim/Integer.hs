module Ciphlaim.Integer where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Text (Text)
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

orSize :: [FinSize] -> FinSize
orSize = sum

orSizeFin :: [Fin] -> FinSize
orSizeFin = orSize . fmap (FinSize . (^. #size))

splitOr :: [FinSize] -> Fin -> Either Text (Int, Natural)
splitOr sizes Fin {size, value} =
  if FinSize size /= orSize sizes
    then Left "Input size mismatch"
    else
      let (_finalSize, index, maybeResult) = foldr go (0, length sizes, Nothing) sizes
      in case maybeResult of
        Nothing -> Left "Unable to split"
        Just result -> Right (index, result)
  where
  go :: FinSize -> (FinSize, Int, Maybe Natural) -> (FinSize, Int, Maybe Natural)
  go _ result@(_, _, Just _) = result -- keep same if found answer
  go currentSize (sumSize, index, Nothing) =
    let newSize = sumSize + currentSize
        newSizeNat = newSize ^. #size
        sumSizeNat = sumSize ^. #size
        newResult =
          if value < newSizeNat && value >= sumSizeNat
            then Just (value - sumSizeNat)
            else Nothing
    in (newSize, index - 1, newResult)
