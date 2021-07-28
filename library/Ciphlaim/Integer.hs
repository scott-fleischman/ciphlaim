module Ciphlaim.Integer where

import Control.Lens ((^.))
import Control.Monad (when)
import Data.Generics.Labels ()
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data Fin = Fin
  { size :: Natural
  , value :: Natural
  }
  deriving stock (Generic, Eq, Show)

newtype FinSize = FinSize { size :: Natural }
  deriving stock (Generic, Eq, Show)
  deriving newtype (Num)

data IndexDirection = LowerIndexFirst | HigherIndexFirst
  deriving stock (Generic, Eq, Show)

data OrRef = OrRef
  { index :: Int
  , value :: Natural
  }
  deriving stock (Generic, Eq, Show)

createOr :: IndexDirection -> OrRef -> Vector FinSize -> Either Text Fin
createOr indexDirection OrRef {index, value} sizes = do
  let sizesLength = Vector.length sizes
  when (sizesLength < 1) do
    Left "createOr: sizes must be at least length 1"
  when (index >= sizesLength) do
    Left "createOr: index must be less than sizes"
  let (leftSizes, currentAndRightSizes) = Vector.splitAt index sizes
      FinSize size = Vector.head currentAndRightSizes
  when (value >= size) do
    Left "createOr: value is too big for indexed size"
  let rightSizes = Vector.tail currentAndRightSizes
      fin = Fin {size, value}
      (leftFunction, rightFunction) =
        case indexDirection of
          LowerIndexFirst -> (increaseValue, increaseSize)
          HigherIndexFirst -> (increaseSize, increaseValue)
      newValue = rightFunction (orSize rightSizes) fin
  pure $
    leftFunction (orSize leftSizes) newValue  

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

orSize :: Foldable f => f FinSize -> FinSize
orSize = sum

orSizeFin :: (Functor f, Foldable f) => f Fin -> FinSize
orSizeFin = orSize . fmap (FinSize . (^. #size))

splitOr :: IndexDirection -> Vector FinSize -> Fin -> Either Text OrRef
splitOr indexDirection sizes Fin {size, value} = do
  when (Vector.length sizes <= 0) do
    Left "Input sizes must be non-empty"
  let combinedSizes = orSize sizes
  when (FinSize size /= combinedSizes) do
    Left "Input size mismatch"
  let directedFold =
        case indexDirection of
          LowerIndexFirst -> Vector.ifoldl' go
          HigherIndexFirst -> Vector.ifoldr' (\i e r -> go r i e)
  let (_finalSize, maybeResult) = directedFold (0, Nothing) sizes
  case maybeResult of
    Nothing -> Left "Unable to split"
    Just result -> Right result
  where
  go :: (FinSize, Maybe OrRef) -> Int -> FinSize -> (FinSize, Maybe OrRef)
  go result@(_, Just _) _ _ = result -- keep same if found answer
  go (sumSize, Nothing) index currentSize =
    let newSize = sumSize + currentSize
        newSizeNat = newSize ^. #size
        sumSizeNat = sumSize ^. #size
        newResult =
          if value < newSizeNat && value >= sumSizeNat
            then Just OrRef {index, value = value - sumSizeNat}
            else Nothing
    in (newSize, newResult)
