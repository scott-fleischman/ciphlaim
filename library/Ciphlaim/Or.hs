module Ciphlaim.Or where

import Ciphlaim.Fin
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- LowerIndexFirst means lower indexes into the size list are tried first when extracting a value
-- HigherIndexFirst means higher indexes into the size list are tried first when extracting a value
data IndexDirection = LowerIndexFirst | HigherIndexFirst
  deriving stock (Generic, Eq, Show)

indexDirectionToFoldDirection :: IndexDirection -> FoldDirection
indexDirectionToFoldDirection LowerIndexFirst = LeftToRight
indexDirectionToFoldDirection HigherIndexFirst = RightToLeft

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
  let (_finalSize, maybeResult) = directedFold (indexDirectionToFoldDirection indexDirection) go (0, Nothing) sizes
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
