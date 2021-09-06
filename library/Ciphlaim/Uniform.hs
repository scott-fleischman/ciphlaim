module Ciphlaim.Uniform where

import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

newtype Size = Size { size :: Natural }
  deriving stock (Generic, Eq)
  deriving newtype (Enum, Integral, Num, Ord, Real, Show)

newtype Value = Value { value :: Natural }
  deriving stock (Generic, Eq)
  deriving newtype (Enum, Integral, Num, Ord, Real, Show)

sizeAsValue :: Size -> Value
sizeAsValue = coerce

combineOrSize :: Size -> Size -> Size
combineOrSize leftSize rightSize =
  leftSize + rightSize

combineAndSize :: Size -> Size -> Size
combineAndSize leftSize rightSize =
  leftSize * rightSize

shiftOrValue :: Size -> Size -> Value -> Value
shiftOrValue _leftSize rightSize leftValue =
  leftValue + (sizeAsValue rightSize)

combineAndValue :: Size -> Value -> Value -> Value
combineAndValue rightSize leftValue rightValue =
  leftValue * (sizeAsValue rightSize) + rightValue

splitAndValue :: Size -> Value -> (Value, Value)
splitAndValue rightSize combinedValue =
  combinedValue `divMod` (sizeAsValue rightSize)

listSize :: Size -> Size -> Size
listSize itemSize itemCount =
  itemSize ^ itemCount

externalCreateListRight :: Foldable f => Size -> f Value -> Value
externalCreateListRight itemSize =
  foldr
    (\itemValue previousValue ->
      combineAndValue
        itemSize
        previousValue
        itemValue
    )
    0

externalSplitList :: Size -> Size -> Value -> [Value]
externalSplitList itemSize itemCount combinedValue =
  externalEmbedFoldr (:) itemSize itemCount combinedValue []

getItemInList :: Size -> Value -> Value -> Value
getItemInList itemSize itemIndex combinedValue | itemSize == 0 =
  error ("getItemInList zero size: " <> show (itemSize, itemIndex, combinedValue))
getItemInList itemSize itemIndex combinedValue =
  let itemSizeValue = sizeAsValue itemSize
      shiftRight x = x `div` (itemSizeValue ^ itemIndex)
      isolateValue x = x `mod` itemSizeValue
  in (isolateValue . shiftRight) combinedValue

externalEmbedFoldr :: (Value -> b -> b) -> Size -> Size -> Value -> b -> b
externalEmbedFoldr _f itemSize itemCount combinedValue _initialValue | itemSize == 0 =
  error ("externalEmbedFoldr zero size: f " <> show (itemSize, itemCount, combinedValue) <> " b")
externalEmbedFoldr _f _itemSize itemCount _ initialValue | itemCount == 0 = initialValue
externalEmbedFoldr f itemSize itemCount combinedValue initialValue =
  let (remainingValue, itemValue) =
        splitAndValue itemSize combinedValue
  in f itemValue (externalEmbedFoldr f itemSize (itemCount - 1) remainingValue initialValue)
