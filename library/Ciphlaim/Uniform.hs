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

combineAndValue :: Size -> Size -> Value -> Value -> Value
combineAndValue _leftSize rightSize leftValue rightValue =
  leftValue * (sizeAsValue rightSize) + rightValue

splitAndValue :: Size -> Size -> Value -> (Value, Value)
splitAndValue _leftSize rightSize combinedValue =
  combinedValue `divMod` (sizeAsValue rightSize)

listSize :: Size -> Size -> Size
listSize itemSize itemCount =
  itemSize ^ itemCount

externalCreateListRight :: Foldable f => Size -> f Value -> Value
externalCreateListRight itemSize =
  foldr
    (\itemValue previousValue ->
      combineAndValue
        (error "externalCreateListRight: left size should not be used") itemSize
        previousValue itemValue
    )
    0

externalSplitList :: Size -> Size -> Value -> [Value]
externalSplitList _itemSize itemCount _combinedValue | itemCount == 0 = []
externalSplitList itemSize itemCount combinedValue =
  let (remainingValue, itemValue) =
        splitAndValue (error "externalSplitList: leftSize not used") itemSize combinedValue
  in itemValue : externalSplitList itemSize (itemCount - 1) remainingValue

getItemInList :: Size -> Value -> Value -> Value
getItemInList itemSize itemIndex combinedValue =
  let itemSizeValue = sizeAsValue itemSize
      shiftRight x = x `div` (itemSizeValue ^ itemIndex)
      isolateValue x = x `mod` itemSizeValue
  in (isolateValue . shiftRight) combinedValue
