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

externalCreateListLeft :: Foldable f => Size -> f Value -> Value
externalCreateListLeft itemSize =
  foldr
    (\itemValue previousValue ->
      combineAndValue
        (error "externalCreateListLeft: left size should not be used") itemSize
        previousValue itemValue
    )
    0

externalSplitList :: Size -> Size -> Value -> [Value]
externalSplitList itemSize itemCount combinedValue =
  if itemCount == 0
    then []
    else
      let (remainingValue, itemValue) = splitAndValue (error "externalSplitList: leftSize not used") itemSize combinedValue
      in itemValue : externalSplitList itemSize (itemCount - 1) remainingValue
