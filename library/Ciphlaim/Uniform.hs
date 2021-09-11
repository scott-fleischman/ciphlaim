module Ciphlaim.Uniform where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Coerce (coerce)
import Data.Either qualified as Either
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (lookup)

newtype Size = Size { size :: Natural }
  deriving stock (Generic, Eq)
  deriving newtype (Enum, Integral, Num, Ord, Real, Show)

newtype Value = Value { value :: Natural }
  deriving stock (Generic, Eq)
  deriving newtype (Enum, Integral, Num, Ord, Real, Show)

data Fin = Fin { size :: Size, value :: Value }
  deriving stock (Generic, Eq, Show)

data SplitOr = SplitOr { sizes :: [Size], valueIndex :: Int, splitValue :: Value }
  deriving stock (Generic, Eq, Show)

data SplitAnd = SplitAnd { sizes :: [Size], values :: [Value] }
  deriving stock (Generic, Eq, Show)

data SplitList = SplitList { itemSize :: Size, values :: [Value] }
  deriving stock (Generic, Eq, Show)

data Error
  = Error_Generic String
  | Error_Invalid_Fin String Fin
  | Error_Invalid_SplitOr String SplitOr
  | Error_Invalid_SplitAnd String SplitAnd [(Int, Size, Value)]
  | Error_SplitOr_FailedPrecondition String [Size] Fin
  | Error_SplitAnd_FailedPrecondition String [Size] Fin
  | Error_SplitList_FailedPrecondition String Size Fin Int
  | Error_CombineOr_FailedPostcondition String SplitOr Size Fin
  | Error_CombineAnd_FailedPostcondition String SplitAnd Size Fin
  deriving stock (Generic, Eq, Show)

sizeAsValue :: Size -> Value
sizeAsValue = coerce

combineOrSize :: Size -> Size -> Size
combineOrSize leftSize rightSize =
  leftSize + rightSize

validateFin :: Fin -> Either Error Fin
validateFin input@Fin {size, value} = do
  if (value < sizeAsValue size)
    then Right input
    else Left (Error_Invalid_Fin "value does not match size" input)

validateSplitOr :: SplitOr -> Either Error SplitOr
validateSplitOr input@SplitOr {sizes, valueIndex, splitValue} = do
  size <- 
    case drop valueIndex sizes of
      [] -> Left (Error_Invalid_SplitOr "valueIndex greater than sizes count" input)
      size : _ -> Right size
  unless (splitValue < sizeAsValue size) do
    Left (Error_Invalid_SplitOr "value does not match size at index" input)
  Right input

validateSplitAnd :: SplitAnd -> Either Error SplitAnd
validateSplitAnd input@SplitAnd {sizes, values} = do
  let check (index, size, value) =
        if value < sizeAsValue size
          then Right ()
          else Left (index, size, value)
      errors = Either.lefts (check <$> zip3 [0..] sizes values)
  unless (length sizes == length values) do
    Left (Error_Invalid_SplitAnd "length of sizes does not match length of values" input errors)
  case errors of
    [] -> Right input
    _ : _ -> Left (Error_Invalid_SplitAnd "values do not match sizes" input errors)

splitListToSplitAnd :: SplitList -> SplitAnd
splitListToSplitAnd SplitList {itemSize, values}=
  SplitAnd {sizes=const itemSize <$> values, values}

validateSplitList :: SplitList -> Either Error SplitList
validateSplitList input = do
  _ <- validateSplitAnd (splitListToSplitAnd input)
  Right input

combineAnd :: SplitAnd -> Either Error Fin
combineAnd input = do
  SplitAnd {sizes, values} <- validateSplitAnd input
  let step (itemSize, itemValue) Fin {size=previousSize, value=previousValue} =
        Fin
          { size = combineAndSize itemSize previousSize
          , value = combineAndValue itemSize previousValue itemValue
          }
      result = foldr step Fin {size=1, value=0} (zip sizes values)
      expectedSize = product sizes
  unless (result ^. #size == expectedSize) do
    Left (Error_CombineAnd_FailedPostcondition "result size does not match expected product of input sizes" input expectedSize result)
  validateFin result

splitAnd :: [Size] -> Fin -> Either Error SplitAnd
splitAnd inputSizes inputFin = do
  Fin {size=combinedSize, value=combinedValue} <- validateFin inputFin
  unless (product inputSizes == combinedSize) do
    Left (Error_SplitAnd_FailedPrecondition "combined size is not product of input sizes" inputSizes inputFin)
  let step itemSize = do
        remainingValue <- State.get
        let (newRunningValue, itemValue) = splitAndValue itemSize remainingValue
        State.put newRunningValue
        pure itemValue
      resultValues = State.evalState (traverse step inputSizes) combinedValue
  validateSplitAnd SplitAnd {sizes=inputSizes, values=resultValues}

combineOr :: SplitOr -> Either Error Fin
combineOr input = do
  SplitOr {sizes, valueIndex, splitValue} <- validateSplitOr input
  let step (index, size) Fin {size=previousSize, value=previousValue} =
        Fin
          { size = combineOrSize size previousSize
          , value =
              if index < valueIndex
                then shiftOrValue size previousValue
                else previousValue
          }
      initialFin = Fin {size=0, value=splitValue} -- initally invalid by size but becomes valid applying sizes
      result = foldr step initialFin (zip [0..] sizes)
      expectedSize = sum sizes
  unless (result ^. #size == expectedSize) do
    Left (Error_CombineOr_FailedPostcondition "expected result size to be sum of sizes" input expectedSize result)
  validateFin result

splitOr :: [Size] -> Fin -> Either Error SplitOr
splitOr splitSizes input = do
  Fin {size = combinedSize, value = combinedValue} <- validateFin input
  unless (sum splitSizes == combinedSize) do
    Left (Error_SplitOr_FailedPrecondition "splitSizes do not match combined size" splitSizes input)
  let go sizes value index =
        case sizes of
          [] -> Left (Error_SplitOr_FailedPrecondition "input value does not match splitSizes" splitSizes input)
          size : remainingSizes ->
            if value < sizeAsValue size
              then Right (value, index)
              else go remainingSizes (value - sizeAsValue size) (index + 1)
  (resultValue, resultIndex) <- go splitSizes combinedValue 0
  validateSplitOr SplitOr {sizes=splitSizes, valueIndex=resultIndex, splitValue=resultValue}

combineAndSize :: Size -> Size -> Size
combineAndSize leftSize rightSize =
  leftSize * rightSize

shiftOrValue :: Size -> Value -> Value
shiftOrValue rightSize leftValue =
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

combineList :: SplitList -> Either Error Fin
combineList input = combineAnd (splitListToSplitAnd input)

splitList :: Size -> Fin -> Either Error SplitList
splitList itemSize combinedInput = do
  _ <- validateFin combinedInput
  let go index Fin {size=currentSize, value=currentValue} = do
        let (remainingValue, itemValue) = splitAndValue itemSize currentValue
        let itemFin = Fin {size=itemSize, value=itemValue}
        unless (Either.isRight $ validateFin itemFin) do
          Left (Error_SplitList_FailedPrecondition ("item value at index exceeds size: " <> show itemValue) itemSize combinedInput index)
        let (remainingSize, sizeMod) = currentSize `divMod` itemSize
        unless (sizeMod == 0) do
          Left (Error_SplitList_FailedPrecondition "size not divisible by item size at index" itemSize combinedInput index)
        let nextFin = Fin {size=remainingSize, value=remainingValue}
        unless (Either.isRight $ validateFin nextFin) do
          Left (Error_SplitList_FailedPrecondition ("remaining value at index not valid fin: " <> show nextFin) itemSize combinedInput index)
        moreValues <-
          if remainingSize > 1
            then go (index + 1) nextFin
            else Right []
        Right (itemValue : moreValues)
  values <- go 0 combinedInput
  let result = SplitList {itemSize, values}
  validateSplitList result

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

-- also can be interpreted as:
-- apply itemSizeAsOutputSize itemIndexAsInputValue combinedValueAsTableFunction
getItemInList :: Size -> Value -> Value -> Value
getItemInList itemSize itemIndex combinedValue
  | itemSize == 0
  = error ("getItemInList zero size: " <> show (itemSize, itemIndex, combinedValue))
getItemInList itemSize itemIndex combinedValue =
  let itemSizeValue = sizeAsValue itemSize
      shiftRight x = x `div` (itemSizeValue ^ itemIndex)
      isolateValue x = x `mod` itemSizeValue
  in (isolateValue . shiftRight) combinedValue

externalEmbedFoldr :: (Value -> b -> b) -> Size -> Size -> Value -> b -> b
externalEmbedFoldr _f itemSize itemCount combinedValue _initialValue
  | itemSize == 0
  = error ("externalEmbedFoldr zero size: f " <> show (itemSize, itemCount, combinedValue) <> " b")
externalEmbedFoldr _f _itemSize itemCount _ initialValue
  | itemCount == 0
  = initialValue
externalEmbedFoldr f itemSize itemCount combinedValue initialValue =
  let (remainingValue, itemValue) =
        splitAndValue itemSize combinedValue
  in f itemValue (externalEmbedFoldr f itemSize (itemCount - 1) remainingValue initialValue)

-- Packs all values into a single list, combined into a single value.
-- Also can be thought of as the identity function (as a table) for the given item size as input size.
allValues :: Size -> Value
allValues size = externalCreateListRight size (externalAllValues size)

externalAllValues :: Size -> [Value]
externalAllValues size
  | size == 0
  = error "allValues 0"
externalAllValues size = [0..(sizeAsValue size)-1]

mapTableFunction :: Size -> Size -> Size -> Value -> Value -> Value
mapTableFunction itemSizeAsOutputSize inputItemSize inputItemCount listAsTableFunction combinedListOfValues =
  let lookup :: Value -> Value -> Value
      lookup currentItemAsIndex previousValue =
        let newResultItemValue = getItemInList itemSizeAsOutputSize currentItemAsIndex listAsTableFunction
        in combineAndValue itemSizeAsOutputSize previousValue newResultItemValue
  in externalEmbedFoldr lookup inputItemSize inputItemCount combinedListOfValues 0

compose :: Size -> Size -> Size -> Value -> Value -> Value
compose size1 size2 size3 map1to2 map2to3 =
  let initial = allValues size1
      intermediate = mapTableFunction size2 size1 size1 map1to2 initial
      final = mapTableFunction size3 size2 size1 map2to3 intermediate
  in final
