module Ciphlaim.Perm where

import Ciphlaim.And
import Ciphlaim.Fin
import Control.Monad.Trans.State.Strict qualified as State
import Data.Bits qualified as Bits
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Numeric.Natural (Natural)

splitPermComposed :: FinSize -> Fin -> Vector Int
splitPermComposed FinSize {size = itemSize} fin =
  let itemSizeInt :: Int
      itemSizeInt = fromIntegral itemSize
      sizes =
        Vector.generate
          itemSizeInt
          (\index -> FinSize {size = fromIntegral @Int @Natural (index + 1)})
      reverseCompact = splitAnd HighIndexMostSignificant sizes fin
      compact = Vector.reverse reverseCompact
  in State.evalState (traverse (stepSplitPerm itemSize) compact) 0

stepSplitPerm :: Natural -> Natural -> State.State Natural Int
stepSplitPerm elemSize compactIndex = do
  seen <- State.get
  let expandedIndex =
        indexForUnsetBitCount
          (fromIntegral @Natural @Int elemSize)
          (fromIntegral @Natural @Int compactIndex + 1)
          seen
      newSeen = Bits.setBit seen expandedIndex
  State.put newSeen
  pure expandedIndex

splitPerm :: FinSize -> Fin -> Vector Int
splitPerm elemSize@FinSize{size=elemSizeNat} fin =
  let reverseCompact :: Vector Natural
      reverseCompact = splitPermCompact elemSize fin
      leftToRightCompact = Vector.reverse reverseCompact
      result :: State.State Natural (Vector Int)
      result = traverse (stepSplitPerm elemSizeNat) leftToRightCompact
  in State.evalState result 0

splitPermCompact :: FinSize -> Fin -> Vector Natural
splitPermCompact FinSize {size=elemSize} Fin {value} =
  let makeCompact :: Int -> State.State (Natural, Natural) Natural
      makeCompact _ = do
        (currentIndex, currentValue) <- State.get
        let (divResult, modResult) = currentValue `divMod` currentIndex
        State.put (currentIndex + 1, divResult)
        pure modResult
      combined :: State.State (Natural, Natural) (Vector Natural)
      combined = Vector.generateM (fromIntegral @_ @Int elemSize) makeCompact
      compactIndexes :: Vector Natural
      compactIndexes = State.evalState combined (1, value)
  in compactIndexes

findIthUnsetBit :: Int -> Int -> Natural -> Int
findIthUnsetBit targetCount size seen = go 0 0
  where
  go :: Int -> Int -> Int
  go index count =
    if Bits.testBit seen index
      then
        let newIndex = index + 1
        in if newIndex < size
          then go (index + 1) count
          else error "findIthUnsetBit: targetCount not found"
      else
        let newIndex = index + 1
            newCount = count + 1
        in if newCount == targetCount
          then index
          else if newIndex < size
            then go (index + 1) (count + 1)
            else error "findIthUnsetBit: targetCount not found"

createPermFused :: DirectionSignificance -> Vector Int -> Fin
createPermFused dirSig input =
  let (result, _, _) =
        directedFold dir go (Fin{size=1, value=0}, fromIntegral @_ @Natural inputSize, 0) input
  in result
  where
  dir = significanceAsDirection dirSig
  inputSize = Vector.length input
  go :: (Fin, Natural, Natural) -> Int -> Int -> (Fin, Natural, Natural)
  go (Fin {size=previousSize, value=previousValue}, valueSize, seen) _index currentValue =
    let bitIndex =
          case dir of
            LeftToRight -> currentValue
            RightToLeft -> (inputSize - currentValue)
        compactCurrentValue =
          case dir of
            LeftToRight -> unsetBitsBeforeIndex currentValue seen
            RightToLeft -> unsetBitsBeforeIndex bitIndex seen -- unsetBitsAfterIndex inputSize currentValue seen
        newSeen = Bits.setBit seen bitIndex
        newResult =
          Fin
            { size = previousSize * valueSize
            , value = previousValue * valueSize + compactCurrentValue
            }
    in (newResult, valueSize - 1, newSeen)

createPermComposed :: DirectionSignificance -> Vector Int -> Fin
createPermComposed dir = createAnd dir . createPermVector

createPermVector :: Vector Int -> Vector Fin
createPermVector input =
  let mappedVector :: State.State (Natural, Natural) (Vector Fin)
      mappedVector = traverse stepCreatePerm input

      totalLength :: Natural
      totalLength = fromIntegral @_ @Natural (Vector.length input)

      (resultVector, (_finalValue, _finalSeen)) = State.runState mappedVector (totalLength, 0)
  in resultVector

stepCreatePerm :: Int -> State.State (Natural, Natural) Fin
stepCreatePerm currentValue = do
  (valueSize, seen) <- State.get
  let compactCurrentValue = unsetBitsBeforeIndex currentValue seen
      newSeen = Bits.setBit seen currentValue
  State.put (valueSize - 1, newSeen)
  pure Fin {size=valueSize, value=compactCurrentValue}

unsetBitsBeforeIndex :: Int -> Natural -> Natural
unsetBitsBeforeIndex index seen = go 0
  where
  go :: Int -> Natural
  go currentIndex =
    if currentIndex < index
      then
        if Bits.testBit seen currentIndex
          then go (currentIndex + 1)
          else 1 + (go (currentIndex + 1))
      else
        0

indexForUnsetBitCount :: Int -> Int -> Natural -> Int
indexForUnsetBitCount maxSize unsetBitCount seen = go (0, 0)
  where
  go :: (Int, Int) -> Int
  go (currentIndex, counted) =
    if currentIndex < maxSize
      then
        if Bits.testBit seen currentIndex
          then go (currentIndex + 1, counted)
          else
            let newCount = counted + 1
            in if newCount == unsetBitCount
                then currentIndex
                else go (currentIndex + 1, newCount)
      else
        error "indexForUnsetBitCount: ran past max index"
