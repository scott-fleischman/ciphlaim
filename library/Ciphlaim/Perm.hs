module Ciphlaim.Perm where

import Ciphlaim.And
import Ciphlaim.Fin
import Control.Monad.Trans.State.Strict qualified as State
import Data.Bits qualified as Bits
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Numeric.Natural (Natural)

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
      mappedVector = traverse go input

      totalLength :: Natural
      totalLength = fromIntegral @_ @Natural (Vector.length input)

      (resultVector, (_finalValue, _finalSeen)) = State.runState mappedVector (totalLength, 0)
  in resultVector
  where
  go :: Int -> State.State (Natural, Natural) Fin
  go currentValue = do
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

unsetBitsAfterIndex :: Int -> Int -> Natural -> Natural
unsetBitsAfterIndex size index seen = go (size - 1)
  where
  go :: Int -> Natural
  go currentIndex =
    if currentIndex < index && currentIndex >= 0
      then
        if Bits.testBit seen currentIndex
          then go (currentIndex - 1)
          else 1 + (go (currentIndex - 1))
      else
        0
