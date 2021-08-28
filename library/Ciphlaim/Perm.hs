module Ciphlaim.Perm where

import Ciphlaim.And
import Ciphlaim.Fin
-- import Control.Lens.Operators
import Control.Monad.Trans.State.Strict qualified as State
import Data.Bits qualified as Bits
import Data.Generics.Labels ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Numeric.Natural (Natural)

-- splitPerm :: Fin -> Vector Natural
-- splitPerm Fin {size, value} =
--   let seen :: Natural
--       seen = 0
--   in _

createPerm :: DirectionSignificance -> Vector Int -> Fin
createPerm dir = createAnd dir . createPermVector

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

-- Could use the cpu instructions bsr/bsf
-- firstSetBit :: Int -> Natural -> Maybe Int
-- firstSetBit size seen = go 0
--   where
--   go :: Int -> Maybe Int
--   go bitIndex =
--     if bitIndex < size
--       then
--         if Bits.testBit seen n
--           then Just n
--           else go (n + 1)
--       else
--         Nothing
