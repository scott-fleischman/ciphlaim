module Ciphlaim.Perm where

import Ciphlaim.Fin
-- import Control.Lens.Operators
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

createPerm :: Vector Int -> Fin
createPerm table =
  let totalLength = fromIntegral @_ @Natural (Vector.length table)
      (valueSize, seen, result) =
        Vector.foldl' go (totalLength, 0, Fin {size=1, value=0}) table
  in 
    if valueSize /= 0
      then error $ "valueSize should be zero but was " <> show valueSize
      else
        if seen /= (2 ^ totalLength) - 1
          then error $ "not all bits set: " <> show seen
          else result
  where
    go :: (Natural, Natural, Fin) -> Int -> (Natural, Natural, Fin)
    go (valueSize, seen, Fin {size=sizeSoFar, value=valueSoFar}) currentValue =
      let compactCurrentValue = unsetBitsBeforeIndex currentValue seen
          newSeen = Bits.setBit seen currentValue
          resultSize = valueSize * sizeSoFar 
          resultValue = valueSize * valueSoFar + compactCurrentValue
      in (valueSize - 1, newSeen, Fin {size=resultSize, value=resultValue})

unsetBitsBeforeIndex :: Int -> Natural -> Natural
unsetBitsBeforeIndex index seen = go 0
  where
  go :: Int -> Natural
  go currentIndex =
    if currentIndex < index
      then
        if Bits.testBit seen currentIndex
          then 1 + (go (currentIndex + 1))
          else go (currentIndex + 1)
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
