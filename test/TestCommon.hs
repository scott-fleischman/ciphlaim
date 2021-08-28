module TestCommon
  ( (Hedgehog.===)
  , Hedgehog.Property
  , Hedgehog.PropertyName
  , vectorFor
  , makeTest
  )
  where

import Data.String (fromString)
import Data.Vector qualified as Vector
import Hedgehog qualified

vectorFor :: Vector.Vector a -> (a -> b) -> [b]
vectorFor vector perItem = Vector.toList $ flip Vector.map vector perItem

makeTest :: String -> Hedgehog.PropertyT IO () -> (Hedgehog.PropertyName, Hedgehog.Property)
makeTest nameString testAction =
  let name = fromString nameString
      test = Hedgehog.withTests 1 $ Hedgehog.property testAction
  in (name, test)
