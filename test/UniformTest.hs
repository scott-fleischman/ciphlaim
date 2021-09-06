module UniformTest where

import Control.Monad (forM_)
import Ciphlaim.Uniform
import TestCommon
import Test.Hspec (shouldSatisfy)

makeValuePairs :: Size -> Size -> [(Value, Value)]
makeValuePairs leftSize rightSize | leftSize == 0 || rightSize == 0 = []
makeValuePairs leftSize rightSize =
  [ (leftValue, rightValue)
  | leftValue <- [0..(sizeAsValue leftSize)-1]
  , rightValue <- [0..(sizeAsValue rightSize)-1]
  ]

makeSizeValuePairs :: Size -> Size -> [(Size, Size, Value, Value)]
makeSizeValuePairs leftMaxSize rightMaxSize =
  let sizes = 
        [ (leftSize, rightSize)
        | leftSize <- [0..leftMaxSize]
        , rightSize <- [0..rightMaxSize]
        ]
      step (leftSize, rightSize) =
        fmap
          (\(leftValue, rightValue) -> (leftSize, rightSize, leftValue, rightValue))
          (makeValuePairs leftSize rightSize)
  in concatMap step sizes

uniformTests :: Spec
uniformTests = do
  describe "uniformTests" do
    forM_ (makeSizeValuePairs 3 3) \input@(leftSize, rightSize, leftValue, _rightValue) -> do
      it ("combineOrSize preserved with shiftOrValue: " <> show input) do
          let shiftedValue = shiftOrValue leftSize rightSize leftValue
              combinedSize = combineOrSize leftSize rightSize
          shiftedValue `shouldSatisfy` (>= 0)
          shiftedValue `shouldSatisfy` (< (sizeAsValue combinedSize))
