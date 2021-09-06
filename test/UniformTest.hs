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

makeAllLists :: Size -> Size -> [[Value]]
makeAllLists itemSize itemCount | itemSize == 0 || itemCount == 0 = [[]]
makeAllLists itemSize itemCount =
  let items = [0..(sizeAsValue itemSize)-1]
      smallerLists = makeAllLists itemSize (itemCount - 1)
  in concatMap
    (\item -> fmap (\list -> item : list) smallerLists)
    items

uniformTests :: Spec
uniformTests = do
  describe "uniformTests" do
    forM_ (makeSizeValuePairs 3 3) \input@(leftSize, rightSize, leftValue, rightValue) -> do
      it ("combineOrSize preserved with shiftOrValue: " <> show input) do
          let shiftedValue = shiftOrValue leftSize rightSize leftValue
              combinedSize = combineOrSize leftSize rightSize
          shiftedValue `shouldSatisfy` (< (sizeAsValue combinedSize))

      -- 'and' checks
      do
        let combinedValue = combineAndValue rightSize leftValue rightValue
            combinedSize = combineAndSize leftSize rightSize
        it ("combineAndSize preserved with combineAndValue: " <> show input) do
            combinedValue `shouldSatisfy` (< (sizeAsValue combinedSize))
        let (splitLeftValue, splitRightValue) = splitAndValue rightSize combinedValue
        it ("splitAndValue inverse of combineAndValue: " <> show input) do
            splitLeftValue `shouldBe` leftValue
            splitRightValue `shouldBe` rightValue

    forM_ [1..4] \itemSize -> do
      forM_ [0..3] \itemCount -> do
        forM_ (makeAllLists itemSize itemCount) \list -> do
          let combinedValue = externalCreateListRight itemSize list
          it ("create/splitList " <> show (itemSize, itemCount, list, combinedValue)) do
            let combinedSize = listSize itemSize itemCount
            combinedValue `shouldSatisfy` (< sizeAsValue combinedSize)

            let splitValues = externalSplitList itemSize itemCount combinedValue
            splitValues `shouldBe` list

            fmap
              (\index -> getItemInList itemSize index combinedValue)
              (if itemCount == 0 then [] else [0..(sizeAsValue itemCount)-1])
              `shouldBe`
              list

    forM_ [1..10] \itemSize -> do
      let allValuesValue = allValues itemSize
      it ("allValues " <> show (itemSize, allValuesValue)) do
        let nums = externalAllValues itemSize
        externalSplitList itemSize itemSize allValuesValue `shouldBe` nums
      it ("mapTableFunction/allValues is identity " <> show (itemSize, allValuesValue)) do
        mapTableFunction itemSize itemSize allValuesValue allValuesValue `shouldBe` allValuesValue
