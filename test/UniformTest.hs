module UniformTest where

import Control.Monad (forM_, when)
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

    forM_ [1..16] \itemSize -> do
      let allValuesValue = allValues itemSize
      let nums = externalAllValues itemSize
      it ("allValues " <> show (itemSize, allValuesValue, nums)) do
        externalSplitList itemSize itemSize allValuesValue `shouldBe` nums
      it ("mapTableFunction/allValues is identity " <> show (itemSize, allValuesValue)) do
        mapTableFunction itemSize itemSize itemSize allValuesValue allValuesValue `shouldBe` allValuesValue

    describe "example table function" do
      forM_
        [ ( 2, []
          , [0, 4]
          , 5, []
          )
        , ( 2, [0]
          , [0, 4]
          , 5, [0]
          )
        , ( 2, [1]
          , [0, 4]
          , 5, [4]
          )
        , ( 2, [0,1]
          , [0, 4]
          , 5, [0,4]
          )
        , ( 2, [0,0,1,1,0,1]
          , [0, 4]
          , 5, [0,0,4,4,0,4]
          )
        , ( 2, [0,0,1,1,0,1]
          , [0]
          , 1, [0,0,0,0,0,0]
          )
        , ( 2, [0,0,1,1,0,1]
          , [0]
          , 1, [] -- mapping anything to size 1 collapses it to being equivalent to the empty list
          )
        ]
        \arg@(inputSize, inputItems, fnItems, outputSize, outputItems) -> it ("example table function: " <> show arg) do
          let inputItemCount = fromIntegral @Int @Size (length inputItems)
          let combinedInputItems = externalCreateListRight inputSize inputItems
          let fn = externalCreateListRight outputSize fnItems
          let actual = mapTableFunction outputSize inputSize inputItemCount fn combinedInputItems
          let expected = externalCreateListRight outputSize outputItems
          when (actual /= expected) do
            expectationFailure $ "example table function: " <> show (actual, fn, combinedInputItems, expected)
