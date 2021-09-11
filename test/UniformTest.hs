module UniformTest where

import Control.Lens ((^.))
import Control.Monad (forM_, when)
import Ciphlaim.Uniform
import Data.Generics.Labels ()
import Data.Either (isLeft)
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
    describe "or/and" do
      forM_ (makeSizeValuePairs 3 3) \input@(leftSize, rightSize, leftValue, rightValue) -> do
        it ("combineOrSize preserved with shiftOrValue: " <> show input) do
            let shiftedValue = shiftOrValue rightSize leftValue
                combinedSize = combineOrSize leftSize rightSize
            shiftedValue `shouldSatisfy` (< (sizeAsValue combinedSize))

        do
          let combinedValue = combineAndValue rightSize leftValue rightValue
              combinedSize = combineAndSize leftSize rightSize
          it ("combineAndSize preserved with combineAndValue: " <> show input) do
              combinedValue `shouldSatisfy` (< (sizeAsValue combinedSize))
          let (splitLeftValue, splitRightValue) = splitAndValue rightSize combinedValue
          it ("splitAndValue inverse of combineAndValue: " <> show input) do
              splitLeftValue `shouldBe` leftValue
              splitRightValue `shouldBe` rightValue

    describe "create/splitList" do
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

    describe "allValues" do
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

    describe "compose" do
      forM_
        [ (2, 2, 2, [0, 1], [0, 1], [0, 1])
        , (2, 2, 2, [1, 0], [1, 0], [0, 1])
        , (4, 4, 4, [0, 2, 1, 3], [1, 0, 3, 2], [1, 3, 0, 2])
        , (2, 3, 4, [1, 2], [3, 2, 1], [2, 1])
        ]
        \input@(size1, size2, size3, map1to2, map2to3, final) -> it ("compose: " <> show input) do
          let combinedMap1to2 = externalCreateListRight size2 map1to2
              combinedMap2to3 = externalCreateListRight size3 map2to3
              combinedFinal = externalCreateListRight size3 final
          compose size1 size2 size3 combinedMap1to2 combinedMap2to3 `shouldBe` combinedFinal

    describe "nested lists equivalent to flattened ones" do
      forM_
        [ (2, [[]])
        , (2, [[0],[1]])
        , (2, [[1],[0],[1],[0]])
        , (2, [[0,1],[1,1]])
        , (2, [[0,0,0],[1,1,1]])
        , (3, [[0,1,2,2,1], [0,1,2,0,0], [0,0,0,1,1]])
        ]
        \input@(itemSize :: Size, nested :: [[Value]]) -> it ("nested lists flattened: " <> show input) do
          let innerCombinedSize :: Size
              innerCombinedSize = listSize itemSize (fromIntegral @Int @Size $ length $ head nested)
          let makeInner :: [Value] -> Value
              makeInner items = externalCreateListRight itemSize items
          let inners :: [Value]
              inners = fmap makeInner nested
          let combinedOuter :: Value
              combinedOuter = externalCreateListRight innerCombinedSize inners
          let combinedFlattened :: Value
              combinedFlattened = externalCreateListRight itemSize ((concat nested) :: [Value])
          combinedOuter `shouldBe` combinedFlattened

    describe "SplitOr" do
      forM_
        [ (SplitOr [1] 0 0, Fin 1 0)
        , (SplitOr [1,1] 0 0, Fin 2 0)
        , (SplitOr [1,1] 1 0, Fin 2 1)
        , (SplitOr [0,1,0,1,0] 1 0, Fin 2 0) -- zero size can be added without changing the value
        , (SplitOr [0,1,0,1,0] 3 0, Fin 2 1)
        , (SplitOr [2] 0 0, Fin 2 0) -- value for sizes [2] is the same as for [1,1]. values can have multiple types
        , (SplitOr [2] 0 1, Fin 2 1)
        , (SplitOr [5,6,7] 0 0, Fin 18 0)
        , (SplitOr [5,6,7] 1 0, Fin 18 5) -- larger values come from deeper in the list
        , (SplitOr [5,6,7] 2 6, Fin 18 17)
        ]
        \input@(split :: SplitOr, combined :: Fin) ->
          it ("combineOr/splitOr: " <> show input) do
            combineOr split `shouldBe` Right combined
            splitOr (split ^. #sizes) combined `shouldBe` Right split
      forM_
        [ (SplitOr [1] 0 1) -- cannot have a value greater than the size at index
        , (SplitOr [2] 0 99)
        , (SplitOr [1] 1 0) -- cannot have an index greater than the size list
        , (SplitOr [1,2,3,4] 99 0)
        , (SplitOr [] 0 0) -- cannot create empty "or"
        , (SplitOr [0,1] 0 0) -- cannot have a value for zero size
        ]
        \input ->
          it ("combineOr error: " <> show input) do
            combineOr input `shouldSatisfy` isLeft
      forM_
        [ ([1], Fin 2 0) -- sizes don't match combined size
        , ([1,2,3], Fin 3 2)
        , ([1,2,3], Fin 6 99) -- invalid fin
        ]
        \input@(sizes :: [Size], fin :: Fin) ->
          it ("splitOr error: " <> show input) do
            splitOr sizes fin `shouldSatisfy` isLeft
    describe "SplitAnd" do
      forM_
        [ (SplitAnd [] [], Fin 1 0) -- there is a base case
        , (SplitAnd [1] [0], Fin 1 0) -- sizes of one do not affect result value
        , (SplitAnd [1,1,1] [0,0,0], Fin 1 0)
        , (SplitAnd [2] [0], Fin 2 0) -- single size preserves size, value
        , (SplitAnd [2] [1], Fin 2 1)
        , (SplitAnd [2,3] [0,0], Fin 6 0) -- head of list is least significant in result value
        , (SplitAnd [2,3] [1,0], Fin 6 1)
        , (SplitAnd [2,3] [0,1], Fin 6 2)
        , (SplitAnd [2,3] [1,1], Fin 6 3)
        , (SplitAnd [2,3] [0,2], Fin 6 4)
        , (SplitAnd [2,3] [1,2], Fin 6 5)
        , (SplitAnd [2,3,4] [0,0,0], Fin 24 0)
        , (SplitAnd [2,3,4] [1,2,3], Fin 24 23)
        , (SplitAnd [1,2,1,3,1,4] [0,0,0,0,0,0], Fin 24 0)
        , (SplitAnd [1,2,1,3,1,4] [0,1,0,2,0,3], Fin 24 23)
        ]
        \input@(split :: SplitAnd, combined :: Fin) ->
          it ("combineAnd/splitAnd: " <> show input) do
            combineAnd split `shouldBe` Right combined
            splitAnd (split ^. #sizes) combined `shouldBe` Right split
      forM_
        [ (SplitAnd [1,2] []) -- length of sizes and values do not match
        , (SplitAnd [] [1,2])
        , (SplitAnd [2,3,4] [5,0,0]) -- value exceeds size
        , (SplitAnd [2,3,4] [0,5,0])
        , (SplitAnd [2,3,4] [0,0,5])
        , (SplitAnd [0] [0]) -- cannot use zero
        ]
        \input ->
          it ("combineAnd error: " <> show input) do
            combineAnd input `shouldSatisfy` isLeft
