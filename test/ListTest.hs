{-# LANGUAGE OverloadedLists #-}

module ListTest where

import Ciphlaim.And
import Ciphlaim.List
import Ciphlaim.Fin
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import TestCommon

data ListAssoc = ListAssoc
  { fin :: Fin
  , size :: FinSize
  , values :: Vector Natural
  , dir :: DirectionSignificance
  }
  deriving stock (Generic, Eq, Show)

combine :: FinSize -> FinSize -> DirectionSignificance -> Vector (Natural, Vector Natural) -> Vector ListAssoc
combine FinSize {size = combinedSize} itemSize dir pairs =
  (\(value, values) -> ListAssoc Fin {size = combinedSize, value} itemSize values dir) <$> pairs

listAssocs :: Vector ListAssoc
listAssocs =
  combine 1 0 LowIndexMostSignificant
    [ (0, [])
    ]
  <>
  combine 1 1 LowIndexMostSignificant
    [ (0, [0])
    ]
  <>
  combine 2 2 LowIndexMostSignificant
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 4 2 LowIndexMostSignificant
    [ (0, [0,0])
    , (1, [0,1])
    , (2, [1,0])
    , (3, [1,1])
    ]
  <>
  combine 8 2 LowIndexMostSignificant
    [ (0, [0,0,0])
    , (1, [0,0,1])
    , (2, [0,1,0])
    , (3, [0,1,1])
    , (4, [1,0,0])
    , (5, [1,0,1])
    , (6, [1,1,0])
    , (7, [1,1,1])
    ]
  <>
  combine 9 3 LowIndexMostSignificant
    [ (0, [0,0])
    , (1, [0,1])
    , (2, [0,2])
    , (3, [1,0])
    , (4, [1,1])
    , (5, [1,2])
    , (6, [2,0])
    , (7, [2,1])
    , (8, [2,2])
    ]
  <>
  combine 1 0 HighIndexMostSignificant
    [ (0, [])
    ]
  <>
  combine 1 1 HighIndexMostSignificant
    [ (0, [0])
    ]
  <>
  combine 2 2 HighIndexMostSignificant
    [ (0, [0])
    , (1, [1])
    ]
  <>
  combine 4 2 HighIndexMostSignificant
    [ (0, [0,0])
    , (1, [1,0])
    , (2, [0,1])
    , (3, [1,1])
    ]
  <>
  combine 8 2 HighIndexMostSignificant
    [ (0, [0,0,0])
    , (1, [1,0,0])
    , (2, [0,1,0])
    , (3, [1,1,0])
    , (4, [0,0,1])
    , (5, [1,0,1])
    , (6, [0,1,1])
    , (7, [1,1,1])
    ]
  <>
  combine 9 3 HighIndexMostSignificant
    [ (0, [0,0])
    , (1, [1,0])
    , (2, [2,0])
    , (3, [0,1])
    , (4, [1,1])
    , (5, [2,1])
    , (6, [0,2])
    , (7, [1,2])
    , (8, [2,2])
    ]

applyEvenTests :: Spec
applyEvenTests = describe "applyEvenTests" do
  let tableValues = [1,0,1,0,1,0,1,0]
      inputSize = fromIntegral @Int @Natural (length tableValues)
      outputSize = 2
      table = createList LowIndexMostSignificant FinSize {size=outputSize} tableValues
      boolAsNat b = if b then 1 else 0
      test :: Natural -> Spec
      test value = it ("apply even" <> show value) do
        applyCalculateOutputSize table Fin {size = inputSize, value} `shouldBe`
          Fin {size = outputSize, value = boolAsNat (value `mod` 2 == 0)}
  mapM_ test ([0..7] :: [Natural])

applyLargerOutputTests :: Spec
applyLargerOutputTests = describe "applyLargerOutputTests" do
  let tableValues = [7, 100]
      outputSize = 1000
      inputSize = 2
      table = createList LowIndexMostSignificant FinSize {size=outputSize} tableValues
  it "applyLargerOutputTests 7" do
    applyCalculateOutputSize table Fin {size = inputSize, value = 0} `shouldBe` Fin {size=outputSize, value=7}
  it "applyLargerOutputTests 100" do
    applyCalculateOutputSize table Fin {size = inputSize, value = 1} `shouldBe` Fin {size=outputSize, value=100}
        
listTests :: Spec
listTests = do
  describe "createList" do
    Vector.forM_ listAssocs \listAssoc@ListAssoc {fin, size, values, dir} ->
      it ("createList " <> show listAssoc) do
        createList dir size values `shouldBe` fin
  describe "splitList" do
    Vector.forM_ listAssocs \listAssoc@ListAssoc {fin, size, values, dir} ->
      it ("splitList " <> show listAssoc) do
        splitList dir size fin `shouldBe` values
  applyEvenTests
  applyLargerOutputTests
  describe "findNthRoot" do
    let values :: Vector (Natural, Natural, Natural)
        values = [(100, 7, 100 ^ (7 :: Natural))]
    Vector.forM_
      values
      \(d, p, r) ->
        it ("findNthRoot " <> show p <> " " <> show r <> " == " <> show d) do
          findNthRoot p r `shouldBe` Just d
