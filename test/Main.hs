{-# LANGUAGE OverloadedLists #-}

module Main where

import Ciphlaim.Integer
import Test.Sandwich

top :: TopSpec
top = do
  describe "createOr LowerIndexFirst" $ do
    let createOrLower = createOr LowerIndexFirst
    it "createOrLower [1] result 0" $ createOrLower OrRef {index=0, value=0} [1] `shouldBe` Right Fin { size=1, value=0 }
    it "createOrLower [2] result 0" $ createOrLower OrRef {index=0, value=0} [2] `shouldBe` Right Fin { size=2, value=0 }
    it "createOrLower [2] result 1" $ createOrLower OrRef {index=0, value=1} [2] `shouldBe` Right Fin { size=2, value=1 }
    it "createOrLower [2,3] result 0" $ createOrLower OrRef {index=0, value=0} [2,3] `shouldBe` Right Fin { size=5, value=0 }
    it "createOrLower [2,3] result 1" $ createOrLower OrRef {index=0, value=1} [2,3] `shouldBe` Right Fin { size=5, value=1 }
    it "createOrLower [2,3] result 2" $ createOrLower OrRef {index=1, value=0} [2,3] `shouldBe` Right Fin { size=5, value=2 }
    it "createOrLower [2,3] result 3" $ createOrLower OrRef {index=1, value=1} [2,3] `shouldBe` Right Fin { size=5, value=3 }
    it "createOrLower [2,3] result 4" $ createOrLower OrRef {index=1, value=2} [2,3] `shouldBe` Right Fin { size=5, value=4 }
    it "createOrLower [4,2,3] result 0" $ createOrLower OrRef {index=0, value=0} [4,2,3] `shouldBe` Right Fin { size=9, value=0 }
    it "createOrLower [4,2,3] result 1" $ createOrLower OrRef {index=0, value=1} [4,2,3] `shouldBe` Right Fin { size=9, value=1 }
    it "createOrLower [4,2,3] result 2" $ createOrLower OrRef {index=0, value=2} [4,2,3] `shouldBe` Right Fin { size=9, value=2 }
    it "createOrLower [4,2,3] result 3" $ createOrLower OrRef {index=0, value=3} [4,2,3] `shouldBe` Right Fin { size=9, value=3 }
    it "createOrLower [4,2,3] result 4" $ createOrLower OrRef {index=1, value=0} [4,2,3] `shouldBe` Right Fin { size=9, value=4 }
    it "createOrLower [4,2,3] result 5" $ createOrLower OrRef {index=1, value=1} [4,2,3] `shouldBe` Right Fin { size=9, value=5 }
    it "createOrLower [4,2,3] result 6" $ createOrLower OrRef {index=2, value=0} [4,2,3] `shouldBe` Right Fin { size=9, value=6 }
    it "createOrLower [4,2,3] result 7" $ createOrLower OrRef {index=2, value=1} [4,2,3] `shouldBe` Right Fin { size=9, value=7 }
    it "createOrLower [4,2,3] result 8" $ createOrLower OrRef {index=2, value=2} [4,2,3] `shouldBe` Right Fin { size=9, value=8 }

  describe "createOr HigherIndexFirst" $ do
    let createOrHigher = createOr HigherIndexFirst
    it "createOrHigher [2,3] result 0" $ createOrHigher OrRef {index=1, value=0} [2,3] `shouldBe` Right Fin { size=5, value=0 }
    it "createOrHigher [2,3] result 1" $ createOrHigher OrRef {index=1, value=1} [2,3] `shouldBe` Right Fin { size=5, value=1 }
    it "createOrHigher [2,3] result 2" $ createOrHigher OrRef {index=1, value=2} [2,3] `shouldBe` Right Fin { size=5, value=2 }
    it "createOrHigher [2,3] result 3" $ createOrHigher OrRef {index=0, value=0} [2,3] `shouldBe` Right Fin { size=5, value=3 }
    it "createOrHigher [2,3] result 4" $ createOrHigher OrRef {index=0, value=1} [2,3] `shouldBe` Right Fin { size=5, value=4 }
    it "createOrHigher [4,2,3] result 0" $ createOrHigher OrRef {index=2, value=0} [4,2,3] `shouldBe` Right Fin { size=9, value=0 }
    it "createOrHigher [4,2,3] result 1" $ createOrHigher OrRef {index=2, value=1} [4,2,3] `shouldBe` Right Fin { size=9, value=1 }
    it "createOrHigher [4,2,3] result 2" $ createOrHigher OrRef {index=2, value=2} [4,2,3] `shouldBe` Right Fin { size=9, value=2 }
    it "createOrHigher [4,2,3] result 3" $ createOrHigher OrRef {index=1, value=0} [4,2,3] `shouldBe` Right Fin { size=9, value=3 }
    it "createOrHigher [4,2,3] result 4" $ createOrHigher OrRef {index=1, value=1} [4,2,3] `shouldBe` Right Fin { size=9, value=4 }
    it "createOrHigher [4,2,3] result 5" $ createOrHigher OrRef {index=0, value=0} [4,2,3] `shouldBe` Right Fin { size=9, value=5 }
    it "createOrHigher [4,2,3] result 6" $ createOrHigher OrRef {index=0, value=1} [4,2,3] `shouldBe` Right Fin { size=9, value=6 }
    it "createOrHigher [4,2,3] result 7" $ createOrHigher OrRef {index=0, value=2} [4,2,3] `shouldBe` Right Fin { size=9, value=7 }
    it "createOrHigher [4,2,3] result 8" $ createOrHigher OrRef {index=0, value=3} [4,2,3] `shouldBe` Right Fin { size=9, value=8 }

  describe "splitOr LowerIndexFirst" $ do
    let splitOrLower = splitOr LowerIndexFirst
    it "splitOrLower 0 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=0 } `shouldBe` Right OrRef {index=0, value=0}
    it "splitOrLower 1 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=1 } `shouldBe` Right OrRef {index=0, value=1}
    it "splitOrLower 2 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=2 } `shouldBe` Right OrRef {index=0, value=2}
    it "splitOrLower 3 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=3 } `shouldBe` Right OrRef {index=0, value=3}
    it "splitOrLower 4 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=4 } `shouldBe` Right OrRef {index=1, value=0}
    it "splitOrLower 5 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=5 } `shouldBe` Right OrRef {index=1, value=1}
    it "splitOrLower 6 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=6 } `shouldBe` Right OrRef {index=2, value=0}
    it "splitOrLower 7 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=7 } `shouldBe` Right OrRef {index=2, value=1}
    it "splitOrLower 8 [4,2,3]" $ splitOrLower [4,2,3] Fin { size=9, value=8 } `shouldBe` Right OrRef {index=2, value=2}

  describe "splitOr HigherIndexFirst" $ do
    let splitOrHigher = splitOr HigherIndexFirst
    it "splitOrHigher 0 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=0 } `shouldBe` Right OrRef {index=2, value=0}
    it "splitOrHigher 1 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=1 } `shouldBe` Right OrRef {index=2, value=1}
    it "splitOrHigher 2 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=2 } `shouldBe` Right OrRef {index=2, value=2}
    it "splitOrHigher 3 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=3 } `shouldBe` Right OrRef {index=1, value=0}
    it "splitOrHigher 4 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=4 } `shouldBe` Right OrRef {index=1, value=1}
    it "splitOrHigher 5 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=5 } `shouldBe` Right OrRef {index=0, value=0}
    it "splitOrHigher 6 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=6 } `shouldBe` Right OrRef {index=0, value=1}
    it "splitOrHigher 7 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=7 } `shouldBe` Right OrRef {index=0, value=2}
    it "splitOrHigher 8 [4,2,3]" $ splitOrHigher [4,2,3] Fin { size=9, value=8 } `shouldBe` Right OrRef {index=0, value=3}

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions top
