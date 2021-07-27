module Main where

import Ciphlaim.Integer
import Test.Sandwich

top :: TopSpec
top = do
  describe "Or" $ do
    it "createOr [1] result 0" $ createOr 0 0 [1] `shouldBe` Fin { size=1, value=0 }
    it "createOr [2] result 0" $ createOr 0 0 [2] `shouldBe` Fin { size=2, value=0 }
    it "createOr [2] result 1" $ createOr 0 1 [2] `shouldBe` Fin { size=2, value=1 }
    it "createOr [2,3] result 0" $ createOr 1 0 [2,3] `shouldBe` Fin { size=5, value=0 }
    it "createOr [2,3] result 1" $ createOr 1 1 [2,3] `shouldBe` Fin { size=5, value=1 }
    it "createOr [2,3] result 2" $ createOr 1 2 [2,3] `shouldBe` Fin { size=5, value=2 }
    it "createOr [2,3] result 3" $ createOr 0 0 [2,3] `shouldBe` Fin { size=5, value=3 }
    it "createOr [2,3] result 4" $ createOr 0 1 [2,3] `shouldBe` Fin { size=5, value=4 }
    it "createOr [4,2,3] result 0" $ createOr 2 0 [4,2,3] `shouldBe` Fin { size=9, value=0 }
    it "createOr [4,2,3] result 1" $ createOr 2 1 [4,2,3] `shouldBe` Fin { size=9, value=1 }
    it "createOr [4,2,3] result 2" $ createOr 2 2 [4,2,3] `shouldBe` Fin { size=9, value=2 }
    it "createOr [4,2,3] result 3" $ createOr 1 0 [4,2,3] `shouldBe` Fin { size=9, value=3 }
    it "createOr [4,2,3] result 4" $ createOr 1 1 [4,2,3] `shouldBe` Fin { size=9, value=4 }
    it "createOr [4,2,3] result 5" $ createOr 0 0 [4,2,3] `shouldBe` Fin { size=9, value=5 }
    it "createOr [4,2,3] result 6" $ createOr 0 1 [4,2,3] `shouldBe` Fin { size=9, value=6 }
    it "createOr [4,2,3] result 7" $ createOr 0 2 [4,2,3] `shouldBe` Fin { size=9, value=7 }
    it "createOr [4,2,3] result 8" $ createOr 0 3 [4,2,3] `shouldBe` Fin { size=9, value=8 }
    it "splitOr 0 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=0 } `shouldBe` Right (2, 0)
    it "splitOr 1 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=1 } `shouldBe` Right (2, 1)
    it "splitOr 2 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=2 } `shouldBe` Right (2, 2)
    it "splitOr 3 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=3 } `shouldBe` Right (1, 0)
    it "splitOr 4 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=4 } `shouldBe` Right (1, 1)
    it "splitOr 5 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=5 } `shouldBe` Right (0, 0)
    it "splitOr 6 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=6 } `shouldBe` Right (0, 1)
    it "splitOr 7 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=7 } `shouldBe` Right (0, 2)
    it "splitOr 8 [4,2,3]" $ splitOr [4,2,3] Fin { size=9, value=8 } `shouldBe` Right (0, 3)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions top
