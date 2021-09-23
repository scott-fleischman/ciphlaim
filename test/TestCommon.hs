module TestCommon
  ( Hspec.it
  , Hspec.describe
  , Hspec.shouldBe
  , Hspec.Spec
  , Hspec.Expectations.expectationFailure
  , shouldBeRight
  )
  where

import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations qualified as Hspec.Expectations

shouldBeRight :: (Show e, Show r, Eq r) => Either e r -> r -> IO ()
shouldBeRight actual expected =
  case actual of
    Left e -> Hspec.Expectations.expectationFailure $ "shouldBeRight but was " <> show e
    Right r -> r `Hspec.shouldBe` expected
