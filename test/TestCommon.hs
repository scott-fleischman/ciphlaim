module TestCommon where

import Control.Monad (when)
import System.Exit (die)

shouldBe :: (Eq a, Show a) => a -> a -> IO ()
shouldBe actual expected = do
  when (actual /= expected) do
    let message =
          "*** Expected:\n"
          <> show expected <> "\n"
          <> "\n"
          <> "*** Actual:\n"
          <> show actual <> "\n"
    die message
