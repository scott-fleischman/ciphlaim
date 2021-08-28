module Main where

import AndTest
import Control.Monad qualified as Monad
import Hedgehog qualified
import ListTest
import OrTest
import PermTest
import System.Exit qualified as Exit
import System.IO qualified as IO

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  pass <-
    Hedgehog.checkParallel $ Hedgehog.Group "ciphlaim"
      $ orTests
      <> andTests
      <> listTests
      <> permTests
  
  Monad.unless pass Exit.exitFailure
