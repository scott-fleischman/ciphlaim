module Main where

import AndTest
import Control.Monad (when)
import ListTest
import OrTest
import PermTest
import Test.Hspec qualified as Hspec
import UniformTest

main :: IO ()
main = Hspec.hspec do
  when False do
    andTests
    orTests
    listTests
    orTests
    permTests
  uniformTests
