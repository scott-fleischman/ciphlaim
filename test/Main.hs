module Main where

import AndTest
import Test.Hspec qualified as Hspec
import ListTest
import OrTest
import PermTest

main :: IO ()
main = Hspec.hspec do
  andTests
  orTests
  listTests
  orTests
  permTests
