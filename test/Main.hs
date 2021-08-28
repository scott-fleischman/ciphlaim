module Main where

import AndTest
import ListTest
import OrTest
import PermTest

main :: IO ()
main = do
  orTest
  andTest
  listTest
  permTest
