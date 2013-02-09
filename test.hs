module Main where

import Test.QuickCheck (quickCheck)
import Data.BetweenableTests as B (test)

main = quickCheck B.test
