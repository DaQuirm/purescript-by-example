module Main where

import Prelude
import Control.Monad.Eff.Console

import Data.Array.Unsafe

{-
  Exercise 1
  (Easy) Write a recursive function which returns true if and only if
  its input is an even integer.
-}

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven(n - 2)

{-
  Exercise 2
  (Medium) Write a recursive function which counts
  the number of even integers in an array. Hint: use the head function from
  Data.Array.Unsafe.
-}

evenCount :: Array Int -> Int
evenCount [] = 0
evenCount array =
	(boolToInt $ isEven $ head array) + (evenCount $ tail array)
	where
	boolToInt :: Boolean -> Int
	boolToInt true = 1
	boolToInt false = 0

main = do

  -- 1
  print $ isEven 37
  print $ isEven 48

  -- 2
  print $ evenCount [1, 2, 3, 4, 5, 6]
  print $ evenCount [1, 2, 3, 4, 5, 6, 7]
  print $ evenCount [4, 5, 80]

