module Main where

import Prelude
import Control.Monad.Eff.Console

import Data.Array

{-
  Exercise 1
  (Easy) Use the map or <$> function to write a function which calculates the
  squares of an array of numbers.
-}

squareAll :: Array Number -> Array Number
squareAll = (<$>) \x -> x * x

{-
  Exercise 2
  (Easy) Use the filter function to write a function which removes the negative
  numbers from an array of numbers.
-}

dropNegatives :: Array Number -> Array Number
dropNegatives = filter \x -> x > 0.0

{-
  Exercise 3
  (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to the
  previous question to use your new operator. Experiment with the precedence
  level and associativity of your operator in PSCi.
-}

(<$?>) = filter

dropNegatives2 :: Array Number -> Array Number
dropNegatives2 array = (\x -> x > 0.0) <$?> array

main = do
	print $ squareAll [1.0, 2.0, 3.3, -4.0]
	print $ dropNegatives [-3.5, 2.0, 3.3, -4.1, 0.0, -2.2, 1.0]
	print $ dropNegatives2 [-3.5, 2.0, 3.3, -4.1, 0.0, -2.2, 1.0]

