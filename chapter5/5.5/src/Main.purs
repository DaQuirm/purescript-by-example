module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

{-
  Exercise 1
  (Easy) Write the factorial function using pattern matching.
  Hint. Consider the two cases zero and non-zero inputs.
-}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

{-
  Exercise 2
  (Medium) Look up Pascal’s Rule for computing binomial
  coefficients. Use it to write a function which computes binomial coefficients
  using pattern matching.
-}

bincoeff :: Int -> Int -> Int
bincoeff n 0 = 1
bincoeff n k | k > n = 0
bincoeff n k = bincoeff (n - 1) (k - 1) + bincoeff (n - 1) k

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ factorial 0
  print $ factorial 5

  print $ bincoeff 4 2
  print $ bincoeff 5 3
