module Main where

import Prelude
import Control.Monad.Eff.Console

import Math (sqrt)
import Control.MonadPlus (guard)

factorsPairs :: Int -> Array (Array Int)
factorsPairs n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i, j]

{-
  Exercise 1
	(Easy) Use the factors function to define a function isPrime which tests if its
	(integer argument is prime or not.
-}

isPrime :: Int -> Boolean
isPrime = factorsPairs >>> length >>> (== 1)

{-
  Exercise 2
  (Medium) Write a function which uses do notation to find the cartesian product
  of two arrays, i.e. the set of all pairs of elements a, b, where a is an
  element of the first array, and b is an element of the second.
-}

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  return [i, j]

{-
  Exercise 3
  (Medium) A Pythagorean triple is an array of numbers [a, b, c] such that a² + b²
  = c². Use the guard function in an array comprehension to write a function
  triples which takes a number n and calculates all Pythagorean triples whose
  components are less than n. Your function should have type Int -> Array (Array
  Int).
-}

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k
  return [i, j, k]

{-
  Exercise 4
  (Difficult) Write a function factorizations which produces all factorizations of
  an integer n, i.e. arrays of integers whose product is n. Hint: for an integer
  greater than 1, break the problem down into two subproblems: finding the first
  factor, and finding the remaining factors.
-}

factors :: Int -> Array Int
factors n = do
  i <- 2 .. sqrt n
  guard $ mod n i == 0
  return i

factorize :: Int -> Array (Array Int)
factorize 1 = [[]]
factorize n = do
  factor <- factors n
  factorization <- factorize $ n / factor
  return $ cons factor factorization

main = do
  print $ isPrime 24
  print $ isPrime 37
  print $ isPrime 15
  print $ cartesianProduct [1, 2] [3, 4, 5]
  print $ triples 50
  print $ factorize 24
  print $ factorize 37
  print $ factorize 225
