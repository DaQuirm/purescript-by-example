module Main where

import Math (pi)
import Prelude
import Control.Monad.Eff.Console

{-
	Exercise 1
	(Easy) Use the Math.pi constant to write a function circleArea which
	computes the area of a circle with a given radius. Test your function
	using PSCi (Hint: don’t forget to import Math.pi by modifying the import
	Math statement).
-}
circleArea radius = pi * radius * radius

main = do
  print (circleArea 10.0)
