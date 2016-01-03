module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Math (pi, pow)

import Data.Picture

{-
  Exercise 1
  (Medium) Extend the vector graphics library with a new operation area which
  computes the area of a Shape. For the purposes of this exercise, the area of a
  piece of text is assumed to be zero.
-}

area :: Shape -> Number
area (Circle c r) = pi * r `pow` 2.0
area (Rectangle c w h) = w * h
area (Line start end) = 0.0
area text@(Text _ _) = 0.0

{-
  Exercise 2
  (Difficult) Extend the Shape type with a new data constructor Clipped, which
  clips another Picture to a rectangle. Extend the shapeBounds function to
  compute the bounds of a clipped picture. Note that this makes Shape into a
  recursive data type.
-}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ area $ Circle (Point { x: 5.0, y: 5.0 }) 4.0
  print $ area $ Rectangle (Point { x: 5.0, y: 5.0 }) 6.0 4.0
  print $ area $ Line (Point { x: 2.0, y: 2.0 }) (Point { x: 5.0, y: 6.0 })
  print $ area $ Text (Point { x: 5.0, y: 5.0 }) "cellar door"

