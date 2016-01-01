module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Maybe

import Data.Picture

{-
  Exercise 1
  (Easy) Construct a value of type Shape which represents a circle centered at
  the origin with radius 10.0.
-}

circle :: Shape
circle = Circle center 10.0
  where
  center :: Point
  center = Point { x: 0.0, y: 0.0 }

{-
  Exercise 2
  (Medium) Write a function from Shapes to Shapes, which scales its argument by
  a factor of 2.0, center the origin.
-}

scale :: Shape -> Shape
scale (Circle c r) = Circle c (2.0 * r)
scale (Rectangle c w h) = Rectangle c (2.0 * w) (2.0 * h)
scale (Line start (Point { x = x, y = y })) = Line start (Point { x: 2.0 * x, y: 2.0 * y })
scale text@(Text _ _) = text

{-
  Exercise 3
  (Medium) Write a function which extracts the text from a Shape. It should
  return Maybe String, and use the Nothing constructor if the input is not
  constructed using Text.
-}

extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _ = Nothing


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ showShape circle

  print $ showShape $ scale $ Circle (Point { x: 5.0, y: 5.0 }) 4.0
  print $ showShape $ scale $ Rectangle (Point { x: 5.0, y: 5.0 }) 6.0 4.0
  print $ showShape $ scale $ Line (Point { x: 2.0, y: 2.0 }) (Point { x: 5.0, y: 6.0 })
  print $ showShape $ scale $ Text (Point { x: 5.0, y: 5.0 }) "cellar door"

  print $ extractText $ Rectangle (Point { x: 5.0, y: 5.0 }) 6.0 4.0
  print $ extractText $ Text (Point { x: 5.0, y: 5.0 }) "cellar door"

