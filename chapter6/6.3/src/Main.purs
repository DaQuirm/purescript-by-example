module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Picture
import Data.Hashable

{-
  Exercise 1
  (Easy) Use the showShape function from the previous chapter to define a Show
  instance for the Shape type.
-}

-- See Picture.purs

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ show $ Circle (Point { x: 5.0, y: 5.0 }) 4.0
