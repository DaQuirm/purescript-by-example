module Main where

import Prelude
import Data.Foldable
import Data.Array
import Control.Monad.Eff
import Control.Monad.Eff.Console

{-
  Exercise 1
  (Easy) Use foldl to test whether an array of boolean values are all true.
-}

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

{-
  Exercise 2
  (Medium) Characterize those arrays xs for which the function foldl (==) false xs
  returns true.

  Arrays ending with ...FTT...T
-}


{-
  Exercise 3
  (Medium) Rewrite the following function in tail recursive form using an
  accumulator parameter:

  count :: forall a. (a -> Boolean) -> Array a -> Int
  count _ [] = 0
  count p xs = if p (head x)
              then count p (tail xs) + 1
              else count p (tail xs)

-}

{-
  Exercise 4
  (Medium) Write reverse in terms of foldl
-}

foldlReverse :: forall a. Array a -> Array a
foldlReverse = foldl (\acc x -> [x] ++ acc) []

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  print $ allTrue [true, true, false]
  print $ allTrue [true, true, true, true]

  print $ foldl (==) false [true, true, true]
  print $ foldl (==) false [true, true, false, true, true]

  print $ foldlReverse [1, 2, 3, 5, 7, 8]
