module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

data NonEmpty a = NonEmpty a (Array a)

{-
  Exercise 1
  (Easy) Write an Eq instance for the type NonEmpty a which reuses the instances
  for Eq a and Eq (Array a).
-}

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty firstValue firstArray) (NonEmpty secondValue secondArray) =
    firstValue == secondValue && firstArray == secondArray

{-
  Exercise 2
  (Medium) Given any type a with an instance of Ord, we can add a new “infinite”
  value which is greater than any other value:
    data Extended a = Finite a | Infinite
  Write an Ord instance for Extended a which reuses the Ord instance for a.
-}

{-
  Exercise 3
  (Difficult) Given an type constructor f which defines an ordered container
  (and so has a Foldable instance), we can create a new container type which
  includes an extra element at the front:

    data OneMore f a = OneMore a (f a)

  The container OneMore f is also has an ordering, where the new element comes
  before any element of f. Write a Foldable instance for OneMore f:

    instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
    ...
-}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let neOne = NonEmpty 0 [1, 2, 3]
  let neTwo = NonEmpty 0 [1, 2, 4]
  let neThree = NonEmpty 1 [1, 2, 3]
  let neFour = NonEmpty 0 [1, 2, 3]
  print (neOne == neTwo)
  print (neOne == neThree)
  print (neOne == neFour)
