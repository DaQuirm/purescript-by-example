module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.List
import Data.Foldable

{-
  Exercise 1
  (Easy) Write an Eq instance for the type NonEmpty a which reuses the instances
  for Eq a and Eq (Array a).
-}

data NonEmpty a = NonEmpty a (Array a)

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

data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite (Finite _) = false
  eq (Finite _) Infinite = false
  eq (Finite one) (Finite two) = eq one two

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite one) (Finite two) = compare one two

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

-- data OneMore f a = OneMore a (f a)
--
-- instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let neOne = NonEmpty 0 [1, 2, 3]
  let neTwo = NonEmpty 0 [1, 2, 4]
  let neThree = NonEmpty 1 [1, 2, 3]
  let neFour = NonEmpty 0 [1, 2, 3]
  print (neOne == neTwo)
  print (neOne == neThree)
  print (neOne == neFour)

  print (Infinite == (Finite 0))
  print $ compare Infinite (Finite 10)
