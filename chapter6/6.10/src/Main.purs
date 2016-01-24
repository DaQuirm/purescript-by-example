module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Monoid
import Data.Array hiding (last)

{-
  Exercise 1
  (Medium) The Action class is a multi-parameter type class which defines an
  action of one type on another:
    class (Monoid m) <= Action m a where
      act :: m -> a -> a
  An action is a function which describes how a monoid can be used to modify a
  value of another type. We expect the action to respect the concatenation
  operator of the monoid. For example, the monoid of natural numbers with
  multiplication acts on strings by repeating a string some number of times:
    instance repeatAction :: Action Int String where
      act 0 _ = ""
      act n s = s ++ act (n - 1) s
  Note that act 2 s is equal to the combination act 1 s <> act 1 s, and
  1 <> 1 = 2 in the monoid of additive integers.
  Write down a reasonable set of laws which describe how the Action class should
  interact with the Monoid class.
  Hint: how do we expect mempty to act on elements? What about append?
-}

{-
  Exercise 2
  (Medium) Write an instance Action m a => Action m (Array a), where the action
  on arrays is defined by acting on the elements independently.
-}

class (Monoid m) <= Action m a where
  act :: m -> a -> a

instance arrayAction :: (Action m a) => Action m (Array a) where
  act m array = map (act m) array

{-
  Exercise 3
  (Difficult) Given the following newtype, write an instance for
  Action m (Self m), where the monoid m acts on itself using append:
    newtype Self m = Self m
-}

newtype Self m = Self m

instance selfAction :: (Monoid m) => Action m (Self m) where
  act _ (Self monoid) = Self (monoid <> monoid)

{-
  Exercise 4
  (Medium) Define a nullary type class Unsafe and use it to define a version of
  the unsafeIndex function from the Data.Array.Unsafe module, which uses your
  constraint to express its lack of type-safety. Use your function to define a
  function last which chooses the last element of an array, and which preserves
  the Unsafe constraint.
-}

class Unsafe

unsafeIndex :: forall a. (Unsafe) => Array a -> Int -> a
unsafeIndex = Data.Array.Unsafe.unsafeIndex

last :: forall a. (Unsafe) => Array a -> a
last array = unsafeIndex array ((length array) - 1)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print "!"
