module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

type Address = { street :: String, city :: String }
type Person  = { name :: String, address :: Address }

{-
  Exercise 1
  (Easy) Write a function sameCity which uses record patterns to test whether
  two Person records belong to the same city.
-}

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: firstCity } } { address: { city: secondCity } } =
  firstCity == secondCity

{-
  Exercise 2
  (Medium) What is the most general type of the sameCity function, taking into
  account row polymorphism? What about the livesInLA function defined above?
-}
{-
  sameCity :: forall r. { address :: Address | r } -> { address :: Address | r } -> Boolean
  livesInLA :: forall r. { address :: Address } -> Boolean
-}

{-
  Exercise 3
  (Medium) Write a function fromSingleton which uses an array literal pattern to
  extract the sole member of a singleton array. If the array is not a singleton,
  your function should return a provided default value. Your function should
  have type forall a. a -> Array a -> a
-}

fromSingleton :: forall a. a -> Array a -> a
fromSingleton default [single] = single
fromSingleton default _ = default

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let personOne = { name: "Caesar", address: { city: "Rome", street: "Via dei Serpenti" } }
  let personTwo = { name: "John Paul II", address: { city: "Vatican City", street: ""  } }
  let personThree = { name: "Francis", address: { city: "Vatican City", street: "" } }
  print $ sameCity personOne personTwo
  print $ sameCity personTwo personThree

  print $ fromSingleton 10 [1, 2]
  print $ fromSingleton [1] [[2], [3]]
  print $ fromSingleton [1] [[10]]
