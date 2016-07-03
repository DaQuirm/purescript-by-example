module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))

{-
  Exercise 1
  (Easy) Use lift2 to write lifted versions of the numeric operators +, -, *
  and / which work with optional arguments.
-}

maybeAdd :: forall a. (Semiring a) => Maybe a -> Maybe a -> Maybe a
maybeAdd = lift2 (+)

maybeSub :: forall a. (Ring a) => Maybe a -> Maybe a -> Maybe a
maybeSub = lift2 (-)

maybeMul :: forall a. (Semiring a) => Maybe a -> Maybe a -> Maybe a
maybeMul = lift2 (*)

maybeDiv :: forall a. (EuclideanRing a) => Maybe a -> Maybe a -> Maybe a
maybeDiv = lift2 (/)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ maybeAdd (Just 3) (Just 7)
  logShow $ maybeSub (Just 3) (Just 7)
  logShow $ maybeMul (Just 3) (Just 7)
  logShow $ maybeDiv (Just 3.0) (Just 7.0)
