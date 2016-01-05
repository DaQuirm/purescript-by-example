module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

{-
  Exercise 1
  (Easy) The following newtype represents a complex number:
   newtype Complex = Complex
     { real :: Number
     , imaginary :: Number
     }
  Define Show and Eq instances for Complex.
-}

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real ++ ", " ++ show imaginary ++ "i"

instance eqComplex :: Eq Complex where
  eq (Complex firstNumber) (Complex secondNumber) =
    eq firstNumber.real secondNumber.real && eq firstNumber.imaginary secondNumber.imaginary

{-
  Exercise 2
  (Medium) The following type defines a type of non-empty arrays of elements of
  type a:
   data NonEmpty a = NonEmpty a (Array a)
  Write a Semigroup instance for non-empty arrays by reusing the Semigroup
  instance for Array.
-}

data NonEmpty a = NonEmpty a (Array a)

instance semigroupNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a) where
  append (NonEmpty valueOne arrOne) (NonEmpty valueTwo arrTwo) =
    NonEmpty valueOne (append arrOne arrTwo)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty value array) = show value ++ show array

{-
  Exercise 3
  (Medium) Write a Functor instance for NonEmpty.
-}

instance functorNonEmpty :: Functor NonEmpty where
  map func (NonEmpty value array) = NonEmpty (func value) (map func array)

{-
  Exercise 4
  (Difficult) Write a Foldable instance for NonEmpty. Hint: reuse the Foldable
  instance for arrays.
-}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ show $ Complex { real: 3.0, imaginary: -2.5 }

  print $ Complex { real: 3.0, imaginary: -2.5 } == Complex { real: 3.0, imaginary: 2.5 }
  print $ Complex { real: 3.0, imaginary: 2.5 } == Complex { real: 3.0, imaginary: 2.5 }

  print $ map (\item -> item > 3) (NonEmpty 4 [1, 5, 3, 2, 7])
