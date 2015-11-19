module Main where

import Math (sqrt)
import Prelude
import Control.Monad.Eff.Console

diagonal w h = sqrt (w * w + h * h)

main = print (diagonal 3.0 4.0)

