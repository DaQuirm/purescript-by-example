module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Array
import Data.Foldable
import Data.Maybe
import Math hiding (log)

import Data.Path
import FileOperations

{-
  Exercise 1
  (Easy) Write a function onlyFiles which returns all files not directories) in
  all subdirectories of a directory.
-}

onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter (not <<< isDirectory)

{-
  Exercise 2
  (Medium) Write a fold to determine the largest and smallest files in the
  filesystem.
-}

largestFile :: Path -> String
largestFile =
  onlyFiles
    >>> foldl
      (\largest file ->
        if size file > size largest then
          file
        else
          largest
      ) root
    >>> filename

{-
  Exercise 3
  (Difficult) Write a function whereIs to search for a file by name. The function
  should return a value of type Maybe Path, indicating the directory containing
  the file, if it exists. It should behave as follows:

   > whereIs "/bin/ls"
   Just (/bin/)

   > whereIs "/bin/cat"
   Nothing

  Hint: Try to write this function as an array comprehension using do notation.
-}

whereIs :: String -> Maybe Path
whereIs name = head $ filter (\file -> filename file == name) $ onlyFiles root

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  print $ allFiles root
  print $ onlyFiles root
  print $ largestFile root
  print $ whereIs "/bin/ls"
  print $ whereIs "/bin/cat"

