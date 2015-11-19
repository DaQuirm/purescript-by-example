module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.List
import Data.Maybe
import Data.AddressBook

{-
  Exercise 1
  (Easy) Test your understanding of the findEntry function   by
  writing down the types of each of its major subexpressions. For example, the
  type of the head function as used is specialized to List Entry -> Maybe Entry.

  filter :: forall a. (a -> Boolean) -> List a -> List a
  filterEntry :: Entry -> Boolean

  then with (a :: Entry) and applying partially:

  filter filterEntry :: List Entry -> List Entry
-}

{-
  Exercise 2
  (Medium) Write a function which looks up an Entry given a
  street address, by reusing the existing code in findEntry. Test your function in
  PSCi.
-}

findByStreet :: String -> AddressBook -> Maybe Entry
findByStreet street = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

{-
  Exercise 3
  (Medium) Write a function which tests whether a name appears
  in a AddressBook, returning a Boolean value. Hint: Use PSCi to find the type of
  the Data.List.null function, which test whether a list is empty or not.
-}

nameAppears :: String -> AddressBook -> Boolean
nameAppears name = filter filterEntry >>> null >>> not
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == name

{-
  Exercise 4
  (Difficult) Write a function removeDuplicates which removes
  duplicate address book entries with the same first and last names. Hint: Use
  PSCi to find the type of the Data.List.nubBy function, which removes duplicate
  elements from a list based on an equality predicate.
-}
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy compareEntries book
  where
  compareEntries :: Entry -> Entry -> Boolean
  compareEntries firstEntry secondEntry = do
    firstEntry.firstName == secondEntry.firstName &&
    firstEntry.lastName == secondEntry.lastName

main = do

  let address = { street: "123 Fake St.", city: "Faketown", state: "CA" }
  let entry = { firstName: "John", lastName: "Smith", address: address }
  let book = insertEntry entry emptyBook

  -- 1
  print $ showEntry <$> findEntry "John" "Smith" book
  print $ showEntry <$> findEntry "Jon" "Snow" book

  -- 2
  print $ showEntry <$> findByStreet "123 Fake St." book
  print $ showEntry <$> findByStreet "Erewhon" book

  -- 3
  print $ nameAppears "Jack" book
  print $ nameAppears "John" book

  -- 4
  let thickBook = { firstName: "John", lastName: "Smith", address: address }
                  : { firstName: "Jack", lastName: "Smith", address: address }
                  : { firstName: "John", lastName: "Smith", address: address }
                  : { firstName: "John", lastName: "Brown", address: address }
                  : emptyBook
  print $ length thickBook
  print $ length $ removeDuplicates thickBook
