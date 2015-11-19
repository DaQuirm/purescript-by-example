module Data.AddressBook where

import Prelude

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++
                  entry.firstName ++ ": " ++
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street ++ ", " ++
                   addr.city ++ ", " ++
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
