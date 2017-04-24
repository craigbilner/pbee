module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
findEntry pred = head <<< (filter pred)

filterByName :: String -> String -> Entry -> Boolean
filterByName firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

findByName :: String -> String -> AddressBook -> Maybe Entry
findByName firstName lastName = findEntry $ filterByName firstName lastName

filterByStreet :: String -> Entry -> Boolean
filterByStreet street entry = entry.address.street == street

findByStreet :: String -> AddressBook -> Maybe Entry
findByStreet street = findEntry $ filterByStreet street

printEntry :: Maybe Entry -> Maybe String
printEntry entry = map showEntry entry
