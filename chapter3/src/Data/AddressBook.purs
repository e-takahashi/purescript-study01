module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe(..))

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

head' :: AddressBook -> Maybe Entry
head' Nil = Nothing
head' (Cons e es) = Just e

findEntry :: String -> String -> AddressBook -> Maybe Entry
--findEntry firstName lastName = head <<< filter filterEntry
findEntry firstName lastName = head' <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntry2 :: String -> AddressBook -> Maybe Entry
findEntry2 streetName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == streetName

nameExists :: String -> String -> AddressBook -> Boolean
nameExists firstName lastName = null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: Entry -> AddressBook -> AddressBook
removeDuplicates = (<<<)(nubBy eq) <<< Cons
  where
    eq :: Entry -> Entry -> Boolean
    eq x y = x.firstName == y.firstName && x.lastName == y.lastName
    

