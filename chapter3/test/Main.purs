module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (AddressBook, Entry, emptyBook, insertEntry, findEntry, showEntry, findEntry2, nameExists, removeDuplicates)
import Data.Maybe (Maybe)

example :: Entry
example =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

printEntry2 :: String -> AddressBook -> Maybe String
printEntry2 streetName book = showEntry <$> findEntry2 streetName book

main :: Eff (console :: CONSOLE) Unit
main = do
  let book1 = insertEntry example emptyBook

  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1
  logShow $ printEntry2 "123 Fake St." book0
  logShow $ printEntry2 "123 Fake St." book1
  logShow $ nameExists "John" "Smith" book0
  logShow $ nameExists "John" "Smith" book1
  logShow $ map showEntry $ removeDuplicates example book0
  logShow $ map showEntry $ removeDuplicates example book1
  
