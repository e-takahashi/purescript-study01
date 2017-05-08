module Data.AddressBook.Validation where

import Prelude
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber, PhoneType(..))
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

--type Errors = Array String
data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneNumbersField
           | PhoneField PhoneType
data ValidationError = ValidationError String Field
type Errors = Array ValidationError

instance showField :: Show Field where
  show FirstNameField = "[First Name]"
  show LastNameField  = "[Last Name]"
  show StreetField    = "[Street]"
  show CityField      = "[City]"
  show StateField     = "[State]"
  show PhoneNumbersField   = "[Phone Numbers]"
  show (PhoneField pt) = "[Phone Number/" <> show pt <> "]"

--nonEmpty :: String -> String -> V Errors Unit
--nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty ::Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" <> show field <> "' cannot be empty") field]
nonEmpty _     _  = pure unit

--arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
--arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty :: forall a. Field -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid [ValidationError ("Field '" <> show field <> "' must contain at least one value") field]
arrayNonEmpty _     _  = pure unit

--lengthIs :: String -> Int -> String -> V Errors Unit
--lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs :: Field -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid [ValidationError ("Field '" <> show field <> "' must have length " <> show len) field]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: Field -> Regex -> String -> V Errors Unit
--matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
--matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]
matches field _     _     = invalid [ValidationError ("Field '" <> show field <> "' did not match the required format") field]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
{-
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> pure o.state)
-}
  address <$> (nonEmpty StreetField o.street *> pure o.street)
          <*> (nonEmpty CityField   o.city   *> pure o.city)
          <*> (lengthIs StateField 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
{--
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)
-}
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o."type") phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
{--
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)
--}
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty PhoneNumbersField o.phones *> traverse validatePhoneNumber o.phones)
         
validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
