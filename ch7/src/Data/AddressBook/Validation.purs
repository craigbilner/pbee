module Data.AddressBook.Validation where

import Prelude
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

makeRegex :: String -> Regex
makeRegex s =
  unsafePartial
    case regex s noFlags of
      Right r -> r

phoneNumberRegex :: Regex
phoneNumberRegex = makeRegex "^\\d{3}-\\d{3}-\\d{4}$"

stateRegex :: Regex
stateRegex = makeRegex "^[A-Z]{2}$"

whitespace :: Regex
whitespace = makeRegex "^\\s*$"

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

isWhitespace :: String -> String -> V Errors String
isWhitespace field s = matches field whitespace s *> pure s

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (isWhitespace "Street" o.street)
          <*> (isWhitespace "City" o.city)
          <*> validateState o.state

validateState :: String -> V Errors String
validateState state = (lengthIs "State" 2 state *> pure state)
                      *> (matches "State" stateRegex state *> pure state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (isWhitespace "First Name" o.firstName)
         <*> (isWhitespace "Last Name"  o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
