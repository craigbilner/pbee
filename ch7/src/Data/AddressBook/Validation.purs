module Data.AddressBook.Validation where

import Prelude (Unit, (<>), pure, unit, (/=), show, (*>), (<$>), (<*>), ($))
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe)

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

notWhitespace :: Regex
notWhitespace = makeRegex "^[A-Za-z]+$"

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

isNotWhitespace :: String -> String -> V Errors String
isNotWhitespace field s = matches field notWhitespace s *> pure s

validateAddress :: Maybe Address -> V Errors (Maybe Address)
validateAddress = traverse validateAddress'
  where
    validateAddress' :: Address -> V Errors Address
    validateAddress' (Address o) = address <$> (isNotWhitespace "Street" o.street)
                                           <*> (isNotWhitespace "City" o.city)
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
  person <$> (isNotWhitespace "First Name" o.firstName)
         <*> (isNotWhitespace "Last Name"  o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
