module Main where

import Data.Char

-- Check that the password length is within the lower and upper bounds
checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case (length password < 10 || length password > 20) of
    True -> Nothing
    False -> Just password

-- Check that the string is composed only of alphnumeric characters
requiredAlphaNum :: String -> Maybe String
requiredAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Nothing
    True -> Just xs

-- Remove leading white space and reject empty strings
cleanWhiteSpace :: String -> Maybe String
cleanWhiteSpace "" = Nothing
cleanWhiteSpace (x:xs) =
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Just (x:xs)

-- validatePassword first strips leading white space from the input password,
-- possibly returing a transformed string. It then checks the other
-- password requirements. checkPassword retruns a Maybe String, Nothing if any
-- of the functions fail, and a Just String is the password is valid.
validatePassword :: String -> Maybe String
validatePassword pwd =
  case (cleanWhiteSpace pwd) of
    Nothing -> Nothing
    Just (pwd') ->
      case (checkPasswordLength pwd') of
        Nothing -> Nothing
        Just (pwd'') -> requiredAlphaNum pwd''

main = do
  putStrLn "Please enter a password"
  password <- getLine
  print (validatePassword password)
