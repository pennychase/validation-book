{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Data.Char
import Data.Coerce
import Data.Validation

-------------------------------------------------------------------------------
-- Chapter 11
-- Generalizing
-- Use validation, the fold for Validation, to refactor display (defined as display')
-- Use the Optics in the Validation library: Prism and Iso
-------------------------------------------------------------------------------

-- Types

-- Password
newtype Password = Password String
  deriving (Show, Eq)

-- Username
newtype Username = Username String
  deriving (Show, Eq)

-- User
data User = User Username Password
  deriving (Show, Eq)

-- Error
-- Go back to Error as a list of Strings and derive the Semigroup instance
newtype Error = Error [String]
  deriving (Show, Eq, Semigroup)

-- Rule
-- To add type annotations in order to coerce between Strings, Password, Usernames
type Rule a = (a -> Validation Error a)

-- Turn a string into an Error. Use this instead of the Error constructor
-- to make it easier to refactror different Error representations.
stringToError :: String -> Error
stringToError str = Error [str]


-- Check that the password length is valid
checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
  case checkLength 3 20 password of
    Failure (Error err) -> Failure (stringToError ("Your password " ++ (head err)))
    Success password -> Success (Password password)

-- Check that the username length is valid
checkUsernameLength :: String -> Validation Error Username
checkUsernameLength name =
  case checkLength 3 15 name of
    Failure (Error err)  -> Failure (stringToError ("Your username " ++ (head err)))
    Success name -> Success (Username name)

-- Refactor checking username and password length by using one function tp check bounds
-- Have a minimum as well as maximum length
checkLength :: Int -> Int -> String -> Validation Error String
checkLength min max str =
  case (length str < min) || (length str > max) of
    True -> Failure (stringToError errMsg)
    False -> Success str
  where
    errMsg = "must be between " ++ show min ++ " and " ++ show max ++ " characters"


-- Check that the string is composed only of alphnumeric characters
requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure (stringToError "Cannot contain white space or \
                  \special characters")
    True -> Success xs

-- Remove leading white space and reject empty strings
cleanWhiteSpace :: String -> Validation Error String
cleanWhiteSpace "" = Failure (stringToError "Cannot be empty")
cleanWhiteSpace (x:xs) =
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Success (x:xs)

-- validatePassword first strips leading white space from the input password,
-- possibly returing a transformed string. It then checks the other
-- password requirements. validatePassword returns an Validation Error Password, so
-- the last validation must return Validation Error Password (so at this point
-- the order matters)
-- Use Rule for the type annotations to make all functions use Username
validatePassword :: Rule Password
validatePassword pwd =
  case (coerce cleanWhiteSpace :: Rule Password) pwd of
    Failure err -> Failure err
    Success pwd' ->
      (coerce requireAlphaNum :: Rule Password) pwd'  *>
      (coerce checkPasswordLength :: Rule Password) pwd'

-- validateUsername validates usernames, first stripping white space and then
-- checking the otehr rwquirements. checkUsernameLength is the last function
-- applied because it rturns a Username when successful
-- Use Rule for the type annotations to make all functions use Username
validateUsername :: Rule Username
validateUsername name =
  case (coerce cleanWhiteSpace :: Rule Username) name of
    Failure err -> Failure err
    Success name' -> (coerce requireAlphaNum :: Rule Username) name' *>
                     (coerce checkUsernameLength :: Rule Username) name'

-- passwordErrors will provide the context for password errors
-- by accumulating the errors from the different validation functions
-- into a list and labeling it with the validation context
-- Use over to map over the _Failure prism and accumulate the errors

passwordErrors :: Password -> Validation Error Password
passwordErrors pwd =
  over _Failure (\err -> stringToError "Invalid password:" <> err)
                (validatePassword pwd)

-- usernameErrors will provide the context for username errors
-- by accumulating the errors from the different validation functions
-- into a list and labeling it with the validation context
-- Use over to map over the _Failure prism and accumulate the errors
usernameErrors :: Username -> Validation Error Username
usernameErrors name =
  over _Failure (\err -> stringToError "Invalid username:" <> err)
                (validateUsername name)
  
-- Constructing a User
-- Using Validate, the class of types that are isomorphic to Validation (e.g., Either),
-- so we can use Either or Validation:
-- makerUser @Either name pwd
-- makeUser @Validation name pwd
-- See display and display' for using type inference with makeUser
makeUser :: Validate v => Username -> Password -> v Error User
makeUser name pwd =
  review _Validation
    (User <$> usernameErrors name <*> passwordErrors pwd)

-- Construct a User with a default temporary password
makeUserTmpPassword :: Username -> Validation Error User
makeUserTmpPassword name =
  User <$> validateUsername name
       <*> pure (Password "tempPassword")

-- Display the result of creating a User: a welcome message
-- if successful, otherwise the error messages formated nicely
-- Using the generic makeUser with Validate Validation (type inferred from case)
display :: Username -> Password -> IO ()
display name pwd =
  case makeUser name pwd of
    Failure err -> putStrLn (unlines (coerce err))
    Success (User name pwd) -> putStrLn ("Welcome " ++ coerce @Username @String name)

-- Display the result of creating a User: a welcome message
-- if successful, otherwise the error messages formated nicely
-- Using the generic makeUser with Validate Either (type inferred from case)
display' :: Username -> Password -> IO ()
display' name pwd =
  case makeUser name pwd of
    Left err -> putStrLn (unlines (coerce err))
    Right (User name pwd) -> putStrLn ("Welcome " ++ coerce @Username @String name)


{--
-- Testing
-- Test results are represented as Either String ()
-- Left is the string describing the failure and Right does nothing so is ()

-- printTestResult takes a test result and prints it
printTestResult :: Either String () -> IO ()
printTestResult r =
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed"

-- eq checks the output of a function application against the expected
-- It takes three arguments: the test ID, the actual result, and the expected
-- result. It asserts that actual == result, and reports the result of
-- that assertion
eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  case (actual == expected) of
    True -> Right ()
    False -> Left (unlines
      [ "Test " ++ show n
      , "  Expected:  " ++ show expected
      , "  But got:  " ++ show actual
      ])
test :: IO ()
test = printTestResult $
  do
    eq 1 (checkPasswordLength "123") (Left (Error "Your password must be \
                \between 10 and 20 characters"))
    eq 2 (checkPasswordLength (replicate 25 'a')) (Left (Error "Your password must be \
                \between 10 and 20 characters"))
    eq 3 (requireAlphaNum "hellothere123") (Right "hellothere123")
    eq 4 (requireAlphaNum "hellothere!!!") (Left (Error "Cannot contain white \
                  \space or special characters"))
    eq 5 (checkPasswordLength "") (Right (Password "\"\""))-- To demonstrate failed test
--}

-- main
main :: IO ()
main = do
  putStr "Please enter a username\n"
  username <- Username <$> getLine
  putStr "Please enter a password\n"
  password <- Password <$> getLine
  display' username password
