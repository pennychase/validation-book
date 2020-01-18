{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Char
import Data.Validation

-------------------------------------------------------------------------------
-- Chapter 8
-- Refactored to use Validation
-- Changed Error to be a list of Strings in order to capture all the
-- possible errors, which also meant creating a Semigroup Validation instance.
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
-- Error needs a Semigroup instance, and since it's really the same as the
-- instance for List, we can use derving to create the instance
newtype Error = Error [String]
  deriving (Show, Eq, Semigroup)

-- Check that the password length is within the lower and upper bounds
checkPasswordLength :: String -> Validation Error Password
checkPasswordLength password =
  case (length password > 20) of
    True -> Failure (Error ["Your password cannot be longer than 20 characters"])
    False -> Success (Password password)

-- Check that the username length is within bounds
checkUsernameLength :: String -> Validation Error Username
checkUsernameLength name =
  case (length name > 15) of
    True -> Failure (Error ["Your username cannot be longer than 15 characters"])
    False -> Success (Username name)

-- Refactor checking username and password length
checkLength :: Int -> String -> Validation Error String
checkLength n str =
  case (length str > n) of
    True -> Failure (Error errMsg)
    False -> Success str
  where
    errMsg = ["Cannot be longer than " ++ show n ++ " characters"]

-- Check that the string is composed only of alphnumeric characters
requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure (Error ["Cannot contain white space or \
                  \special characters"])
    True -> Success xs

-- Remove leading white space and reject empty strings
cleanWhiteSpace :: String -> Validation Error String
cleanWhiteSpace "" = Failure (Error ["Cannot be empty"])
cleanWhiteSpace (x:xs) =
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Success (x:xs)

-- validatePassword first strips leading white space from the input password,
-- possibly returing a transformed string. It then checks the other
-- password requirements. validatePassword returns an Validation Error Password, so
-- the last validation must return Validation Error Password (so at this point
-- the order matters)
validatePassword :: Password -> Validation Error Password
validatePassword (Password pwd) =
  case (cleanWhiteSpace pwd) of
    Failure err -> Failure err
    Success pwd' -> requireAlphaNum pwd' *>
                    checkPasswordLength pwd'

-- validateUsername validates usernames, first stripping white space and then
-- checking the otehr rwquirements. checkUsernameLength is the last function
-- applied because it rturns a Username when successdul
validateUsername :: Username -> Validation Error Username
validateUsername (Username name) =
  case (cleanWhiteSpace name) of
    Failure err -> Failure err
    Success name' -> requireAlphaNum name' *>
                     checkUsernameLength name'

-- Constructing a User
makeUser :: Username -> Password -> Validation Error User
makeUser name pwd =
  User <$> validateUsername name
       <*> validatePassword pwd

-- Construct a User with a default temporary password
makeUserTmpPassword :: Username -> Validation Error User
makeUserTmpPassword name =
  User <$> validateUsername name
       <*> pure (Password "tempPassword")

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

-- main using bind
main' :: IO ()
main' =
  putStrLn "Please enter a password" >>
  (Password <$> getLine) >>=
    (print . validatePassword)
    -- can also do this with an explicit lambda: \pwd -> print (validatePassword pwd)

-- main
main = do
  putStr "Please enter a username\n"
  username <- Username <$> getLine
  putStr "Please enter a password\n"
  password <- Password <$> getLine
  print (makeUser username password)
