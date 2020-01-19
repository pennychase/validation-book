{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Validation

-------------------------------------------------------------------------------
-- Chapter 8 - Exercise 27
-- Refactored to use Text instead of T.Text
-------------------------------------------------------------------------------

-- Types

-- Password
newtype Password = Password T.Text
  deriving (Show, Eq)

-- Username
newtype Username = Username T.Text
  deriving (Show, Eq)

-- User
data User = User Username Password
  deriving (Show, Eq)

-- Error
-- Error needs a Semigroup instance, and since it's really the same as the
-- instance for List, we can use derving to create the instance
newtype Error = Error [T.Text]
  deriving (Show, Eq, Semigroup)

-- Check that the password length is within the lower and upper bounds
checkPasswordLength :: T.Text -> Validation Error Password
checkPasswordLength password =
  case (T.length password > 20) of
    True -> Failure (Error ["Your password cannot be longer than 20 characters"])
    False -> Success (Password password)

-- Check that the username length is within bounds
checkUsernameLength :: T.Text -> Validation Error Username
checkUsernameLength name =
  case (T.length name > 15) of
    True -> Failure (Error ["Your username cannot be longer than 15 characters"])
    False -> Success (Username name)

-- Refactor checking username and password length
checkLength :: Int -> T.Text -> Validation Error T.Text
checkLength n str =
  case (T.length str > n) of
    True -> Failure (Error errMsg)
    False -> Success str
  where
    errMsg = [T.pack("Cannot be longer than " ++ show n ++ " characters")]

-- Check that the string is composed only of alphnumeric characters
requireAlphaNum :: T.Text -> Validation Error T.Text
requireAlphaNum xs =
  case T.all isAlphaNum xs of
    False -> Failure (Error ["Cannot contain white space or \
                  \special characters"])
    True -> Success xs

-- Reject empty strings and remove leading/trailing white space
cleanWhiteSpace :: T.Text -> Validation Error T.Text
cleanWhiteSpace "" = Failure (Error ["Cannot be empty"])
cleanWhiteSpace str = Success (T.strip str)

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
  (Password <$> T.getLine) >>=
    (print . validatePassword)
    -- can also do this with an explicit lambda: \pwd -> print (validatePassword pwd)

-- main
main :: IO ()
main = do
  putStr "Please enter a username\n"
  username <- Username <$> T.getLine
  putStr "Please enter a password\n"
  password <- Password <$> T.getLine
  print (makeUser username password)
