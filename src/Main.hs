module Main where

import Data.Char

-------------------------------------------------------------------------------
-- Chapter 6
-- Use newtype to define types for Password, Username, and Error in order
-- to enable type safety for these different Strings. Modfied function to
-- use these types.
-- Wrote checkUsernameLength
-- Updated tests
-------------------------------------------------------------------------------

newtype Password = Password String
  deriving (Show, Eq)

newtype Error = Error String
  deriving (Show, Eq)

newtype Username = Username String
  deriving (Show, Eq)

-- Check that the password length is within the lower and upper bounds
checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case (length password > 20) of
    True -> Left (Error "Your password cannot be longer than 20 characters")
    False -> Right (Password password)

-- Check that the username length is within bounds
checkUsernameLength :: String -> Either Error Username
checkUsernameLength name =
  case (length name > 15) of
    True -> Left (Error "Your username cannot be longer than 15 characters")
    False -> Right (Username name)

-- Refactor checking username and password length
checkLength :: Int -> String -> Either Error String
checkLength n str =
  case (length str > n) of
    True -> Left (Error errMsg)
    False -> Right str
  where
    errMsg = "Cannot be longer than " ++ show n ++ " characters"

-- Check that the string is composed only of alphnumeric characters
requiredAlphaNum :: String -> Either Error String
requiredAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Left (Error "Cannot contain white space or \
                  \special characters")
    True -> Right xs

-- Remove leading white space and reject empty strings
cleanWhiteSpace :: String -> Either Error String
cleanWhiteSpace "" = Left (Error "Cannot be empty")
cleanWhiteSpace (x:xs) =
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Right (x:xs)

-- validatePassword first strips leading white space from the input password,
-- possibly returing a transformed string. It then checks the other
-- password requirements. validatePassword returns an EIther Error Password, so
-- the last validation est must return EIther Error Password (so at this point
-- the order matters)
validatePassword :: Password -> Either Error Password
validatePassword (Password pwd) =
  cleanWhiteSpace pwd
    >>= requiredAlphaNum
    >>= checkPasswordLength

-- Version using checkLength
validatePassword' :: Password -> Either Error Password
validatePassword' (Password pwd) =
  Password <$>
    (cleanWhiteSpace pwd
    >>= requiredAlphaNum
    >>= checkLength 20)

-- Version using do notation
validatePassword'' :: Password -> Either Error Password
validatePassword'' (Password pwd) =
  do
    pwd' <- cleanWhiteSpace pwd
    pwd'' <- requiredAlphaNum pwd'
    checkPasswordLength pwd''

-- validateUsername validates usernames, first stripping white space and then
-- checking the otehr rwquirements
validateUsername :: Username -> Either Error Username
validateUsername (Username name) =
  cleanWhiteSpace name
    >>= requiredAlphaNum
    >>= checkUsernameLength

-- Version using checkLength
validateUsername' :: Username -> Either Error Username
validateUsername' (Username name) =
  Username <$>
    (cleanWhiteSpace name
      >>= requiredAlphaNum
      >>= checkLength 15)

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
    eq 3 (requiredAlphaNum "hellothere123") (Right "hellothere123")
    eq 4 (requiredAlphaNum "hellothere!!!") (Left (Error "Cannot contain white \
                  \space or special characters"))
    eq 5 (checkPasswordLength "") (Right (Password "\"\""))-- To demonstrate failed test

-- main using bind
main' :: IO ()
main' =
  putStrLn "Please enter a password" >>
  (Password <$> getLine) >>=
      \pwd -> print (validatePassword pwd)

-- main
main = do
  putStr "Please enter a password\n"
  password <- Password <$> getLine
  print (validatePassword password)
