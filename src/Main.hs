module Main where

import Data.Char

-------------------------------------------------------------------------------
-- Chapter 5
-- Refactor to use Either monad in order to provide an indication of
-- the validation error (as a string) and to allow >>= to sequence
-- the validation checks.
-- Also includes some rudimentary testing.
-------------------------------------------------------------------------------

-- Check that the password length is within the lower and upper bounds
checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case (length password < 10 || length password > 20) of
    True -> Left "Your password must be between 10 and 20 characters"
    False -> Right password

-- Check that the string is composed only of alphnumeric characters
requiredAlphaNum :: String -> Either String String
requiredAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Left "Your password cannot contain white space or \
                  \special characters"
    True -> Right xs

-- Remove leading white space and reject empty strings
cleanWhiteSpace :: String -> Either String String
cleanWhiteSpace "" =     Left "Your password cannot be empty"
cleanWhiteSpace (x:xs) =
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Right (x:xs)

-- validatePassword first strips leading white space from the input password,
-- possibly returing a transformed string. It then checks the other
-- password requirements. checkPassword returns a Maybe String, Nothing if any
-- of the functions fail, and a Just String is the password is valid.
validatePassword :: String -> Either String String
validatePassword pwd =
  cleanWhiteSpace pwd
    >>= requiredAlphaNum
    >>= checkPasswordLength

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
    eq 1 (checkPasswordLength "123") (Left "Your password must be between 10 and 20 characters")
    eq 2 (checkPasswordLength (replicate 25 'a')) (Left "Your password must be between \
                  \10 and 20 characters")
    eq 3 (requiredAlphaNum "hellothere123") (Right "hellothere123")
    eq 4 (requiredAlphaNum "hellothere!!!") (Left "Your password cannot contain white space or \
                  \special characters")
    eq 5 (checkPasswordLength "") (Right "")

-- main
main = do
  putStr "Please enter a password\n"
  password <- getLine
  print (validatePassword password)
