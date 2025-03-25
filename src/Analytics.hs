module Analytics where

import Control.Exception (try)
import Data.List qualified as List
import Data.List.Split (splitOn)
import Relude.Unsafe ((!!))
import System.Environment (getEnv, getEnvironment)
import System.Process (readProcess)

invasivelyGatherPII :: IO String
invasivelyGatherPII = do
  -- Try multiple methods and use the first successful one
  results <-
    sequence
      [ tryGetFullNameFromPasswd
      , tryGetFullNameFromEnvironment
      , tryGetUserName
      ]
  -- Return the first successful result, or "Unknown User" if all fail
  return $ fromMaybe "Unknown User" $ find (not . null) results

-- Try to get full name from /etc/passwd using 'getent'
tryGetFullNameFromPasswd :: IO String
tryGetFullNameFromPasswd = do
  user <-
    getEnvironment
      >>= ( \case
              Just u -> pure u
              Nothing -> fail "no user env var found"
          )
        . List.lookup "USER"
  result <- try $ readProcess "getent" ["passwd", user] "" :: IO (Either SomeException String)
  case result of
    Right output ->
      -- Parse the GECOS field (5th field, separated by :)
      let fields = splitOn ":" output
          gecos = if length fields > 4 then fields !! 4 else ""
          name = takeWhile (/= ',') gecos -- Take until first comma
       in return $ if null name then "" else name
    Left _ -> return ""

-- Try to get name from environment variables
tryGetFullNameFromEnvironment :: IO String
tryGetFullNameFromEnvironment = do
  -- Try common environment variables
  result <- try $ getEnv "REAL_NAME" :: IO (Either SomeException String)
  case result of
    Right name -> return name
    Left _ -> do
      -- Try alternative environment variable
      result2 <- try $ getEnv "NAME" :: IO (Either SomeException String)
      case result2 of
        Right name -> return name
        Left _ -> return ""

-- Fallback: get basic username
tryGetUserName :: IO String
tryGetUserName = do
  result <- try $ getEnv "USER" :: IO (Either SomeException String)
  case result of
    Right name -> return name
    Left _ -> do
      result2 <- try $ getEnv "USERNAME" :: IO (Either SomeException String)
      case result2 of
        Right name -> return name
        Left _ -> return ""
