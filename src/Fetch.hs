{-# LANGUAGE OverloadedStrings #-}

module Fetch (getTweet, getUser, searchTweets, getUserByID) where

import Control.Exception (try)
import Data.Aeson (FromJSON, Value, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as S8
import Data.Char (isDigit, isLetter)
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import System.Exit (exitFailure)

-- ##########################################################################################################################################
-- Convert from String to ByteString
packStr'' :: String -> BS.ByteString
packStr'' = encodeUtf8 . DT.pack

-- Returns True if string contains only numbers and False if not
isNumeric :: [Char] -> Bool
isNumeric [] = True
isNumeric (a : as) = if (isDigit a) then isNumeric as else False

--Returns False if string contains a symbol which is not an _. Based on valid twitter username rules
containsSymbol [] = False
containsSymbol (a : as) = if not (isDigit a) && not (isLetter a) && a /= '_' then True else containsSymbol as

bearerToken = "Bearer AAAAAAAAAAAAAAAAAAAAAKqQWAEAAAAADjJIsHNmXPfaCE8vNJkEZbL8z%2Fw%3DeoJJbBerLBYwlS9TeLEQxfK9XAlzZ801y7oHsuU0jAzInIBCnI"

-- ##########################################################################################################################################
searchTweets :: [Char] -> IO S8.ByteString
searchTweets query = do
  case query of
    "" -> do
      Prelude.putStrLn "Please enter a search term"
      exitFailure
      return "Error"
    otherwise -> do
      let first = Prelude.head query
      let first' = case first of
            '#' -> "%23"
            otherwise -> [first]
      let urlQuery = first' ++ Prelude.tail query
      let filters = " lang:en &expansions=author_id&tweet.fields=public_metrics,created_at" --Tweets in English. Expansions show more information in results.
      let extension = urlQuery ++ filters
      request' <- parseRequest $ "GET https://api.twitter.com/2/tweets/search/recent?query=" ++ extension
      let request =
            addRequestHeader "Authorization" bearerToken $
              --setRequestQueryString [("tweet.fields", Just "author_id,created_at,public_metrics")] $
              setRequestSecure True $
                setRequestPort
                  443
                  request'
      response <- httpLBS request
      --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString)
      -- print $ getRequestQueryString request --debug only
      return $ getResponseBody response

-- ##########################################################################################################################################
getTweet :: [Char] -> IO S8.ByteString
getTweet tweetId = do
  let tweetId' = if (isNumeric tweetId) then tweetId else "Not numeric"
  case tweetId' of
    "" -> do
      print "Please enter a tweet ID"
      return "Error"
    "Not numeric" -> do
      print "Please enter a numeric tweet ID"
      exitFailure
      return "Error"
    otherwise -> do
      --let filters = "?tweet.fields=author_id,created_at,public_metrics" --Tweets in English. Expansions show more information in results.
      --let extension = tweetId ++ filters
      request' <- parseRequest $ "GET https://api.twitter.com/2/tweets/" ++ tweetId
      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestQueryString [("tweet.fields", Just "author_id,created_at,public_metrics")] $
                setRequestSecure True $
                  setRequestPort
                    443
                    request'
      response <- httpLBS request
      --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString )
      --print $ getRequestQueryString request --debug only
      return $ getResponseBody response

-- ##########################################################################################################################################
getUser :: [Char] -> IO S8.ByteString
getUser username = do
  let length' = Prelude.length username
  let username'
        | containsSymbol username = "Invalid username"
        | (length' > 15 || length' < 4) = "Invalid length"
        | otherwise = username
  --Validate user input
  case username' of
    "Invalid username" -> do
      Prelude.putStrLn "Please enter a useraname containing only letters, numbers, or _"
      exitFailure
      return "Error"
    "Invalid length" -> do
      Prelude.putStrLn "Please enter a useraname between 4 and 15 characters"
      exitFailure
      return "Error"
    otherwise -> do
      Prelude.putStrLn "Valid username"
      request' <- parseRequest $ "GET https://api.twitter.com/2/users/by/username/" ++ username

      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                setRequestQueryString [("user.fields", Just "public_metrics,created_at,location,description,verified")] $
                  setRequestPort
                    443
                    request'
      response <- try $ httpLBS request

      --Ensure request can be run correctly
      case response of
        Left e -> do
          --catch connection errors
          print (e :: HttpException)
          Prelude.putStrLn "Please ensure you are connected to the internet"
          return "Error"
        Right response -> do
          S8.putStrLn $ (getResponseBody response)
          --print $ getRequestQueryString request --debug only
          return $ getResponseBody response

getUserByID :: [Char] -> IO S8.ByteString
getUserByID id = do
  let length' = Prelude.length id
  let id'
        | not(isNumeric id) = "Invalid user id"
        | (length' > 20 || length' < 1) = "Invalid length"
        | otherwise = id
  --Validate user input
  case id' of
    "Invalid user id" -> do
      Prelude.putStrLn "Please enter a user id containing only of numbers"
      exitFailure
      return "Error"
    "Invalid length" -> do
      Prelude.putStrLn "Please enter an id between 1 and 20 characters"
      exitFailure
      return "Error"
    otherwise -> do
      --Prelude.putStrLn "Valid user id"
      request' <- parseRequest $ "GET https://api.twitter.com/2/users/" ++ id

      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                setRequestQueryString [("user.fields", Just "public_metrics,created_at,location,description,verified")] $
                  setRequestPort
                    443
                    request'
      response <- try $ httpLBS request

      --Ensure request can be run correctly
      case response of
        Left e -> do
          --catch connection errors
          print (e :: HttpException)
          Prelude.putStrLn "Please ensure you are connected to the internet"
          return "Error"
        Right response -> do
          --S8.putStrLn $ (getResponseBody response)
          --print $ getRequestQueryString request --debug only
          return $ getResponseBody response

-- ##########################################################################################################################################
