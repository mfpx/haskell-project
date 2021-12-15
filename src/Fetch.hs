{-# LANGUAGE OverloadedStrings #-}
{-|

Module        : Fetch
Description   : Performs HTTP get requests to the twitter api to search tweets, find a user, and find a tweet given an input.
-}
-- The following functions are used:
-- getTweet :: [Char] -> IO ByteString
-- getUser :: [Char] -> IO ByteString
-- searchTweets :: [Char] -> IO ByteString

-- Input to the function is also validated before performing a HTTP request and the following functions help with this.
-- isNumeric :: [Char] -> Bool - Returns True if string contains only numbers and False otherwise
-- containsSymbol :: [Char] -> Bool - Returns False if string contains only letters, numbers, or _. Returns False otherwise


module Fetch (getTweet, getUser, searchTweets, searchTweets') where

import Data.Aeson (FromJSON, Value, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as S8
import Data.Text
import Network.HTTP.Simple
--Error handling
import Data.Char (isDigit, isLetter) -- check if input numeric
import           Control.Exception          (try)
import System.Exit (exitSuccess, exitFailure)
import Network.HTTP.Client.Conduit (Response(responseHeaders))
bearerToken = "Bearer AAAAAAAAAAAAAAAAAAAAAKqQWAEAAAAADjJIsHNmXPfaCE8vNJkEZbL8z%2Fw%3DeoJJbBerLBYwlS9TeLEQxfK9XAlzZ801y7oHsuU0jAzInIBCnI"


{-|
  The searchTweets function returns tweets from the last seven days matching the input string.
-}
searchTweets' :: [Char] -- ^ string to search against tweets
                  -> IO S8.ByteString  -- ^ tweets matching input string
searchTweets' query = do
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
      let filters = " lang:en &expansions=author_id&tweet.fields=public_metrics" --Tweets in English. Expansions show more information in results.
      let url = "https://api.twitter.com/2/tweets/search/recent?query=" ++ urlQuery ++ filters
      request' <- parseRequest $ "GET " ++ url
      let request
                    = addRequestHeader "Authorization" bearerToken
                    $ setRequestSecure True
                    $ setRequestPort 443
                    request'
      
      response <- httpLBS request
      return $ getResponseBody response      

searchTweets :: ByteString -> IO Value
searchTweets query = do
  
  case query of 
    "" -> do 
      Prelude.putStrLn "Please enter a search term"
      exitFailure
      return "Error"
    otherwise -> do 
      let url = "https://api.twitter.com/2/tweets/search/recent"
      
      request' <- parseRequest $ "GET " ++ url
      let request
                    = addRequestHeader "Authorization" bearerToken
                    $ setRequestQueryString [("query", Prelude.Just query)]
                    $ addToRequestQueryString [("expansions", Prelude.Just "author_id")]
                    $ addToRequestQueryString [("tweet_fields", Prelude.Just "public_metrics")]
                    $ setRequestSecure True
                    $ setRequestPort 443
                    request'
      
      response <- httpJSON request
      S8.putStrLn $ encode (getResponseBody response :: Value)
      print $ getRequestQueryString request --debug only
      --S8.putStrLn $ encode (getResponseBody response)
      --print $ getRequestQueryString request --debug only
      

      return $ getResponseBody response      

{-|
  Checks whether a string contains only numbers
-}
isNumeric :: [Char] -- ^ String to check
            -> Bool -- ^ True if string contains only numbers, False if not
isNumeric [] = True
isNumeric (a:as) = if not(isDigit a) then False else isNumeric as


{-|
  Returns a tweet matching a tweet id
-}
data TwitterQuery = Either S8.ByteString Bool

data Maybe' a = Just a | Bool

{-|
  Returns a tweet given a valid tweet id.
-}

getTweet :: [Char] -- ^ Tweet id. Must be numeric and have a length between 1 and 25.
          ->  IO S8.ByteString  -- ^ Tweet corresponding to tweet id. In the event of an error, returns False and exits the script with exitFailure.
getTweet tweetId = do
  let length' = Prelude.length tweetId
  let tweetId'
                | not(isNumeric tweetId) = "Not numeric"
                | length' > 25  = "Invalid length"
                | tweetId == "" = ""
                | otherwise = tweetId

  case tweetId' of 
    "" -> do
      Prelude.putStrLn "Please enter a tweet id"
      exitFailure 
      return "Error"
    "Not numeric" -> do
      Prelude.putStrLn "Please enter a tweet id consisting of only numbers"
      exitFailure
      return "Error"
    "Invalid length" -> do
      Prelude.putStrLn "Please enter a tweet id with a maximum of 25 characters"
      exitFailure
      return "Error"
    otherwise -> do 
      request' <- parseRequest $ "GET https://api.twitter.com/2/tweets/" ++ tweetId
      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                setRequestQueryString [("tweet.fields", Prelude.Just "public_metrics")] $
                addToRequestQueryString [("expansions", Prelude.Just "author_id")] $
                  setRequestPort
                    443
                    request'
      response <- httpLBS request
      S8.putStrLn $ getResponseBody response
      print $ getRequestQueryString request --debug only
      return $ getResponseBody response

{-| 
Checks whether a string contains symbols which are not underscores '_'.
Returns True if it does and False otherwise
-}
containsSymbol [] = False
containsSymbol (a:as) = if not(isDigit a) && not(isLetter a) && a /= '_'  then True else containsSymbol as

-- | Returns a user's information on twitter given a user id
getUser :: [Char] -- ^ User id. Must contain only letters, numbers, and underscores. Must have a length between 4 and 15 characters.
            -> IO S8.ByteString -- ^ Twitter user's information. Returns "Error" and exits the script if there is an Error
getUser username = do
  
  --Validate user input 
  let length' = Prelude.length username
  let username'
                | containsSymbol username = "Invalid username"
                | (length' > 15 || length' < 4) = "Invalid length"
                | otherwise = username

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

      request' <- parseRequest $ "GET https://api.twitter.com/2/users/by/username/" ++ username
      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                setRequestQueryString [("user.fields", Prelude.Just "public_metrics,created_at,location,description")] $
                  setRequestPort
                    443
                    request'
      response <- httpLBS request

      --S8.putStrLn $ encode (getResponseBody response :: Value)
      --print $ getRequestQueryString request --debug only

      return $ getResponseBody response