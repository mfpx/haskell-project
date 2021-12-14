{-# LANGUAGE OverloadedStrings #-}

module Fetch (getRecentTweets, getTweet, getUser, searchTweets) where

import Data.Aeson (FromJSON, Value, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as S8
import Network.HTTP.Simple

bearerToken = "Bearer AAAAAAAAAAAAAAAAAAAAAKqQWAEAAAAADjJIsHNmXPfaCE8vNJkEZbL8z%2Fw%3DeoJJbBerLBYwlS9TeLEQxfK9XAlzZ801y7oHsuU0jAzInIBCnI"

searchTweets :: ByteString -> IO S8.ByteString -- identical to the one below? why?
searchTweets query = do
  request' <- parseRequest "GET https://api.twitter.com/2/tweets/search/recent"
  let request =
        addRequestHeader "Authorization" bearerToken $
          setRequestQueryString [("query", Just query)] $
            setRequestSecure True $
              setRequestPort
                443
                request'
  response <- httpLBS request

  --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString)
  -- print $ getRequestQueryString request --debug only
  return $ getResponseBody response

getRecentTweets :: ByteString -> IO S8.ByteString
getRecentTweets username = do
  request' <- parseRequest "GET https://api.twitter.com/2/tweets/search/recent"
  let request =
        addRequestHeader "Authorization" bearerToken $
          setRequestQueryString [("query", Just username)] $
            setRequestSecure True $
              setRequestPort
                443
                request'
  response <- httpLBS request

  --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString )
  -- print $ getRequestQueryString request --debug only
  return $ getResponseBody response --returns different results on each run, why?

getTweet :: [Char] -> IO S8.ByteString
getTweet tweetId = do
  request' <- parseRequest $ "GET https://api.twitter.com/2/tweets/" ++ tweetId
  let request =
        addRequestHeader "Authorization" bearerToken $
          setRequestSecure True $
            setRequestQueryString [("tweet.fields", Just "public_metrics")] $
              setRequestPort
                443
                request'
  response <- httpLBS request

  --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString )
  --print $ getRequestQueryString request --debug only
  return $ getResponseBody response

getUser :: [Char] -> IO S8.ByteString
getUser username = do
  request' <- parseRequest $ "GET https://api.twitter.com/2/users/by/username/" ++ username
  let request =
        addRequestHeader "Authorization" bearerToken $
          setRequestSecure True $
            setRequestQueryString [("user.fields", Just "public_metrics,created_at,location,description")] $
              setRequestPort
                443
                request'
  response <- httpLBS request

  --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString )
  --print $ getRequestQueryString request --debug only
  return $ getResponseBody response