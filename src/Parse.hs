{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseDataUser,
    parseUser,
    parseUserMetrics,
    parseDataTweet,
    parseTweet,
    parseTweets,
    parseTweetMetrics,
  )
where

import Control.Arrow
import Data.Aeson
import qualified Data.Aeson as JSON
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import qualified Distribution.Fields as JSON
import Distribution.SPDX (LicenseId (JSON))
import Distribution.Simple (License (BSD2))
import GHC.Generics
import Types

-- ##########################################################################################################################################

renameFieldsUser :: [Char] -> [Char]
renameFieldsUser "raw_user_data" = "data"
renameFieldsUser "user_id" = "id"
renameFieldsUser "bio" = "description"
renameFieldsUser "user_metrics" = "public_metrics"
renameFieldsUser "tweet_total" = "tweet_count"
renameFieldsUser "like_total" = "listed_count"
renameFieldsUser other = other

customOptionsUser :: Options
customOptionsUser =
  defaultOptions
    { fieldLabelModifier = renameFieldsUser
    }

instance FromJSON RawUser where
  parseJSON = JSON.genericParseJSON customOptionsUser

parseDataUser :: L8.ByteString -> Either String RawUser
parseDataUser json = eitherDecode json :: Either String RawUser

instance FromJSON User where
  parseJSON = JSON.genericParseJSON customOptionsUser

parseUser :: L8.ByteString -> Either String User
parseUser json = eitherDecode json :: Either String User

instance FromJSON UserMetrics where
  parseJSON = withObject "public_metrics" $ \b ->
    UserMetrics <$> b .: "following_count"
      <*> b .: "tweet_count"
      <*> b .: "listed_count"
      <*> b .: "followers_count"

parseUserMetrics :: L8.ByteString -> Either String UserMetrics
parseUserMetrics json = eitherDecode json :: Either String UserMetrics

-- ##########################################################################################################################################
-- Optional Custom implementation
{-
instance FromJSON User where
  parseJSON = withObject "User" $ \b ->
    User <$> b .: "user_id"
      <*> b .: "username"
      <*> b .: "name"
      <*> b .: "verified"
      <*> b .: "location"
      <*> b .: "created_at"
      <*> b .: "description"
      <*> b .: "user_url"
-}
{-
instance FromJSON UserMetrics where
  parseJSON = JSON.withObject "public_metrics" $ \o -> do
    following <- o .: "following_count"
    tweet <- o .: "tweet_count"
    listed <- o .: "listed_count"
    followers <- o .: "followers_count"
    let following_count = read following
        tweet_count = read tweet
        listed_count = read listed
        followers_count = read followers
    pure UserMetrics {following_count, tweet_count, listed_count, followers_count}
-}
jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
   in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> renameFieldsUser}

-- ##########################################################################################################################################

renameFieldsRaw "raw_tweet_data" = "data"
renameFieldsRaw "tweeted_at" = "created_at"
renameFieldsRaw other = other

customOptionsRaw :: Options
customOptionsRaw =
  defaultOptions
    { fieldLabelModifier = renameFieldsRaw
    }

instance FromJSON RawTweets where
  parseJSON = JSON.genericParseJSON customOptionsRaw

parseDataTweet :: L8.ByteString -> Either String RawTweets
parseDataTweet json = eitherDecode json :: Either String RawTweets

renameFieldsTweet :: [Char] -> [Char]
renameFieldsTweet "tweets" = "data"
renameFieldsTweet "tweet_id" = "id"
renameFieldsTweet "tweeted_at" = "created_at"
renameFieldsTweet "fk_user_id" = "author_id"
renameFieldsTweet "contents" = "text"
renameFieldsTweet "tweet_metrics" = "public_metrics"
renameFieldsTweet "retweets_count" = "retweet_count"
renameFieldsTweet "quotes_count" = "quote_count"
renameFieldsTweet "likes_count" = "like_count"
renameFieldsTweet "replies_count" = "reply_count"
renameFieldsTweet other = other

customOptionsTweet :: Options
customOptionsTweet =
  defaultOptions
    { fieldLabelModifier = renameFieldsTweet
    }

instance FromJSON Tweet where
  parseJSON = JSON.genericParseJSON customOptionsTweet

parseTweet :: L8.ByteString -> Either String Tweet
parseTweet json = eitherDecode json :: Either String Tweet

instance FromJSON Tweets where
  parseJSON = JSON.genericParseJSON customOptionsTweet

parseTweets :: L8.ByteString -> Either String Tweets
parseTweets json = eitherDecode json :: Either String Tweets

instance FromJSON TweetMetrics where
  parseJSON = JSON.genericParseJSON customOptionsTweet

parseTweetMetrics :: L8.ByteString -> Either String TweetMetrics
parseTweetMetrics json = eitherDecode json :: Either String TweetMetrics

-- ##########################################################################################################################################
