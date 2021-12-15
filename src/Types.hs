{-# LANGUAGE DeriveGeneric #-}

module Types
  ( RawUser (..),
    User (..),
    Users (..),
    UserMetrics (..),
    RawTweets (..),
    Tweet (..),
    Tweets (..),
    TweetMetrics (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)

-- ##########################################################################################################################################

newtype RawUser = RawUser
  { raw_user_data :: User
  --raw_tweet_data :: Maybe Tweet
  }
  deriving (Eq, Show, Generic)

data User = User
  { user_id :: String,
    username :: String,
    name :: String,
    verified :: Maybe String,
    location :: Maybe String,
    created_at :: String,
    bio :: Maybe String,
    user_metrics :: UserMetrics
  }
  deriving (Eq, Show, Generic)

data UserMetrics = UserMetrics
  { following_count :: !Int,
    tweet_count :: !Int,
    listed_count :: !Int,
    followers_count :: !Int
  }
  deriving (Eq, Show, Generic)

newtype Users = Users
  { users :: [User]
  }
  deriving (Eq, Show, Generic)

-- ##########################################################################################################################################

newtype RawTweets = RawTweets
  { raw_tweet_data :: Tweet
  }
  deriving (Eq, Show, Generic)

data Tweet = Tweet
  { tweet_id :: String,
    fk_user_id :: String,
    tweeted_at :: Maybe String,
    contents :: Maybe String,
    tweet_metrics :: TweetMetrics
  }
  deriving (Eq, Show, Generic)

data TweetMetrics = TweetMetrics
  { quotes_count :: Int,
    retweets_count :: Int,
    likes_count :: Int,
    replies_count :: Int
  }
  deriving (Eq, Show, Generic)

newtype Tweets = Tweets
  { tweets :: [Tweet]
  }
  deriving (Eq, Show, Generic)

-- ##########################################################################################################################################