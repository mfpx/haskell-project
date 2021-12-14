{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Raw (..),
    User (..),
    Users (..),
    UserMetrics (..),
    Tweet (..),
    Tweets (..),
    TweetMetrics (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)

data Raw = Raw
  { raw_user_data :: User
  --raw_tweet_data :: Maybe Tweet
  }
  deriving (Eq, Show, Generic)

data User = User
  { user_id :: String,
    username :: String,
    name :: String,
    verified :: Maybe Bool,
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
  deriving (Show, Generic)

data Tweet = Tweet
  { tweet_id :: Int,
    fk_user_id :: Int,
    tweeted_at :: String,
    contents :: String,
    tweet_metrics :: Maybe TweetMetrics,
    tweet_url :: Maybe String
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
  deriving (Show, Generic)
