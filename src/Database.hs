{-# LANGUAGE OverloadedStrings #-}

module Database (initDB, saveUser, saveTweet, saveTweets) where

import Control.Applicative
import Data.Dynamic
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.ToRow
import GHC.Generics (Generic)
import Types

--instance ToRow User where
--toRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

initDB :: IO Connection
initDB = do
  conn <- open "database.sqlite"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS users (\
    \user_id VARCHAR(20) PRIMARY KEY,\
    \username VARCHAR(15) NOT NULL, \
    \name VARCHAR(15) NOT NULL, \
    \verified VARCHAR(15) NOT NULL, \
    \location VARCHAR(50)  NULL, \
    \created_at VARCHAR(15) NOT NULL, \
    \bio VARCHAR(160)  NULL, \
    \following_count INT DEFAULT NULL, \
    \tweet_count INT DEFAULT NULL, \
    \listed_count INT DEFAULT NULL, \
    \followers_count INT DEFAULT NULL)"
  {-
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS tweets (\
    \tweet_id VARCHAR(20),\
    \FOREIGN KEY(fk_user_id) REFERENCES users(user_id), \
    \tweeted_at VARCHAR(40) NOT NULL, \
    \contents VARCHAR(250) NOT NULL, \
    \quotes_count INT DEFAULT NULL, \
    \retweets_count INT DEFAULT NULL, \
    \likes_count INT DEFAULT NULL, \
    \replies_count INT DEFAULT NULL)"
  -}
  return conn

saveUser :: Connection -> User -> IO ()
saveUser conn my_user = do
  print (dynTypeRep (toDyn my_user))
  let my_user_id = user_id my_user
  let my_user_metric = user_metrics my_user
  print my_user_id
  print my_user_metric

--execute conn "INSERT INTO users (user_id, username, name, verified, location, created_at, bio, following_count, tweet_count, listed_count, followers_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" ("1", "2", "3", "4", "5", "6", "7", 8, 9, 10, 11)

{-
  results <- queryNamed conn "SELECT * FROM users WHERE user_id=:id AND username=:un"
  -- Check if user already exists in table
  if not (null results)
    then return . head $ results
    else do
      execute conn "INSERT INTO users (user_id, username, name, verified, location, created_at, bio, following_count, tweet_count, listed_count, followers_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" (user_id, username, name, verified, created_at, description, following_count, followers_count, like_total, tweet_total)
-}
saveTweet :: Connection -> Tweet -> IO ()
saveTweet conn my_tweet = do
  print (dynTypeRep (toDyn my_tweet))
  let my_tweet_id = tweet_id my_tweet
  let my_tweet_metric = tweet_metrics my_tweet
  print my_tweet_id
  print my_tweet_metric

saveTweets :: Connection -> Tweets -> IO ()
saveTweets conn my_tweets = do
  print (dynTypeRep (toDyn my_tweets))

--print my_tweets

--let my_tweet_ids = tweet_id my_tweets
--let my_tweet_metrics = tweet_metrics my_tweets

--print my_tweet_id
--print my_tweet_metrics
