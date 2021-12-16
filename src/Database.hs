{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database (initDB, saveUser, saveTweet, saveTweets, querySavedUsers, querySavedTweetsByUser) where

import Control.Applicative
import Data.Dynamic
import qualified Data.Text as DT
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow
import Fetch
import GHC.Generics (Generic)
import Parse
import Types

newtype UserField = UserField DT.Text deriving (Show)

instance FromRow UserField where
  fromRow = UserField <$> field

newtype TweetField = TweetField DT.Text deriving (Show)

instance FromRow TweetField where
  fromRow = TweetField <$> field

-- ##########################################################################################################################################

initDB :: IO Connection
initDB = do
  conn <- open "database.sqlite"
  --runRaw conn "COMMIT; PRAGMA foreign_keys = ON"
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
    \followers_count INT DEFAULT NULL)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS tweets (\
    \tweet_id VARCHAR(20) PRIMARY KEY,\
    \fk_user_id VARCHAR(20),\
    \tweeted_at VARCHAR(40) NULL, \
    \contents VARCHAR(250)  NULL, \
    \quote_count INT DEFAULT NULL, \
    \retweet_count INT DEFAULT NULL, \
    \like_count INT DEFAULT NULL, \
    \reply_count INT DEFAULT NULL, \
    \FOREIGN KEY('fk_user_id') REFERENCES 'users'('user_id'))"
  return conn

-- ##########################################################################################################################################

saveUser :: Connection -> User -> IO ()
saveUser conn my_user = do
  --print (dynTypeRep (toDyn my_user))
  let my_user_id = user_id my_user
  let my_user_metric = user_metrics my_user
  -- Check if User already exists
  checkUser <- queryNamed conn "SELECT user_id FROM users WHERE user_id = :user_id" [":user_id" := my_user_id] :: IO [UserField]
  --print $ length checkUser
  if null checkUser
    then do
      -- Add User to Database
      print "Adding user to database"
      execute conn "INSERT INTO users (user_id, username, name, verified, location, created_at, bio, following_count, tweet_count, followers_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" (user_id my_user, username my_user, name my_user, verified my_user, location my_user, created_at my_user, bio my_user, following_count my_user_metric, tweet_count my_user_metric, followers_count my_user_metric)
    else do
      --Update user
      print "User Already exists, Updataing..."
      executeNamed conn "UPDATE users SET username = :username, name= :name, verified = :verified, location = :location, bio = :bio, following_count = :following_count, tweet_count= :tweet_count, followers_count = :followers_count WHERE user_id = :user_id" [":username" := username my_user, ":name" := name my_user, ":verified" := verified my_user, ":location" := location my_user, ":bio" := bio my_user, ":following_count" := following_count my_user_metric, ":tweet_count" := tweet_count my_user_metric, ":followers_count" := followers_count my_user_metric, ":user_id" := user_id my_user]

-- ##########################################################################################################################################
saveTweet :: Connection -> Tweet -> IO ()
saveTweet conn my_tweet = do
  --print (dynTypeRep (toDyn my_tweet))
  let my_tweet_id = tweet_id my_tweet
  let my_tweet_metric = tweet_metrics my_tweet
  let my_user_id = fk_user_id my_tweet
  --print my_tweet_id
  -- Check if a Tweet already exists
  checkTweet <- queryNamed conn "SELECT tweet_id FROM tweets WHERE tweet_id = :tweet_id" [":tweet_id" := my_tweet_id] :: IO [TweetField]
  --print $ length checkTweet
  if null checkTweet
    then do
      -- Add User to Database
      json <- getUserByID my_user_id --fetch user information
      case parseDataUser json of
        Left err -> print err
        Right result -> do
          let output = raw_user_data result
          let metrics = user_metrics output
          let output_metrics = UserMetrics (following_count metrics) (tweet_count metrics) (followers_count metrics)
          let output_user = User (user_id output) (username output) (name output) (verified output) (location output) (created_at output) (bio output) output_metrics

          -- Add user to database
          saveUser conn output_user
          -- Add Tweet to Database
          print "Inserting new tweets..."
          execute conn "INSERT INTO tweets (tweet_id,fk_user_id,tweeted_at, contents, quote_count, retweet_count, like_count, reply_count ) VALUES (?,?, ?, ?, ?, ?, ?, ?)" (tweet_id my_tweet, fk_user_id my_tweet, tweeted_at my_tweet, contents my_tweet, quote_count my_tweet_metric, retweet_count my_tweet_metric, like_count my_tweet_metric, reply_count my_tweet_metric)
    else do
      --Update Tweet
      print "Tweet Already exists, Updating..."
      executeNamed conn "UPDATE tweets SET like_count = :like_count, retweet_count=:retweet_count, quote_count=:quote_count , reply_count=:reply_count  WHERE tweet_id = :tweet_id" [":tweet_id" := tweet_id my_tweet, ":like_count" := like_count my_tweet_metric, ":retweet_count" := retweet_count my_tweet_metric, ":quote_count" := quote_count my_tweet_metric, ":reply_count" := reply_count my_tweet_metric]

saveTweets :: Connection -> [Tweet] -> IO ()
saveTweets conn = mapM_ (saveTweet conn)

-- ##########################################################################################################################################

querySavedUsers :: Connection -> IO () -- [User]
querySavedUsers conn = do
  putStr "Enter username > "
  userName <- getLine
  putStrLn $ "Looking for " ++ userName ++ "..."
  searchUser <- queryNamed conn "SELECT name FROM users WHERE username = :username" [":username" := userName] :: IO [UserField]
  if not $ null searchUser
    then do
      print "Found user in Database!"
      print searchUser
    else do
      print "User not found in Database!"

querySavedTweetsByUser :: Connection -> IO () -- [Tweet]
querySavedTweetsByUser conn = do
  putStr "Enter username > "
  userName <- getLine
  putStrLn $ "Looking for " ++ userName ++ "'s Tweets..."

--let sql = "SELECT * FROM tweets inner join users on tweets.fk_user_id == users.user_id WHERE username =?"
--query conn sql [userName]
-- ##########################################################################################################################################
