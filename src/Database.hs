module Database where

{-
  ( initialiseDB,
    getOrCreateUser,
  )
where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.ToRow
import Types

--import Types (User (created_at, description, followers_count, following_count, like_total, name, tweet_total, user_id, user_url), Users (username_))

-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User user_id username name verified created_at description following_count followers_count like_total tweet_total user_url) =
    toRow (user_id, username, name, verified, created_at, description, following_count, followers_count, like_total, tweet_total, user_url)

initialiseDB :: IO Connection
initialiseDB = do
  conn <- open "stalker.sqlite"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS users (\
    \user_id INTEGER PRIMARY KEY,\
    \username VARCHAR(15) NOT NULL, \
    \name VARCHAR(15) NOT NULL, \
    \verified BOOLEAN NOT NULL, \
    \location VARCHAR(50)  NULL, \
    \created_at VARCHAR(15) NOT NULL, \
    \description VARCHAR(160)  NULL, \
    \following INT DEFAULT NULL, \
    \followers INT DEFAULT NULL, \
    \likes_total INT DEFAULT NULL, \
    \tweets_total INT DEFAULT NULL, \
    \user_url VARCHAR(100) NULL, \
    \)"
  return conn

{-
getOrCreateUser :: Connection -> Int -> String -> String -> Bool -> Maybe String -> String -> Maybe String -> Int -> Int -> Int -> Int -> String -> IO User
getOrCreateUser conn user_id username name verified location created_at description following_count followers_count like_total tweet_total user_url = do
  results <- queryNamed conn "SELECT * FROM users WHERE user_id=:user_id AND username=:username" [":user_id" := user_id, ":username" := username, ":name" := name, ":verified" := verified, ":location" := location, ":created_at" := created_at, ":description" := description, ":following_count" := following_count, ":followers_count" := followers_count, ":like_total" := like_total, ":tweet_total" := tweet_total, ":user_url" := user_url]
  if length results > 0
    then return . head $ results
    else do
      execute conn "INSERT INTO users (user_id, username, name, verified, location, created_at, description, following_count, followers_count, like_total, tweet_total, user_url) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" (user_id, username, name, verified, created_at, description, following_count, followers_count, like_total, tweet_total, user_url)
      getOrCreateUser conn user_id username name verified location created_at description following_count followers_count like_total tweet_total user_url
-}
-}