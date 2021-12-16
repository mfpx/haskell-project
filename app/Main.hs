module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Dynamic
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml.Builder (toByteString)
import Database
import Distribution.SPDX (LicenseId (JSON))
import Distribution.Types.InstalledPackageInfo.Lens (description)
import Fetch
import Parse
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Types

packStr'' :: String -> BS.ByteString
packStr'' = encodeUtf8 . DT.pack

-- pls dont delete above
main = do
  putStrLn "---------------------------------"
  putStrLn "  Welcome to the Twitter api app "
  putStrLn "  (1) Get user information       "
  putStrLn "  (2) Search tweets (e.g., relating to crypto)  "
  putStrLn "  (3) Get tweet by ID            "
  putStrLn "  (4) Search saved Users         "
  --putStrLn "  (5) Compare two saved Users  "
  putStrLn "  (5) Get user by user id                       "
  putStrLn "  (6) Quit                       "
  putStrLn "---------------------------------"
  conn <- initDB
  hSetBuffering stdout NoBuffering
  putStr "Choose an option > "
  option <- readLn :: IO Int
  case option of
    1 -> do
      putStr "Twitter username: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- getUser input
      --print json
      print "Parsing..."
      case parseDataUser json of
        Left err -> print err
        Right result -> do
          let output = raw_user_data result
          let metrics = user_metrics output
          let output_metrics = UserMetrics (following_count metrics) (tweet_count metrics) (followers_count metrics)
          let output_user = User (user_id output) (username output) (name output) (verified output) (location output) (created_at output) (bio output) output_metrics
          -- output_user
          print "Saving on DB..."
          saveUser conn output_user
          main
    2 -> do
      putStr "Search term: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- searchTweets input
      --print json
      print "Parsing..."
      case parseTweets json of
        Left err -> print err
        Right result -> do
          let output_tweets = tweets result
          let l = length output_tweets
          print "Saving on DB..."
          saveTweets conn output_tweets
          main
    3 -> do
      putStr "Tweet ID: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- getTweet input
      --print json
      print "Parsing..."
      case parseDataTweet json of
        Left err -> print err
        Right result -> do
          let output_tweet = raw_tweet_data result
          print output_tweet
          print "Saving on DB..."
          saveTweet conn output_tweet
          main
    4 -> do
      entries <- querySavedUsers conn
      --mapM_ print entries
      main
    5 -> do
      putStr "User ID: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- getUserByID input
      print "Parsing..."
      case parseDataUser json of
        Left err -> print err
        Right result -> do
          let output = raw_user_data result
          let metrics = user_metrics output
          let output_metrics = UserMetrics (following_count metrics) (tweet_count metrics) (followers_count metrics)
          let output_user = User (user_id output) (username output) (name output) (verified output) (location output) (created_at output) (bio output) output_metrics
          print output_user
          print "Saving on DB..."
          saveUser conn output_user
          main
    6 -> do
      exitSuccess
    _ -> do
      exitFailure