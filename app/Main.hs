module Main where

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
  putStrLn "  (4) Get user's recent tweets   "
  putStrLn "  (5) Quit                       "
  putStrLn "---------------------------------"
  hSetBuffering stdout NoBuffering
  putStr "Choose an option > "
  option <- readLn :: IO Int

  case option of
    1 -> do
      putStr "Twitter username: "
      hFlush stdout
      input <- getLine
      json <- getUser input
      --print (dynTypeRep (toDyn json))
      case (parseData json) of
        Left err -> print err
        Right result -> do
          --print (dynTypeRep (toDyn result))
          let output = raw_user_data result
          --print (dynTypeRep (toDyn output))
          --print output
          --print $ username output

          let metrics = user_metrics output
          --print (dynTypeRep (toDyn metrics))
          let output_metrics = UserMetrics (following_count metrics) (tweet_count metrics) (listed_count metrics) (followers_count metrics)
          --print (dynTypeRep (toDyn output_metrics))
          let output_user = User (user_id output) (username output) (name output) (verified output) (location output) (created_at output) (bio output) output_metrics
          -- print (dynTypeRep (toDyn output_user))
          print output_user
          --print $ username output_user

          {-
          --myUser <- parseUser json
          --print myUser
          --print (dynTypeRep (toDyn myUser))
          print $ parseUser json
          --myMetrics <- parseUserMetrics json
          --print myMetrics
          --print (dynTypeRep (toDyn myMetrics))
          print $ parseMetrics json
          -}
          main
    2 -> do
      putStr "Search term: "
      hFlush stdout
      input <- getLine
      let query = packStr'' input
      json <- searchTweets query
      print json
      main
    3 -> do
      putStr "Tweet ID: "
      hFlush stdout
      input <- getLine
      json <- getTweet input
      print json
      main
    4 -> do
      putStr "Twitter username: "
      hFlush stdout
      input <- getLine
      let user = packStr'' input
      json <- getRecentTweets user
      print json

      main
    5 -> do
      exitSuccess
    _ -> do
      exitFailure