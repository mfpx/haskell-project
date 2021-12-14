{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseData,
    parseUser,
    parseUserMetrics,
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

renameFields :: [Char] -> [Char]
renameFields "raw_user_data" = "data"
renameFields "user_id" = "id"
renameFields "bio" = "description"
renameFields "user_metrics" = "public_metrics"
renameFields "tweet_total" = "tweet_count"
renameFields "like_total" = "listed_count"
renameFields other = other

customOptions :: Options
customOptions =
  defaultOptions
    { fieldLabelModifier = renameFields
    }

instance FromJSON Raw where
  parseJSON = JSON.genericParseJSON customOptions

parseData :: L8.ByteString -> Either String Raw
parseData json = eitherDecode json :: Either String Raw

{-
instance ToJSON User where
  toJSON = JSON.genericToJSON $ jsonOptions "Raw"
-}
instance FromJSON User where
  parseJSON = JSON.genericParseJSON customOptions

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
parseUser :: L8.ByteString -> Either String User
parseUser json = eitherDecode json :: Either String User

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

--instance FromJSON UserMetrics where
--parseJSON = JSON.genericParseJSON customOptions

instance FromJSON UserMetrics where
  parseJSON = withObject "public_metrics" $ \b ->
    UserMetrics <$> b .: "following_count"
      <*> b .: "tweet_count"
      <*> b .: "listed_count"
      <*> b .: "followers_count"

parseUserMetrics :: L8.ByteString -> Either String UserMetrics
parseUserMetrics json = eitherDecode json :: Either String UserMetrics

jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
   in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> renameFields}
