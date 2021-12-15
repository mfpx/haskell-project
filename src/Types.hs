{-# LANGUAGE DeriveGeneric #-}

module Types (
    Entry (..),
    Country (..),
    Record (..),
    Records (..)
) where

import GHC.Generics

data Entry = Entry {
    date_ :: String,
    day_ :: String,
    month_ :: String,
    year_ :: String,
    cases_ :: Int,
    deaths_ :: Int,
    fk_country :: Int
} deriving (Show)

data Country = Country {
    id_ :: Int,
    country_ :: String,
    continent_ ::  String,
    population_ :: Maybe Int
} deriving (Show)

data Record = Record {
    date :: String,
    day :: String,
    month :: String,
    year :: String,
    cases :: Int,
    deaths :: Int,
    country :: String,
    continent ::  String,
    population :: Maybe Int
} deriving (Show, Generic)

data Records = Records {
    records :: [Record]
} deriving (Show, Generic)
