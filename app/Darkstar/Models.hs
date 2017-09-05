{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Darkstar.Models where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Data.Word
import Data.Int

data AuctionHouseItem = AuctionHouse
  {
    itemid :: !Word16,
    stack :: !Bool,
    count :: !Int64
  } deriving (Show, Generic, ToJSON, FromJSON)

data AuctionHouseTransaction = AuctionHouseTransaction
  {
    seller :: !T.Text,
    buyer :: !T.Text,
    transactionDate :: !Word32,
    sale :: !Word32
  } deriving (Show, Generic, ToJSON, FromJSON)
