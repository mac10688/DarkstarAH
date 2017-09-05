{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Data.Monoid

import qualified Data.Text as T
import Control.Monad.Trans
import Data.Aeson hiding (json)

import qualified System.Environment as Env
import Text.Read (readMaybe)

import qualified Database.MySQL.Base as MySQL

import Darkstar.Queries

type Api = SpockM MySQL.MySQLConn () () ()

type ApiAction a = SpockAction MySQL.MySQLConn () () a

main :: IO ()
main =
    do 
       let connection = MySQL.connect MySQL.defaultConnectInfo {MySQL.ciUser = "darkstar_auction_house", MySQL.ciDatabase = "darkstar" }
           pool = ConnBuilder connection MySQL.close (PoolCfg 1 1 30)
       spockCfg <- defaultSpockCfg () (PCConn pool) () 
       port <- Env.lookupEnv "PORT"
       case readMaybe =<< port of
         Just portNum -> runSpock portNum (spock spockCfg app)
         Nothing -> runSpock 8080 (spock spockCfg app)

app :: Api
app =
  do get root $
      text "Welcome to darkstar auction house database server."
     get "prices" $ do
      auctionHouseItems <- runQuery getAuctionHousePrices 
      json auctionHouseItems
     get ("transactionHistory" <//> var) $ \itemid -> do
      auctionHouseTransactions <- runQuery (getAuctionHouseTransactions itemid)
      json auctionHouseTransactions
     {-get "armorItems" $ do
      armors <- runQuery getItemArmors
      json armors-}



