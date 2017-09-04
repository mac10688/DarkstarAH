{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Data.Word
import Data.Int
import Data.Monoid
import Data.Traversable
import Data.Maybe
import qualified Data.Text as T
import Control.Monad.Trans
import Data.Aeson hiding (json)

import Data.Time.Clock
import Data.Time.Clock.POSIX

import GHC.Generics
import qualified System.Environment as Env
import Text.Read (readMaybe)

import qualified Database.MySQL.Base as MySQL
import qualified System.IO.Streams as Streams

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

getAuctionHouseTransactions :: Word32 -> MySQL.MySQLConn -> IO [AuctionHouseTransaction]
getAuctionHouseTransactions id conn = do
  s <- MySQL.prepareStmt conn "SELECT seller_name, buyer_name, sell_date, sale\
                        \ FROM auction_house \
                        \ WHERE itemid = ? AND sell_date <> 0 \
                        \ ORDER BY date DESC \
                        \ LIMIT 10;"
  (_, is) <- MySQL.queryStmt conn s [MySQL.MySQLInt32U id]
  ahTransactionsStream <- Streams.toList is
  ahTransactions <- forM ahTransactionsStream buildAuctionHouseTransaction
  return $ catMaybes ahTransactions

getAuctionHousePrices :: MySQL.MySQLConn -> IO [AuctionHouseItem]
getAuctionHousePrices conn = do
  currentTime <- getPOSIXTime
  let expirationDate = truncate $ currentTime - (7 * posixDayLength)  :: Word32
  print expirationDate
  s <- MySQL.prepareStmt conn "SELECT itemid \
                              \, stack \
                              \, COUNT(*) \
                              \FROM auction_house \
                              \WHERE sale = 0 AND date > ? \
                              \GROUP BY itemid, stack \
                              \ORDER BY itemid, stack;"
  (def:defs, is) <- MySQL.queryStmt conn s [MySQL.MySQLInt32U expirationDate]
  armorStreams <- Streams.toList is
  armors <- forM armorStreams buildAuctionHouseItem
  return $ catMaybes armors

buildAuctionHouseItem :: [MySQL.MySQLValue] -> IO (Maybe AuctionHouseItem)
buildAuctionHouseItem (MySQL.MySQLInt16U itemid': 
                MySQL.MySQLInt8U stack':
                MySQL.MySQLInt64 count':
                _) = return $ Just $ AuctionHouse 
                                      itemid' 
                                      (stack' == 1) 
                                      count'

buildAuctionHouseItem xs = do 
                            print xs
                            return Nothing 

buildAuctionHouseTransaction :: [MySQL.MySQLValue] -> IO (Maybe AuctionHouseTransaction)
buildAuctionHouseTransaction (MySQL.MySQLText seller':
                              MySQL.MySQLText buyer':
                              MySQL.MySQLInt32U transactionDate':
                              MySQL.MySQLInt32U sale':
                              _) = return $ Just $ AuctionHouseTransaction
                                                    seller'
                                                    buyer'
                                                    transactionDate'
                                                    sale'
buildAuctionHouseTransaction xs = do
  print xs
  return Nothing

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
