{-# LANGUAGE OverloadedStrings #-}

module Darkstar.Queries where

import Darkstar.Models

import qualified System.IO.Streams as Streams
import qualified Database.MySQL.Base as MySQL
import Data.Word

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Data.Int

import Data.Traversable
import Data.Maybe

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

getArmorItems :: MySQL.MySQLConn -> IO [ArmorItem]
getArmorItems conn = do
  s <- MySQL.prepareStmt conn "SELECT b.itemid, a.name\
                              \ FROM item_basic b\
                              \ JOIN item_armor a ON b.itemid = a.itemid\
                              \ WHERE NoSale = 0;"
  (_, is) <- MySQL.queryStmt conn s []
  armorStream <- Streams.toList is
  armors <- forM armorStream buildArmorItem
  return $ catMaybes armors

buildArmorItem :: [MySQL.MySQLValue] -> IO (Maybe ArmorItem)
buildArmorItem (MySQL.MySQLInt16U itemid':
                 MySQL.MySQLText name':
                 _) = return $ Just $ ArmorItem
                                      itemid'
                                      name'
buildArmorItem xs = do
  print xs
  return Nothing

getWeaponItems :: MySQL.MySQLConn -> IO [WeaponItem]
getWeaponItems conn = do
  s <- MySQL.prepareStmt conn "SELECT b.itemid, a.name\
                              \ FROM item_basic b\
                              \ JOIN item_armor a ON b.itemid = a.itemid\
                              \ JOIN item_weapon w ON a.itemid = w.itemid;"
  (_, is) <- MySQL.queryStmt conn s []
  weaponStream <- Streams.toList is
  weapons <- forM weaponStream buildWeaponItem
  return $ catMaybes weapons

buildWeaponItem :: [MySQL.MySQLValue] -> IO (Maybe WeaponItem)
buildWeaponItem (MySQL.MySQLInt16U itemid':
                 MySQL.MySQLText name':
                 _) = return $ Just $ WeaponItem
                                      itemid'
                                      name'
buildWeaponItem xs = do
  print xs
  return Nothing
