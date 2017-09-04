{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AuctionHouseModel where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock
import Data.Time.Clock.POSIX

data AuctionHouse = AuctionHouse {
  items :: [AuctionHouseItem]
} deriving (Show, Generic, ToJSON, FromJSON)

data AuctionHouseItem = AuctionHouseItem {
  price :: Int,
  expirationDate :: Int,
  item :: Item
} deriving (Show, Generic, ToJSON, FromJSON)

data Item = WeaponItem Weapon | ArmorItem Armor deriving (Show, Generic, ToJSON, FromJSON)

data Weapon = Weapon { 
  name :: !T.Text
  , damage :: Int 
  , delay :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

data Armor = Armor {
  name :: !T.Text
  , defense :: Int
  , elementalResistance :: Int
} deriving (Show, Generic, ToJSON, FromJSON)

balrog = Armor { name=T.pack "Balrog Skin", defense=1050, elementalResistance=50 }

balrogItem :: POSIXTime -> AuctionHouseItem
balrogItem today = AuctionHouseItem { price = 1050, expirationDate = round $ addDay today 3, item = ArmorItem balrog }

scorpionHarness = Armor { name=T.pack "Scorpion Harness", defense=46, elementalResistance=20 }

scorpionHarnessItem :: POSIXTime -> AuctionHouseItem
scorpionHarnessItem today = AuctionHouseItem { price = 1300, expirationDate = round $ addDay today 4, item = ArmorItem scorpionHarness }

krakenClub = Weapon { name=T.pack "Kraken Club", damage=11, delay=264 }

krakenClubItem :: POSIXTime -> AuctionHouseItem
krakenClubItem today = AuctionHouseItem { price = 1200, expirationDate = round $ addDay today 10, item = WeaponItem krakenClub }

windforce = Weapon { name=T.pack "Windforce", damage=547, delay=55 }

windforceItem :: POSIXTime  -> AuctionHouseItem
windforceItem today = AuctionHouseItem { price = 800, expirationDate = round $ addDay today 20, item = WeaponItem windforce }

addDay :: POSIXTime -> Int -> POSIXTime
addDay dateTime daysToAdd = utcTimeToPOSIXSeconds $ (fromIntegral daysToAdd * posixDayLength) `addUTCTime` posixSecondsToUTCTime dateTime

auctionHouse :: POSIXTime -> AuctionHouse
auctionHouse today = AuctionHouse {
  items = [balrogItem today, scorpionHarnessItem today, krakenClubItem today, windforceItem today] 
}

test :: IO()
test = do
  today <- getPOSIXTime
  putStrLn $ B.unpack $ encodePretty $ auctionHouse today
