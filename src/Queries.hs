{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries 
( setupDatabase
, insertItem
, insertItems
, getItems
, desqlizeItem
, sqlizeItem
, Item(..)
, Connection(..)
, decode
, encode
) where

import Database.HDBC.Sqlite3
import Database.HDBC

import GHC.Generics
import Data.Aeson

import Control.Monad

data Item = Item 
  { id_ :: Maybe Integer 
  , title :: String 
  , url :: String 
  } deriving (Show, Generic)

instance FromJSON Item where
  parseJSON (Object v) =
    Item <$> v .:? "id_"
         <*> v .: "title"
         <*> v .: "url"
  parseJSON _ = mzero

instance ToJSON Item
--instance FromJSON Item
--instance ToJSON Item

type QueryString = String

insertItemQuery :: QueryString
insertItemQuery = "INSERT INTO items VALUES (?, ?, ?)"
getItemsQuery :: QueryString
getItemsQuery = "SELECT * FROM items"

sqlizeItem :: Item -> [SqlValue]
sqlizeItem (Item id title url) =
  [toSql id, toSql title, toSql url]

desqlizeItem :: [SqlValue] -> Item
desqlizeItem [id, title, url] =
  Item (fromSql id) (fromSql title) (fromSql url)

mockData :: [Item]
mockData = 
  [ Item {id_ = Just 0, title = "Wil's website", url = "www.wil.com"}
  , Item {id_ = Just 1, title = "Wol's wobsote", url = "www.wol.cim"}
  , Item {id_ = Just 2, title = "Wal's wubsite", url = "www.wal.crum"}
  ]

setupDatabase :: IO Connection
setupDatabase = do
  conn <- connectSqlite3 "test1.db"
  -- TODO: migration so only created if not there already
  --run conn "CREATE TABLE items (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, title VARCHAR(24), url VARCHAR(80))" []
  --insertItems conn mockData
  return conn

insertItem :: Connection -> Item -> IO Integer
insertItem conn item = do
  res <- run conn insertItemQuery $ sqlizeItem item
  commit conn
  return res
  
insertItems :: Connection -> [Item] -> IO ()
insertItems conn items = do
  stmt <- prepare conn insertItemQuery
  res <- executeMany stmt $ map sqlizeItem items
  commit conn
  return res

getItems :: Connection -> IO [Item]
getItems conn = do 
  r <- quickQuery conn getItemsQuery []
  return $ map desqlizeItem r
