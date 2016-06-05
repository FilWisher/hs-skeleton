{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Control.Monad
import Data.Monoid
import Control.Monad.IO.Class

import Text.Blaze.Html.Renderer.Text

import qualified Views
import Queries

main :: IO ()
main = do
  conn <- setupDatabase
  scotty 3000 $ do
    get "/" $ html $ renderHtml Views.home
    get "/items" $ itemsG conn
    post "/items" $ itemsP conn
    get "/new" $ html $ renderHtml Views.new
      
itemsG :: Connection -> ActionM ()
itemsG conn = do
  items <- liftIO $ getItems conn
  json items
 
-- TODO: should return Nothing on failure; not return 500
parseItem :: ActionM (Maybe Item)
parseItem = do
  u <- param "url"
  t <- param "title"
  return $ Just (Item {id_ = Nothing, url = u, title = t})

itemsP :: Connection -> ActionM ()
itemsP conn = do
  maybeItem <- parseItem
  case maybeItem of
    (Just i) -> do
      r <- liftIO $ insertItem conn i
      json i
    otherwise -> text "wat"
