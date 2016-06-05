{-# LANGUAGE OverloadedStrings #-}

module Views 
( home
, new
) where

import Data.Monoid
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

home :: Html
home = do
  h1 "Home"
  p "My name is Wil"
  a "Click here to add new" ! A.href "/new"
 
namedInput :: AttributeValue -> AttributeValue -> Html
namedInput n p = input ! A.type_ "text" ! A.placeholder p ! A.name n
 
new :: Html
new = do
  h1 "new"
  form ! A.action "/items" ! A.method "POST" $ do
    namedInput "url" "URL"
    namedInput "title" "Enter title"
    input ! A.type_ "submit" ! A.value "Submit the form"
