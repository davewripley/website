{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}

module WebsiteTools (AuthorCat(..), classify, listItems, pileUp, lk, doiToLink, sHtml) where

import Lucid
import Data.Monoid ((<>), mempty)
import Data.Text (Text)

import Data.Aeson
import Data.Aeson.TH

data AuthorCat = Solo | CERvR | Other [Text] deriving (Show, Eq)

deriveJSON defaultOptions{sumEncoding = ObjectWithSingleField} ''AuthorCat

class Classify a where
  classify :: a -> Text

instance Classify AuthorCat where
  classify Solo = "solo"
  classify CERvR = "cervr"
  classify (Other _) = "other"

pileUp :: [Html ()] -> Html ()
pileUp = foldr (<>) mempty

listItems :: [Attribute] -> [Html ()] -> Html ()
listItems atts ts = pileUp (map listItem ts)
  where
    listItem t = li_ atts t

lk :: Text -> Html () -> Html ()
lk u t = a_ [href_ u, target_ "_blank"] t

doiToLink :: Text -> Html ()
doiToLink d = lk lnk "DOI link"
  where lnk = "http://dx.doi.org/" <> d

sHtml :: (Show a, Monad m) => a -> HtmlT m ()
sHtml = toHtml . show



