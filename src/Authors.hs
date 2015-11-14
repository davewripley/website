{-# LANGUAGE OverloadedStrings #-}

module Authors (Author, authors, makeAuthorLink) where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Text.Internal (Text)
import Data.ByteString.Lazy (readFile, ByteString)

import Data.Text.Internal (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Lucid

data Author = Author { name :: Text, url  :: Text } deriving (Show, Read, Eq)

type AuthorMap = Map Text Author

data AuthorRaw = AuthorRaw { rawTag  :: Text
                           , rawName :: Text
                           , rawUrl  :: Text
                           } deriving (Show)

instance FromJSON AuthorRaw where
  parseJSON (Object v) = AuthorRaw <$>
                         v .: "tag" <*>
                         v .: "name" <*>
                         v .: "url"
  parseJSON _          = mzero


authorArrayFile :: FilePath
authorArrayFile = "./authors-array.json"

outputFilePath :: FilePath
outputFilePath = "./authorData.hs"

authorsBS :: IO ByteString
authorsBS = Data.ByteString.Lazy.readFile authorArrayFile

makeAuthorLink :: Text -> Html ()
makeAuthorLink tg = case (url auth) of
                      "" -> toHtml (name auth)
                      x  -> a_ [href_ x, target_ "_blank"] (toHtml (name auth))
                    where
                      auth = fromJust (M.lookup tg authors)

authorsRawList :: IO (Maybe [AuthorRaw])
authorsRawList = authorsBS >>= (\a -> return (decode a))

getAuthor :: AuthorRaw -> Author
getAuthor (AuthorRaw _ n u) = Author n u

makeAuthorMap :: [AuthorRaw] -> AuthorMap
makeAuthorMap = foldr (\ar -> M.insert (rawTag ar) (getAuthor ar)) M.empty

authors :: IO AuthorMap
authors = do
  arl <- authorsRawList
  return . makeAuthorMap $ fromJust arl
