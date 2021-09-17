module Action.Utils where

import qualified Network.Wai as W (Request, pathInfo, queryString)
import qualified Network.HTTP.Types.URI as U (QueryItem)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Aeson as Ae (decode, Value)
import qualified Data.ByteString.Lazy as BSL (fromStrict, unpack, ByteString)
import qualified Data.ByteString as BS
import qualified Data.Time as Time
import qualified Data.HashMap.Strict as HS (HashMap, fromList, lookup)
import qualified Data.Maybe as Mb (catMaybes)
import Control.Applicative ((<|>))

import Data.Bifunctor (bimap)
import GHC.Generics
import qualified GenericPretty as GP
import Action.Common





requireField :: (BS.ByteString -> Maybe a) -> BS.ByteString -> Either ActionError a
requireField func fieldname =
    maybe (Left $ ERequiredFieldMissing fieldname) Right $ func fieldname



require :: (BS.ByteString -> Maybe a) -> Query -> BS.ByteString -> Maybe a
require prse qu arg = HS.lookup arg qu >>= prse

requireText :: Query -> BS.ByteString -> Maybe T.Text
requireText = require (pure . E.decodeUtf8)

requireByteString :: Query -> BS.ByteString -> Maybe BS.ByteString
requireByteString = require pure



requireInt :: Query -> BS.ByteString -> Maybe Int
requireInt = require readInt

readIntText :: T.Text -> Maybe Int
readIntText = readInt . E.encodeUtf8

readInt :: BS.ByteString -> Maybe Int
readInt = Ae.decode . BSL.fromStrict

requireIntList :: Query -> BS.ByteString -> Maybe [Int]
requireIntList = require (Ae.decode . BSL.fromStrict)

requireDay :: Query -> BS.ByteString -> Maybe Time.Day
requireDay = require
     (Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack . E.decodeUtf8)


