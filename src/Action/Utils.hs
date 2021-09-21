module Action.Utils where

import qualified Network.Wai as W (Request, pathInfo, queryString)
import qualified Network.HTTP.Types.URI as U (QueryItem)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Aeson as Ae (decode, Value, FromJSON)
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


notEmpty :: T.Text -> Bool
notEmpty = (/= "")

validateRequired :: Bool -> (a -> Bool) -> (BS.ByteString -> Maybe a) -> BS.ByteString -> Either ActionError a
validateRequired admin p func field = case func field of
    Nothing ->  Left $ ERequiredFieldMissing admin field
    Just x  ->  if p x
                then Right x
                else Left $ EInvalidFieldValue admin field

validateOptional :: Bool -> (a -> Bool) -> (BS.ByteString -> Maybe a) -> BS.ByteString -> Either ActionError (Maybe a)
validateOptional admin p func field = case func field of
    Nothing -> return Nothing
    Just x  -> if p x then return $ Just x else Left $ EInvalidFieldValue admin field

requireField :: Bool -> (BS.ByteString -> Maybe a) -> BS.ByteString -> Either ActionError a
requireField admin func fieldname =
    maybe (Left $ ERequiredFieldMissing admin fieldname) Right $ func fieldname



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

requireDay :: Query -> BS.ByteString -> Maybe Time.Day
requireDay = require
     (Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack . E.decodeUtf8)

{-
requireIntList :: Query -> BS.ByteString -> Maybe [Int]
--requireIntList = require (Ae.decode . BSL.fromStrict)
requireIntList = requireList


requireTextList :: Query -> BS.ByteString -> Maybe [T.Text]
--requireTextList = require (Ae.decode . BSL.fromStrict)
requireTextList = requireList
-}
requireList :: (Ae.FromJSON a) => Query -> BS.ByteString -> Maybe [a]
requireList = require (Ae.decode . BSL.fromStrict)

