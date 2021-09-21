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

validateNotEmpty = validator notEmpty . readText

validator :: (a -> Bool) -> a -> Maybe a
validator p x
  | p x = Just x
  | otherwise = Nothing

requireField :: (BS.ByteString -> Maybe a) -> BS.ByteString -> Router a
requireField prse fieldname = do
    hash <- askHash
    bs <- errorOnNothing (ERequiredFieldMissing fieldname) $ getBs hash fieldname
    x  <- errorOnNothing (EInvalidFieldValue fieldname) $ prse bs
    return x

optional :: (BS.ByteString -> Maybe a) -> BS.ByteString -> Router (Maybe a)
optional prse fieldname = do
    hash <- askHash
    let bs = getBs hash fieldname
    case bs of
        Nothing -> return Nothing
        Just x -> fmap Just $ errorOnNothing (EInvalidFieldValue fieldname) $ prse x

oneOf :: [Router (Maybe a)] -> Router (Maybe a)
oneOf lst = foldr f (return Nothing) lst
  where f x acc = x >>= maybe acc (return . Just)
            

getBs :: Query -> BS.ByteString -> Maybe BS.ByteString
getBs hash field = HS.lookup field hash

readText :: BS.ByteString -> T.Text
readText = E.decodeUtf8

readIntText :: T.Text -> Maybe Int
readIntText = readInt . E.encodeUtf8

readInt :: BS.ByteString -> Maybe Int
readInt = Ae.decode . BSL.fromStrict

readDay :: BS.ByteString -> Maybe Time.Day
readDay = 
     (Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" . T.unpack . E.decodeUtf8)
readList :: (Ae.FromJSON a) => BS.ByteString -> Maybe [a]
readList = (Ae.decode . BSL.fromStrict)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f x = fmap (fmap f) x

{-
require :: (BS.ByteString -> Maybe a) -> Query -> BS.ByteString -> Maybe a
require prse qu arg = HS.lookup arg qu >>= prse

requireText :: Query -> BS.ByteString -> Maybe T.Text
requireText = require (pure . E.decodeUtf8)
requireByteString :: Query -> BS.ByteString -> Maybe BS.ByteString
requireByteString = require pure
-}
{-
requireInt :: Query -> BS.ByteString -> Maybe Int
requireInt = require readInt
-}
{-
requireDay :: Query -> BS.ByteString -> Maybe Time.Day
requireDay = require readDay
-}
{-
requireIntList :: Query -> BS.ByteString -> Maybe [Int]
--requireIntList = require (Ae.decode . BSL.fromStrict)
requireIntList = requireList


requireTextList :: Query -> BS.ByteString -> Maybe [T.Text]
--requireTextList = require (Ae.decode . BSL.fromStrict)
requireTextList = requireList
-}{-
requireList :: (Ae.FromJSON a) => Query -> BS.ByteString -> Maybe [a]
requireList = require (Ae.decode . BSL.fromStrict)
-}
