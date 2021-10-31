module Action.Utils
    ( requireField
    , validateNotEmpty
    , readInt
    , fmap2
    , optional
    , readDay
    , oneOf
    , readList
    , withPagination
    , validator
    , notEmpty
    , readText
    , readIntText
    , defaultPage
    , defaultSize
    ) where

import Action.Common
import qualified Data.Aeson as Ae (FromJSON, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import qualified Data.HashMap.Strict as HS (lookup)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import Prelude hiding (readList)
import qualified Types as T

notEmpty :: Text.Text -> Bool
notEmpty = (/= "")

validateNotEmpty :: BS.ByteString -> Maybe Text.Text
validateNotEmpty = validator notEmpty . readText

validator :: (a -> Bool) -> a -> Maybe a
validator p x
    | p x = Just x
    | otherwise = Nothing

requireField ::
       (BS.ByteString -> Maybe a) -> BS.ByteString -> Router a
requireField prse fieldname = do
    hash <- askHash
    bs <-
        errorOnNothing (ERequiredFieldMissing fieldname) $
        getBs hash fieldname
    errorOnNothing (EInvalidFieldValue fieldname) $ prse bs

optional ::
       (BS.ByteString -> Maybe a) -> BS.ByteString -> Router (Maybe a)
optional prse fieldname = do
    hash <- askHash
    let bs = getBs hash fieldname
    case bs of
        Nothing -> pure Nothing
        Just x ->
            fmap Just $
            errorOnNothing (EInvalidFieldValue fieldname) $ prse x

oneOf :: [Router (Maybe a)] -> Router (Maybe a)
oneOf = foldr f (pure Nothing)
  where
    f x acc = x >>= maybe acc (pure . Just)

requireWithDefault ::
       (BS.ByteString -> Maybe a) -> a -> BS.ByteString -> Router a
requireWithDefault prse deflt fieldname = do
    hash <- askHash
    let bs = getBs hash fieldname
    case bs of
        Nothing -> pure deflt
        Just x ->
            errorOnNothing (EInvalidFieldValue fieldname) $ prse x

getBs :: Query -> BS.ByteString -> Maybe BS.ByteString
getBs hash field = HS.lookup field hash

readText :: BS.ByteString -> Text.Text
readText = E.decodeUtf8

readIntText :: Text.Text -> Maybe Int
readIntText = readInt . E.encodeUtf8

readInt :: BS.ByteString -> Maybe Int
readInt = Ae.decode . BSL.fromStrict

readDay :: BS.ByteString -> Maybe Time.Day
readDay =
    Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" .
    Text.unpack . E.decodeUtf8

readList :: (Ae.FromJSON a) => BS.ByteString -> Maybe [a]
readList = Ae.decode . BSL.fromStrict

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f = fmap (fmap f)

defaultPage, defaultSize :: Int
defaultPage = 0

defaultSize = 10000

withPagination :: Router a -> Router (T.Paginated a)
withPagination m = do
    x <- m
    page <- requireWithDefault readInt defaultPage "page"
    size <- requireWithDefault readInt defaultSize "size"
    pure $ T.Paginated page size x
