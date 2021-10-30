module Action.Utils (
    requireField,
    validateNotEmpty,
    readInt,
    fmap2,
    optional,
    readDay,
    oneOf,
    readList,
    withPagination,
    validator,
    notEmpty,
    readText,
    readIntText
) where

import Action.Common
import qualified Data.Aeson as Ae (FromJSON, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import qualified Data.HashMap.Strict as HS (lookup)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import qualified Data.Time as Time
import qualified Types as Y
import Prelude hiding (readList)

notEmpty :: T.Text -> Bool
notEmpty = (/= "")

validateNotEmpty :: BS.ByteString -> Maybe T.Text
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

readText :: BS.ByteString -> T.Text
readText = E.decodeUtf8

readIntText :: T.Text -> Maybe Int
readIntText = readInt . E.encodeUtf8

readInt :: BS.ByteString -> Maybe Int
readInt = Ae.decode . BSL.fromStrict

readDay :: BS.ByteString -> Maybe Time.Day
readDay =
    Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" .
    T.unpack . E.decodeUtf8

readList :: (Ae.FromJSON a) => BS.ByteString -> Maybe [a]
readList = Ae.decode . BSL.fromStrict

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f = fmap (fmap f)

{-
data Y.Paginated a = Paginated {
    _pag_page :: Int,
    _pag_size :: Int,
    _pag_data :: a
    } deriving (Show, Generic, GP.PrettyShow) 

-}
defaultPage, defaultSize :: Int
defaultPage = 0

defaultSize = 10000

withPagination :: Router a -> Router (Y.Paginated a)
withPagination m = do
    x <- m
    page <- requireWithDefault readInt defaultPage "page"
    size <- requireWithDefault readInt defaultSize "size"
    pure $ Y.Paginated page size x
