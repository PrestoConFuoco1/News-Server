{-# LANGUAGE DeriveAnyClass #-}
module RequestToAction where

import qualified Network.Wai as W (Request, pathInfo, queryString)
import qualified Network.HTTP.Types.URI as U (QueryItem)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8)
import qualified Data.Aeson as Ae (decode)
import qualified Data.ByteString.Lazy as BSL (fromStrict, unpack, ByteString)
import qualified Data.ByteString as BS
import qualified Data.Time as Time

import Data.Bifunctor (bimap)
import GHC.Generics
import qualified GenericPretty as GP

type TagId = Int
type Author = T.Text
type QueryItem = (T.Text, Maybe T.Text)
type Query = [QueryItem]
type Token = T.Text

data CreationDateOptions = Created Time.Day
                         | CreatedEarlier Time.Day
                         | CreatedLater Time.Day
            deriving (Show, Generic)
data TagsOptions = OneTag TagId | TagsIn [TagId] | TagsAll [TagId]
    deriving (Show, Generic)
data SearchOptions = SearchOptions T.Text
    deriving (Show, Generic)

data SortEntity = SEDate | SEAuthor | SECategory | SEPhotoNumber
    deriving (Show)
data SortOrder  = SOAscending | SODescending
    deriving (Show)
data SortOptions = SortOptions {
    _so_sortBy :: SortEntity,
    _so_order :: SortOrder
    }
    deriving (Show, Generic)
defaultSortOptions :: SortOptions
defaultSortOptions = SortOptions SEDate SODescending -- newer posts first

data WhoWhat a = WhoWhat {
    _ww_token  :: Maybe Token,
    --_ww_action :: Action
    _ww_action :: a
    } deriving (Show, Generic) 

data Action = AGetPosts GetPosts | AGetCategories GetCategories | AGetAuthors GetAuthors
    deriving (Show, Generic)

data GetCategories = GetCategories
    deriving (Show, Generic)

data GetAuthors = GetAuthors
    deriving (Show, Generic)



data GetPosts = GetPosts {
    _gp_creationDate :: Maybe CreationDateOptions,
    _gp_tags :: Maybe TagsOptions,
    _gp_search :: Maybe SearchOptions,
    _gp_sort :: SortOptions
    } deriving (Show, Generic)


defaultGetPosts :: GetPosts
defaultGetPosts = GetPosts Nothing Nothing Nothing defaultSortOptions

requestToAction :: W.Request -> WhoWhat Action
requestToAction req =
  let --queryText = map (bimap E.decodeUtf8 $ fmap E.decodeUtf8) $ W.queryString req
      maybeToken = case W.queryString req of
        ((tokenPar, Just tokenVal):ys) ->
                if tokenPar == "token"
                then Just $ E.decodeUtf8 tokenVal
                else Nothing
        _ -> Nothing
  in  WhoWhat maybeToken $ case W.pathInfo req of 
    (x:xs)
     | x == "posts" -> AGetPosts $ foldr getPostsStep defaultGetPosts $ W.queryString req
    (y:z:zs)
     | y == "categories" && z == "get" -> AGetCategories GetCategories
     | y == "authors" && z == "get" -> AGetAuthors GetAuthors


getPostsStep :: U.QueryItem -> GetPosts -> GetPosts
getPostsStep (_, Nothing) acc = acc
getPostsStep (par, Just val)  acc
  | par == "tag" = case Ae.decode $ BSL.fromStrict val :: Maybe Int of
                Just q -> acc { _gp_tags = Just $ OneTag q }
                _      -> acc
  | par == "tags__in" = f val acc TagsIn
  | par == "tags__all" = f val acc TagsAll
  | par == "created_at" = g val acc Created
  | par == "created_at__lt" = g val acc CreatedEarlier
  | par == "created_at__gt" = g val acc CreatedLater
  | par == "search" = acc { _gp_search = Just $ SearchOptions $ E.decodeUtf8 val }
  | par == "sort" = maybe acc (\x -> acc { _gp_sort = x }) $ sortOptions val
  | otherwise = acc
    -- to be continued
  where f v acc wrap = case Ae.decode $ BSL.fromStrict v :: Maybe [Int] of
                Just q@(x:xs) -> acc { _gp_tags = Just $ wrap q }
                _   -> acc
        g v acc wrap = case Time.parseTimeM True Time.defaultTimeLocale "%Y-%-m-%-d" $ T.unpack $ E.decodeUtf8 v of
                Just x -> acc { _gp_creationDate = Just $ wrap x }
                Nothing -> acc


sortOptions :: BS.ByteString -> Maybe SortOptions
sortOptions = undefined

------------------------ PrettyShow instances ------------------------

instance GP.PrettyShow CreationDateOptions where
--    prettyShow = GP.LStr . GP.gprettyShowSum . from
    prettyShow = GP.LStr . show
instance GP.PrettyShow TagsOptions where
--    prettyShow = GP.LStr . GP.gprettyShowSum . from
    prettyShow = GP.LStr . show
instance GP.PrettyShow SearchOptions where
--    prettyShow = GP.LStr . GP.gprettyShowSum . from
    prettyShow = GP.LStr . show

instance GP.PrettyShow GetCategories where
    prettyShow = GP.LStr . show

instance GP.PrettyShow GetAuthors where
    prettyShow = GP.LStr . show


instance GP.PrettyShow SortEntity where
    prettyShow = GP.LStr . drop 2 . show
instance GP.PrettyShow SortOrder where
    prettyShow = GP.LStr . drop 2 . show

instance GP.PrettyShow SortOptions where
 
instance GP.PrettyShow Action
instance GP.PrettyShow GetPosts


