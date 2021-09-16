{-# LANGUAGE DeriveAnyClass #-}
module RequestToAction where

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
import ActionTypes


requestToAction :: W.Request -> WhoWhat Action
requestToAction req =
  let
    queryString = W.queryString req
    pathInfo = W.pathInfo req
    maybeToken = case queryString of
        ((tokenPar, Just tokenVal):ys) ->
                if tokenPar == "token"
                then Just $ E.decodeUtf8 tokenVal
                else Nothing
        _ -> Nothing
    hash :: Query
    hash = HS.fromList . Mb.catMaybes . map f $ W.queryString req
    f (x, y) = fmap ((,) x) y
  in  WhoWhat maybeToken $ requestToAction' pathInfo hash

invalidEP = AError EInvalidEndpoint

requestToAction' :: [T.Text] -> Query -> Action
requestToAction' path hash = case path of 
    (x:xs)
     | x == "posts"      -> requestToActionPosts xs hash
     | x == "categories" -> requestToActionCats xs hash
     | x == "authors"    -> requestToActionAuthors xs hash
     | x == "tags"       -> requestToActionTags xs hash
     | x == "users"      -> requestToActionUsers xs hash
    (y:z:zs)
   --  | y == "categories" && z == "get" -> AGetCategories GetCategories
   --  | y == "authors" && z == "get" -> AGetAuthors GetAuthors
   --  | y == "tags" && z == "get" -> AGetTags GetTags
 --    | y == "users" && z == "get" -> AGetUser GetUser
     | otherwise -> invalidEP

requestToActionPosts :: [T.Text] -> Query -> Action
requestToActionPosts path hash = case path of
  (x:xs)
    | x == "get" -> AGetPosts $ getPostsAction hash
    | otherwise -> case readIntText x of
        (Just id) -> actionWithPost id xs hash
        Nothing -> invalidEP
  [] -> invalidEP

actionWithPost :: Int -> [T.Text] -> Query -> Action
actionWithPost id path hash = case path of
  (x:xs)
    | x == "comments" -> AGetComments $ GetComments id
  [] -> invalidEP

requestToActionCats :: [T.Text] -> Query -> Action
requestToActionCats path hash = case path of
  (x:xs)
    | x == "get" -> AGetCategories GetCategories
    | x == "create" -> either AError ACreateCategory $ createCatsToAction hash
  [] -> invalidEP

createCatsToAction :: Query -> Either ActionError CreateCategory
createCatsToAction hash = do
--    name <- maybe (Left . ERequiredFieldMissing $ "name") Right
--                $ requireText hash "name"
    name <- requireField (requireText hash) "name"
    parentId <- maybe (Left . ERequiredFieldMissing $ "parent_id") Right
                $ requireInt hash "parent_id"
    return $ CreateCategory name parentId

 
requestToActionUsers :: [T.Text] -> Query -> Action
requestToActionUsers path hash = case path of
  (x:xs)
 --   | x == "get" -> AGetCategories GetCategories
    | x == "create" -> either AError ACreateUser $ createUserToAction hash
  [] -> invalidEP

createUserToAction :: Query -> Either ActionError CreateUser
createUserToAction hash = do
    login <- requireField (requireText hash) "login"
    passHash <- requireField (requireText hash) "pass_hash"
    firstName <- requireField (requireText hash) "firstname"
    lastName <- requireField (requireText hash) "lastname"
    return $ CreateUser login passHash firstName lastName




requireField :: (BS.ByteString -> Maybe a) -> BS.ByteString -> Either ActionError a
requireField func fieldname =
    maybe (Left $ ERequiredFieldMissing fieldname) Right $ func fieldname

requestToActionAuthors :: [T.Text] -> Query -> Action
requestToActionAuthors path hash = case path of
  (x:xs)
    | x == "get" -> AGetAuthors GetAuthors
  [] -> invalidEP


requestToActionTags :: [T.Text] -> Query -> Action
requestToActionTags path hash = case path of
  (x:xs)
    | x == "get" -> AGetTags GetTags
  [] -> invalidEP


type Query = HS.HashMap BS.ByteString BS.ByteString

getPostsAction :: Query -> GetPosts
getPostsAction qu =
    let tagopts = foldr (<|>) Nothing
                           [fmap OneTag $ requireInt qu "tag",
                            fmap TagsIn $ requireIntList qu "tags__in",
                            fmap TagsAll $ requireIntList qu "tags__all"]
        creationopts = foldr (<|>) Nothing $
                       [fmap Created $ requireDay qu "created_at",
                        fmap CreatedEarlier $ requireDay qu "created_at__lt",
                        fmap CreatedLater   $ requireDay qu "created_at__gt"]
        sortopts = maybe defaultSortOptions id
                        $ (HS.lookup "sort" qu >>= sortOptions)
        searchopts = fmap SearchOptions $ requireText qu "search"
    in  GetPosts creationopts tagopts searchopts sortopts







require :: (BS.ByteString -> Maybe a) -> Query -> BS.ByteString -> Maybe a
require prse qu arg = HS.lookup arg qu >>= prse

requireText :: Query -> BS.ByteString -> Maybe T.Text
requireText = require (pure . E.decodeUtf8)

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

instance GP.PrettyShow GetTags where
    prettyShow = GP.LStr . show

instance GP.PrettyShow ActionError where
    prettyShow = GP.LStr . show

instance GP.PrettyShow GetComments where
    prettyShow = GP.LStr . show

instance GP.PrettyShow SortEntity where
    prettyShow = GP.LStr . drop 2 . show
instance GP.PrettyShow SortOrder where
    prettyShow = GP.LStr . drop 2 . show

instance GP.PrettyShow SortOptions where
 
instance GP.PrettyShow Action
instance GP.PrettyShow GetPosts


