{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}

module Action.RequestToAction
    ( Action(..)
    , requestToActionHTTP
    , requestToAction
    ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HS (fromList, lookup)
import qualified Data.Maybe as Mb (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E (decodeUtf8)
import qualified Network.Wai as W (Request, pathInfo, queryString)

import Data.Bifunctor (bimap)
import GHC.Generics
import qualified GenericPretty as GP

import Action.Authors
import Action.Category
import Action.Comments
import Action.Draft
import Action.Posts
import Action.Tags
import Action.Users

import Action.Common
import qualified Action.Utils as AU

import qualified Types as T

data Action
    = AAuthors T.ActionAuthors
    | ACategory T.ActionCategory
    | APosts T.ActionPosts1
    | ATags T.ActionTags
    | AUsers T.ActionUsers
    | AAuth T.Authenticate
    | AComments T.ActionComments
    | ADrafts T.ActionDrafts
    | APublish T.Publish
  deriving (Generic, Show, Eq, GP.PrettyShow)

requestToActionHTTP ::
       W.Request
    -> Either (T.WhoWhat ActionErrorPerms) (T.WhoWhat Action)
requestToActionHTTP req =
    let queryString = W.queryString req
        pathInfo = W.pathInfo req
     in requestToAction pathInfo queryString

requestToAction ::
       [Text.Text]
    -> [(BS.ByteString, Maybe BS.ByteString)]
    -> Either (T.WhoWhat ActionErrorPerms) (T.WhoWhat Action)
requestToAction pathInfo queryString =
    let hash :: Query
        hash = HS.fromList . Mb.mapMaybe f $ queryString
        maybeToken = extractToken hash
        f (x, y) = (x, ) <$> y
     in bimap (T.WhoWhat maybeToken) (T.WhoWhat maybeToken) $
        requestToAction1 pathInfo hash

extractToken :: Query -> Maybe T.Token
extractToken = fmap (T.Token . E.decodeUtf8) . HS.lookup "token"

requestToAction1 ::
       [Text.Text] -> Query -> Either ActionErrorPerms Action
requestToAction1 path hash =
    case path of
        [x]
            | x == "auth" ->
                AAuth <$>
                runRouter
                    (routingEnv False hash)
                    requestToActionAuthenticate
            | x == "publish" ->
                APublish <$>
                runRouter (routingEnv False hash) publishAction
            | otherwise -> Left pathNotFound
        (x:xs)
            | x == "posts" -> APosts <$> requestToActionPosts xs hash
            | x == "drafts" ->
                ADrafts <$> requestToActionDrafts xs hash
            | x == "categories" ->
                ACategory <$> requestToActionCats xs hash
            | x == "authors" ->
                AAuthors <$> requestToActionAuthors xs hash
            | x == "tags" -> ATags <$> requestToActionTags xs hash
            | x == "users" -> AUsers <$> requestToActionUsers xs hash
            | x == "comments" ->
                AComments <$> requestToActionComments xs hash
            | otherwise -> Left pathNotFound
        []
            | otherwise -> Left pathNotFound

requestToActionAuthenticate :: Router T.Authenticate
requestToActionAuthenticate = do
    login <- AU.requireField AU.validateNotEmpty "login"
    passHash <- AU.requireField AU.validateNotEmpty "pass_hash"
    pure $ T.Authenticate login passHash

requestToActionPosts ::
       [Text.Text] -> Query -> Either ActionErrorPerms T.ActionPosts1
requestToActionPosts path hash =
    case path of
        [x]
            | x == "get" ->
                fmap (T.AP . T.Read) $
                runRouter (routingEnv False hash) $
                AU.withPagination getPostsAction
            | otherwise ->
                Left $ ActionErrorPerms False EInvalidEndpoint
        (x:xs) ->
            case AU.readIntText x of
                (Just pid) -> T.GC <$> actionWithPost pid xs hash
                Nothing -> Left pathNotFound
        [] -> Left pathNotFound

requestToActionDrafts ::
       [Text.Text] -> Query -> Either ActionErrorPerms T.ActionDrafts
requestToActionDrafts path hash =
    case path of
        [x]
            | x == "get" ->
                fmap T.Read $
                runRouter (routingEnv False hash) $
                AU.withPagination (pure T.GetDrafts)
            | x == "create" ->
                T.Create <$>
                runRouter (routingEnv False hash) createDraftToAction
            | x == "edit" ->
                T.Update <$>
                runRouter (routingEnv False hash) editDraftToAction
            | x == "delete" ->
                T.Delete <$>
                runRouter (routingEnv False hash) deleteDraftToAction
        _ -> Left pathNotFound

requestToActionCats ::
       [Text.Text]
    -> Query
    -> Either ActionErrorPerms T.ActionCategory
requestToActionCats path hash =
    case path of
        [x]
            | x == "get" ->
                fmap T.Read $
                runRouter (routingEnv False hash) $
                AU.withPagination (pure $ T.GetCategories Nothing)
            | x == "create" ->
                T.Create <$>
                runRouter (routingEnv True hash) createCatsToAction
            | x == "edit" ->
                T.Update <$>
                runRouter (routingEnv True hash) editCatsToAction
            | x == "delete" ->
                T.Delete <$>
                runRouter (routingEnv True hash) deleteCatsToAction
        _ -> Left pathNotFound

requestToActionTags ::
       [Text.Text] -> Query -> Either ActionErrorPerms T.ActionTags
requestToActionTags path hash =
    case path of
        [x]
            | x == "get" ->
                fmap T.Read $
                runRouter (routingEnv False hash) $
                AU.withPagination (pure T.GetTags)
            | x == "create" ->
                T.Create <$>
                runRouter (routingEnv True hash) createTagToAction
            | x == "edit" ->
                T.Update <$>
                runRouter (routingEnv True hash) editTagToAction
            | x == "delete" ->
                T.Delete <$>
                runRouter (routingEnv True hash) deleteTagToAction
        _ -> Left pathNotFound

requestToActionUsers ::
       [Text.Text] -> Query -> Either ActionErrorPerms T.ActionUsers
requestToActionUsers path hash =
    case path of
        [x]
            | x == "profile" -> pure $ T.Read T.GetProfile
            | x == "create" ->
                T.Create <$>
                runRouter (routingEnv False hash) createUserToAction
            | x == "delete" ->
                T.Delete <$>
                runRouter (routingEnv True hash) deleteUserToAction
        _ -> Left pathNotFound

requestToActionAuthors ::
       [Text.Text] -> Query -> Either ActionErrorPerms T.ActionAuthors
requestToActionAuthors path hash =
    case path of
        [x]
            | x == "get" ->
                fmap T.Read $
                runRouter (routingEnv False hash) $
                AU.withPagination (pure $ T.GetAuthors Nothing)
            | x == "create" ->
                T.Create <$>
                runRouter (routingEnv True hash) createAuthorToAction
            | x == "delete" ->
                T.Delete <$>
                runRouter (routingEnv True hash) deleteAuthorToAction
            | x == "edit" ->
                T.Update <$>
                runRouter (routingEnv True hash) editAuthorToAction
        _ -> Left pathNotFound

requestToActionComments ::
       [Text.Text]
    -> Query
    -> Either ActionErrorPerms T.ActionComments
requestToActionComments path hash =
    case path of
        [x]
            | x == "get" ->
                fmap T.Read $
                runRouter (routingEnv False hash) $
                AU.withPagination getCommentsToAction
            | x == "create" ->
                T.Create <$>
                runRouter
                    (routingEnv False hash)
                    createCommentsToAction
            | x == "delete" ->
                T.Delete <$>
                runRouter
                    (routingEnv False hash)
                    deleteCommentsToAction
        _ -> Left pathNotFound
