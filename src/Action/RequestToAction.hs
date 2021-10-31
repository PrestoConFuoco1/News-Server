{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}

module Action.RequestToAction
    ( Action(..)
    , requestToActionHTTP
    , requestToAction
    ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HS (fromList)
import qualified Data.Maybe as Mb (mapMaybe)
import qualified Data.Text as T
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

import qualified Types as Y

data Action
    = AAuthors Y.ActionAuthors
    | ACategory Y.ActionCategory
    | APosts Y.ActionPosts1
    | ATags Y.ActionTags
    | AUsers Y.ActionUsers
    | AAuth Y.Authenticate
    | AComments Y.ActionComments
    | ADrafts Y.ActionDrafts
    | APublish Y.Publish
  deriving (Generic, Show, Eq, GP.PrettyShow)

requestToActionHTTP ::
       W.Request
    -> Either (Y.WhoWhat ActionErrorPerms) (Y.WhoWhat Action)
requestToActionHTTP req =
    let queryString = W.queryString req
        pathInfo = W.pathInfo req
     in requestToAction pathInfo queryString

requestToAction ::
       [T.Text]
    -> [(BS.ByteString, Maybe BS.ByteString)]
    -> Either (Y.WhoWhat ActionErrorPerms) (Y.WhoWhat Action)
requestToAction pathInfo queryString =
    let maybeToken =
            Y.Token <$>
            case queryString of
                ((tokenPar, Just tokenVal):_) ->
                    if tokenPar == "token"
                        then Just $ E.decodeUtf8 tokenVal
                        else Nothing
                _ -> Nothing
        hash :: Query
        hash = HS.fromList . Mb.mapMaybe f $ queryString
        f (x, y) = (x, ) <$> y
     in bimap (Y.WhoWhat maybeToken) (Y.WhoWhat maybeToken) $
        requestToAction1 pathInfo hash

requestToAction1 ::
       [T.Text] -> Query -> Either ActionErrorPerms Action
requestToAction1 path hash =
    case path of
        [x]
            | x == "auth" ->
                AAuth <$>
                runRouter
                    (renv False hash)
                    requestToActionAuthenticate
            | x == "publish" ->
                APublish <$> runRouter (renv False hash) publishAction
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

requestToActionAuthenticate :: Router Y.Authenticate
requestToActionAuthenticate = do
    login <- AU.requireField AU.validateNotEmpty "login"
    passHash <- AU.requireField AU.validateNotEmpty "pass_hash"
    pure $ Y.Authenticate login passHash

requestToActionPosts ::
       [T.Text] -> Query -> Either ActionErrorPerms Y.ActionPosts1
requestToActionPosts path hash =
    case path of
        [x]
            | x == "get" ->
                fmap (Y.AP . Y.Read) $
                runRouter (renv False hash) $
                AU.withPagination getPostsAction
            | otherwise ->
                Left $ ActionErrorPerms False EInvalidEndpoint
        (x:xs) ->
            case AU.readIntText x of
                (Just pid) -> Y.GC <$> actionWithPost pid xs hash
                Nothing -> Left pathNotFound
        [] -> Left pathNotFound

requestToActionDrafts ::
       [T.Text] -> Query -> Either ActionErrorPerms Y.ActionDrafts
requestToActionDrafts path hash =
    case path of
        [x]
            | x == "get" ->
                fmap Y.Read $
                runRouter (renv False hash) $
                AU.withPagination (pure Y.GetDrafts)
            | x == "create" ->
                Y.Create <$>
                runRouter (renv False hash) createDraftToAction
            | x == "edit" ->
                Y.Update <$>
                runRouter (renv False hash) editDraftToAction
            | x == "delete" ->
                Y.Delete <$>
                runRouter (renv False hash) deleteDraftToAction
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionCats ::
       [T.Text] -> Query -> Either ActionErrorPerms Y.ActionCategory
requestToActionCats path hash =
    case path of
        [x]
            | x == "get" ->
                fmap Y.Read $
                runRouter (renv False hash) $
                AU.withPagination (pure Y.GetCategories)
            | x == "create" ->
                Y.Create <$>
                runRouter (renv True hash) createCatsToAction
            | x == "edit" ->
                Y.Update <$>
                runRouter (renv True hash) editCatsToAction
            | x == "delete" ->
                Y.Delete <$>
                runRouter (renv True hash) deleteCatsToAction
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionTags ::
       [T.Text] -> Query -> Either ActionErrorPerms Y.ActionTags
requestToActionTags path hash =
    case path of
        [x]
            | x == "get" ->
                fmap Y.Read $
                runRouter (renv False hash) $
                AU.withPagination (pure Y.GetTags)
            | x == "create" ->
                Y.Create <$>
                runRouter (renv True hash) createTagToAction
            | x == "edit" ->
                Y.Update <$>
                runRouter (renv True hash) editTagToAction
            | x == "delete" ->
                Y.Delete <$>
                runRouter (renv True hash) deleteTagToAction
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionUsers ::
       [T.Text] -> Query -> Either ActionErrorPerms Y.ActionUsers
requestToActionUsers path hash =
    case path of
        [x]
            | x == "profile" -> pure $ Y.Read Y.GetProfile
            | x == "create" ->
                Y.Create <$>
                runRouter (renv False hash) createUserToAction
            | x == "delete" ->
                Y.Delete <$>
                runRouter (renv True hash) deleteUserToAction
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionAuthors ::
       [T.Text] -> Query -> Either ActionErrorPerms Y.ActionAuthors
requestToActionAuthors path hash =
    case path of
        [x]
            | x == "get" ->
                fmap Y.Read $
                runRouter (renv False hash) $
                AU.withPagination (pure $ Y.GetAuthors Nothing)
            | x == "create" ->
                Y.Create <$>
                runRouter (renv True hash) createAuthorToAction
            | x == "delete" ->
                Y.Delete <$>
                runRouter (renv True hash) deleteAuthorToAction
            | x == "edit" ->
                Y.Update <$>
                runRouter (renv True hash) editAuthorToAction
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionComments ::
       [T.Text] -> Query -> Either ActionErrorPerms Y.ActionComments
requestToActionComments path hash =
    case path of
        [x]
            | x == "get" ->
                fmap Y.Read $
                runRouter (renv False hash) $
                AU.withPagination getCommentsToAction
            | x == "create" ->
                Y.Create <$>
                runRouter (renv False hash) createCommentsToAction
            | x == "delete" ->
                Y.Delete <$>
                runRouter (renv False hash) deleteCommentsToAction
        _ -> Left $ ActionErrorPerms False EInvalidEndpoint
