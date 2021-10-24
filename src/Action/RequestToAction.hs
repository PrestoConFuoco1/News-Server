{-# LANGUAGE TupleSections #-}
module Action.RequestToAction (
    Action(..), requestToActionHTTP, requestToAction
) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HS (fromList)
import qualified Data.Maybe as Mb (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (decodeUtf8)
import qualified Network.Wai as W
   ( Request
   , pathInfo
   , queryString
   )

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
import Action.Utils

import Types

data Action
   = AAuthors ActionAuthors
   | ACategory ActionCategory
   | APosts ActionPosts1
   | ATags ActionTags
   | AUsers ActionUsers
   | AAuth Authenticate
   | AComments ActionComments
   | ADrafts ActionDrafts
   | APublish Publish
   deriving (Generic, Show, Eq)

requestToActionHTTP ::
      W.Request
   -> Either (WhoWhat ActionErrorPerms) (WhoWhat Action)
requestToActionHTTP req =
   let queryString = W.queryString req
       pathInfo = W.pathInfo req
    in requestToAction pathInfo queryString

requestToAction ::
      [T.Text]
   -> [(BS.ByteString, Maybe BS.ByteString)]
   -> Either (WhoWhat ActionErrorPerms) (WhoWhat Action)
requestToAction pathInfo queryString =
   let maybeToken = Token <$>
          case queryString of
             ((tokenPar, Just tokenVal):_) ->
                if tokenPar == "token"
                   then Just $ E.decodeUtf8 tokenVal
                   else Nothing
             _ -> Nothing
       hash :: Query
       hash = HS.fromList . Mb.mapMaybe f $ queryString
       f (x, y) = (x,) <$> y
    in bimap (WhoWhat maybeToken) (WhoWhat maybeToken) $
       requestToAction2 pathInfo hash

requestToAction2 ::
      [T.Text] -> Query -> Either ActionErrorPerms Action
requestToAction2 path hash =
   case path of
      [x]
         | x == "auth" ->
            AAuth <$>
            runRouter
               (renv False hash)
               requestToActionAuthenticate
         | x == "publish" ->
            APublish <$>
            runRouter (renv False hash) publishAction
         | otherwise -> Left pathNotFound
      (x:xs)
         | x == "posts" ->
            APosts <$> requestToActionPosts xs hash
         | x == "drafts" ->
            ADrafts <$> requestToActionDrafts xs hash
         | x == "categories" ->
            ACategory <$> requestToActionCats xs hash
         | x == "authors" ->
            AAuthors <$> requestToActionAuthors xs hash
         | x == "tags" ->
            ATags <$> requestToActionTags xs hash
         | x == "users" ->
            AUsers <$> requestToActionUsers xs hash
         | x == "comments" ->
            AComments <$> requestToActionComments xs hash
         | otherwise -> Left pathNotFound
      []
         | otherwise -> Left pathNotFound

requestToActionAuthenticate :: Router Authenticate
requestToActionAuthenticate = do
   login <- requireField validateNotEmpty "login"
   passHash <- requireField validateNotEmpty "pass_hash"
   pure $ Authenticate login passHash

requestToActionPosts ::
      [T.Text]
   -> Query
   -> Either ActionErrorPerms ActionPosts1
requestToActionPosts path hash =
   case path of
      [x]
         | x == "get" ->
            fmap (AP . Read) $
            runRouter (renv False hash) $
            withPagination getPostsAction
         | otherwise ->
            Left $ ActionErrorPerms False EInvalidEndpoint
      (x:xs) ->
         case readIntText x of
            (Just pid) -> GC <$> actionWithPost pid xs hash
            Nothing -> Left pathNotFound
      [] -> Left pathNotFound

requestToActionDrafts ::
      [T.Text]
   -> Query
   -> Either ActionErrorPerms ActionDrafts
requestToActionDrafts path hash =
   case path of
      [x]
         | x == "get" ->
            fmap Read $
            runRouter (renv False hash) $
            withPagination (pure GetDrafts)
         | x == "create" ->
            Create <$>
            runRouter (renv False hash) createDraftToAction
         | x == "edit" ->
            Update <$>
            runRouter (renv False hash) editDraftToAction
         | x == "delete" ->
            Delete <$>
            runRouter (renv False hash) deleteDraftToAction
      _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionCats ::
      [T.Text]
   -> Query
   -> Either ActionErrorPerms ActionCategory
requestToActionCats path hash =
   case path of
      [x]
         | x == "get" ->
            fmap Read $
            runRouter (renv False hash) $
            withPagination (pure GetCategories)
         | x == "create" ->
            Create <$>
            runRouter (renv True hash) createCatsToAction
         | x == "edit" ->
            Update <$>
            runRouter (renv True hash) editCatsToAction
         | x == "delete" ->
            Delete <$>
            runRouter (renv True hash) deleteCatsToAction
      _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionTags ::
      [T.Text]
   -> Query
   -> Either ActionErrorPerms ActionTags
requestToActionTags path hash =
   case path of
      [x]
         | x == "get" ->
            fmap Read $
            runRouter (renv False hash) $
            withPagination (pure GetTags)
         | x == "create" ->
            Create <$>
            runRouter (renv True hash) createTagToAction
         | x == "edit" ->
            Update <$>
            runRouter (renv True hash) editTagToAction
         | x == "delete" ->
            Delete <$>
            runRouter (renv True hash) deleteTagToAction
      _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionUsers ::
      [T.Text]
   -> Query
   -> Either ActionErrorPerms ActionUsers
requestToActionUsers path hash =
   case path of
      [x]
         | x == "profile" -> pure $ Read GetProfile
         | x == "create" ->
            Create <$>
            runRouter (renv False hash) createUserToAction
         | x == "delete" ->
            Delete <$>
            runRouter (renv True hash) deleteUserToAction
      _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionAuthors ::
      [T.Text]
   -> Query
   -> Either ActionErrorPerms ActionAuthors
requestToActionAuthors path hash =
   case path of
      [x]
         | x == "get" ->
            fmap Read $
            runRouter (renv False hash) $
            withPagination (pure $ GetAuthors Nothing)
         | x == "create" ->
            Create <$>
            runRouter (renv True hash) createAuthorToAction
         | x == "delete" ->
            Delete <$>
            runRouter (renv True hash) deleteAuthorToAction
         | x == "edit" ->
            Update <$>
            runRouter (renv True hash) editAuthorToAction
      _ -> Left $ ActionErrorPerms False EInvalidEndpoint

requestToActionComments ::
      [T.Text]
   -> Query
   -> Either ActionErrorPerms ActionComments
requestToActionComments path hash =
   case path of
      [x]
         | x == "get" ->
            fmap Read $
            runRouter (renv False hash) $
            withPagination getCommentsToAction
         | x == "create" ->
            Create <$>
            runRouter
               (renv False hash)
               createCommentsToAction
         | x == "delete" ->
            Delete <$>
            runRouter
               (renv False hash)
               deleteCommentsToAction
      _ -> Left $ ActionErrorPerms False EInvalidEndpoint

instance GP.PrettyShow Action
