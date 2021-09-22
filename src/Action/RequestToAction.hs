{-# LANGUAGE DeriveAnyClass #-}
module Action.RequestToAction where

import Action.Types
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

import Action.Authors
import Action.Category
import Action.Posts
import Action.Tags
import Action.Users
import Action.Comments
import Action.Draft

import Action.Utils
import Action.Common


data Action = AAuthors ActionAuthors
            | ACategory ActionCategory
            | APosts ActionPosts1
            | ATags  ActionTags
            | AUsers ActionUsers
            | AAuth  Authenticate
            | AComments ActionComments
            | ADrafts ActionDrafts
            | APublish Publish
    deriving (Generic, Show)



requestToAction :: W.Request -> Either (WhoWhat ActionErrorPerms) (WhoWhat Action)
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
  in  bimap (WhoWhat maybeToken) (WhoWhat maybeToken) $ requestToAction' pathInfo hash


requestToAction' :: [T.Text] -> Query -> Either ActionErrorPerms Action
requestToAction' path hash = case path of 
    (x:xs)
     | x == "auth"       -> fmap AAuth $ runRouter (renv False hash) requestToActionAuthenticate
     | x == "posts"      -> fmap APosts $ requestToActionPosts xs hash
     | x == "drafts"     -> fmap ADrafts $ requestToActionDrafts xs hash
     | x == "publish"    -> fmap APublish $ requestToActionPublish xs hash
     | x == "categories" -> fmap ACategory $ requestToActionCats xs hash
     | x == "authors"    -> fmap AAuthors $ requestToActionAuthors xs hash
     | x == "tags"       -> fmap ATags $ requestToActionTags xs hash
     | x == "users"      -> fmap AUsers $ requestToActionUsers xs hash
     | x == "comments"   -> fmap AComments $ requestToActionComments xs hash
    []
     | otherwise -> Left  $ ActionErrorPerms False EInvalidEndpoint



requestToActionAuthenticate :: Router Authenticate
requestToActionAuthenticate = do
    login <- requireField validateNotEmpty "login"
    passHash <- requireField validateNotEmpty "pass_hash"
    return $ Authenticate login passHash

requestToActionPosts :: [T.Text] -> Query -> Either ActionErrorPerms ActionPosts1
requestToActionPosts path hash = case path of
  (x:xs)
    | x == "get" -> fmap AP $ runRouter (renv False hash) getPostsAction
    | otherwise -> case readIntText x of
        (Just id) -> fmap GC $ actionWithPost id xs hash
        Nothing -> Left  $ ActionErrorPerms False EInvalidEndpoint
  [] -> Left  $ ActionErrorPerms False EInvalidEndpoint

requestToActionDrafts :: [T.Text] -> Query -> Either ActionErrorPerms ActionDrafts
requestToActionDrafts path hash = case path of
  (x:xs)
    | x == "get" -> Right $ Read GetDrafts
    | x == "create" -> fmap Create $ runRouter (renv False hash) $ createDraftToAction
    | x == "edit" -> fmap Update $ runRouter (renv False hash) $ editDraftToAction
    | x == "delete" -> fmap Delete $ runRouter (renv False hash) $ deleteDraftToAction
  [] -> Left  $ ActionErrorPerms False EInvalidEndpoint

requestToActionPublish :: [T.Text] -> Query -> Either ActionErrorPerms Publish
requestToActionPublish path hash = case path of
    (x:xs) -> Left  $ ActionErrorPerms False EInvalidEndpoint
    [] -> runRouter (renv False hash) $ publishAction



requestToActionCats :: [T.Text] -> Query -> Either ActionErrorPerms ActionCategory
requestToActionCats path hash = case path of
  x:[]
    | x == "get" -> Right $ Read GetCategories
    | x == "create" -> fmap Create $ runRouter (renv True hash) $ createCatsToAction
    | x == "edit" -> fmap Update $ runRouter (renv True hash) $ editCatsToAction
    | x == "delete" -> fmap Delete $ runRouter (renv True hash) $ deleteCatsToAction
  _ -> Left  $ ActionErrorPerms False EInvalidEndpoint




requestToActionTags :: [T.Text] -> Query -> Either ActionErrorPerms ActionTags
requestToActionTags path hash = case path of
  (x:xs)
    | x == "get" -> pure $ Read GetTags
    | x == "create" -> fmap Create $ runRouter (renv True hash) $ createTagToAction
    | x == "edit" -> fmap Update $ runRouter (renv True hash) $ editTagToAction
    | x == "delete" -> fmap Delete $ runRouter (renv True hash) $ deleteTagToAction
  [] -> Left  $ ActionErrorPerms False EInvalidEndpoint




requestToActionUsers :: [T.Text] -> Query -> Either ActionErrorPerms ActionUsers
requestToActionUsers path hash = case path of
  (x:[])
    | x == "profile" -> return $ Read GetProfile
    | x == "create" -> fmap Create $ runRouter (renv False hash) $ createUserToAction
    | x == "delete" -> fmap Delete $ runRouter (renv True hash) $ deleteUserToAction
  _ -> Left  $ ActionErrorPerms False EInvalidEndpoint

requestToActionAuthors :: [T.Text] -> Query -> Either ActionErrorPerms ActionAuthors
requestToActionAuthors path hash = case path of
  x:[]
    | x == "get" -> Right $ Read $ GetAuthors Nothing
    | x == "create" -> fmap Create $ runRouter (renv True hash) $ createAuthorToAction
    | x == "delete" -> fmap Delete $ runRouter (renv True hash) $ deleteAuthorToAction
    | x == "edit" -> fmap Update $ runRouter (renv True hash) $ editAuthorToAction
  _ -> Left  $ ActionErrorPerms False EInvalidEndpoint



requestToActionComments :: [T.Text] -> Query -> Either ActionErrorPerms ActionComments
requestToActionComments path hash = case path of
    x:[]
      | x == "get" -> fmap Read $ runRouter (renv False hash) $ getCommentsToAction
      | x == "create" -> fmap Create $ runRouter (renv False hash) $ createCommentsToAction
      | x == "delete" -> fmap Delete $ runRouter (renv False hash) $ deleteCommentsToAction
    _ -> Left $ ActionErrorPerms False EInvalidEndpoint


instance GP.PrettyShow Action

