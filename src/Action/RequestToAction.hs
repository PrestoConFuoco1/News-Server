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

import Action.Authors.Types
import Action.Authors.Authors
import Action.Category.Types
import Action.Category.Category
import Action.Posts.Types
import Action.Posts.Posts
import Action.Tags.Types
import Action.Tags.Tags
import Action.Users.Types
import Action.Users.Users

import Action.Utils
import Action.Common


data Action = AAuthors ActionAuthors
            | ACategory ActionCategory
            | APosts ActionPosts
            | ATags  ActionTags
            | AUsers ActionUsers
    deriving (Generic)

requestToAction :: W.Request -> Either ActionError (WhoWhat Action)
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
  in  fmap (WhoWhat maybeToken) $ requestToAction' pathInfo hash


requestToAction' :: [T.Text] -> Query -> Either ActionError Action
requestToAction' path hash = case path of 
    (x:xs)
     | x == "posts"      -> fmap APosts $ requestToActionPosts xs hash
     | x == "categories" -> fmap ACategory $ requestToActionCats xs hash
     | x == "authors"    -> fmap AAuthors $ requestToActionAuthors xs hash
     | x == "tags"       -> fmap ATags $ requestToActionTags xs hash
     | x == "users"      -> fmap AUsers $ requestToActionUsers xs hash
    (y:z:zs)
     | otherwise -> Left EInvalidEndpoint



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

--instance GP.PrettyShow GetComments where
--    prettyShow = GP.LStr . show

instance GP.PrettyShow SortEntity where
    prettyShow = GP.LStr . drop 2 . show
instance GP.PrettyShow SortOrder where
    prettyShow = GP.LStr . drop 2 . show

instance GP.PrettyShow SortOptions where
 
instance GP.PrettyShow GetPosts

instance GP.PrettyShow Action
