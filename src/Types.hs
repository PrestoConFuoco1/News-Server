{-# LANGUAGE DeriveAnyClass #-}
module Types where

import GHC.Generics
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.FromField as PSF
import qualified Database.PostgreSQL.Simple.FromRow as PSR
import qualified Database.PostgreSQL.Simple.Types as PST
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified GenericPretty as GP
import qualified Data.Aeson as Ae
import Type.Reflection
import General


type UserId = Int
type AuthorId = Int
type PostId = Int
type CategoryId = Int
type TagId = Int
type CommentId = Int



data User = User {
    _u_id :: UserId,
    _u_firstname :: T.Text,
    _u_lastname :: T.Text,
    _u_pictureUrl :: Maybe T.Text,
    _u_login :: T.Text,
    _u_passHash :: T.Text,
    _u_creationDate :: Time.Day,
    _u_admin :: Bool
    } deriving (Show, Eq, Generic, PS.FromRow, GP.PrettyShow)



data Author = Author {
    _a_authorId :: AuthorId,
    _a_user :: User,
    _a_description :: Maybe T.Text
    } deriving (Show, Eq, Generic, PSF.FromField, GP.PrettyShow)

data Category = Category {
    _cat_categoryId :: CategoryId,
    _cat_description :: T.Text,
    _cat_childCategory :: Maybe Category
    } deriving (Show, Eq, Generic, GP.PrettyShow)

instance Ae.ToJSON Category where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }

instance PSR.FromRow Category where
    fromRow = fmap listToCategory $ zip <$> PSR.field <*> PSR.field

--instance PSF.FromField Category where
--    fromField f b = fmap listToCategory $ PSF.fromField f b

defaultCategoryId = 1 :: CategoryId


listToCategory :: [(CategoryId, T.Text)] -> Category
listToCategory [] = Category defaultCategoryId "default" Nothing
listToCategory ((catId, txt):xs) = Category catId txt $ foldr f Nothing xs
    where f :: (CategoryId, T.Text) -> Maybe Category -> Maybe Category
          f (cid, t) cat = Just $ Category cid t cat



data Post = Post {
    _p_postId :: PostId,
    _p_title :: T.Text,
    _p_creationDate :: Time.Day,
    _p_authorId :: Author,
    _p_categoryId :: Category,
    _p_content :: T.Text,
    _p_mainPhoto :: Maybe T.Text,
    _p_extraPhotos :: Maybe [T.Text]
    } deriving (Show, Eq, Generic)

instance GP.PrettyShow Post

data Comment = Comment {
    _com_commentId :: CommentId,
    _com_postId :: PostId,
    _com_content :: T.Text
    } deriving (Show, Eq, Generic)


