{-# LANGUAGE DeriveAnyClass #-}
module DBTypes where

import GHC.Generics
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.FromField as PSF
import qualified Database.PostgreSQL.Simple.Types as PST
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified GenericPretty as GP
import Type.Reflection
import General

type UserId = Int
type AuthorId = Int
type PostId = Int
type CategoryId = Int
type TagId = Int
type CommentId = Int


------------ the following datatypes are
------------ supposed to get raw data from database 
----------------------------------------------


data User = User {
    _u_id :: UserId,
    _u_firstname :: T.Text,
    _u_lastname :: T.Text,
    _u_pictureUrl :: Maybe T.Text,
    _u_login :: T.Text,
    _u_passHash :: T.Text,
    _u_creationDate :: Time.Day,
    _u_admin :: Bool
    } deriving (Show, Eq, Generic, PS.FromRow)



data Author = Author {
    _a_authorId :: AuthorId,
    _a_userId :: UserId,
    _a_description :: Maybe T.Text
    } deriving (Show, Eq, Generic)

data Category = Category {
    _cat_categoryId :: CategoryId,
    _cat_parentId :: Maybe CategoryId,
    _cat_description :: T.Text
    } deriving (Show, Eq, Generic)

data Post = Post {
    _p_postId :: PostId,
    _p_title :: T.Text,
    _p_creationDate :: Time.Day,
    _p_authorId :: AuthorId,
    _p_categoryId :: CategoryId,
    _p_content :: T.Text,
    _p_mainPhoto :: Maybe T.Text,
    _p_extraPhotos :: Maybe [T.Text]
    } deriving (Show, Eq, Generic, PS.FromRow)

instance GP.PrettyShow Post
   

data Comment = Comment {
    _com_commentId :: CommentId,
    _com_postId :: PostId,
    _com_content :: T.Text
    } deriving (Show, Eq, Generic)


