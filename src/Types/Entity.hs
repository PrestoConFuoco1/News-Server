{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Entity
    ( Entity(..)
    , Gettable
    , User(..)
    , showEText
    , Tag(..)
    , Comment(..)
    , Category(..)
    , Draft(..)
    , DraftRaw(..)
    , Author(..)
    , Post(..)
    , getCategoryParents
    ) where

import qualified Data.Aeson as Ae
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.FromRow as PSR
import DerivingJSON (RemovePrefix(..))
import GHC.Generics
import qualified GenericPretty as GP
import Types.Authors (AuthorId)
import Types.Category (CategoryId)
import Types.Draft (DraftId)
import Types.Posts (CommentId, PostId)
import Types.Tags (TagId)
import Types.Users (UserId)
import qualified Utils as S

data Entity
    = EUser
    | EAuthor
    | ECategory
    | ETag
    | EComment
    | EDraft
    | EPost
  deriving (Show, Eq)

showE :: Entity -> String
showE x = S.unCap $ drop 1 $ show x

showEText :: Entity -> Text.Text
showEText = Text.pack . showE

class (Ae.ToJSON a, Show a, GP.PrettyShow a) => Gettable a


data User =
    User
        { _u_id :: UserId
        , _u_firstname :: Text.Text
        , _u_lastname :: Text.Text
        , _u_pictureUrl :: Maybe Text.Text
        , _u_login :: Text.Text
        , _u_passHash :: Text.Text
        , _u_creationDate :: Time.Day
        , _u_admin :: Maybe Bool
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PSR.FromRow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via RemovePrefix User

data Author =
    Author
        { _a_authorId :: AuthorId
        , _a_description :: Maybe Text.Text
        , _a_user :: User
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via RemovePrefix Author

instance PSR.FromRow Author where
    fromRow = Author <$> PSR.field <*> PSR.field <*> PSR.fromRow

data Category =
    Category
        { _cat_categoryId :: CategoryId
        , _cat_description :: Text.Text
        , _cat_parentCategory :: Maybe Category
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via RemovePrefix Category

instance PSR.FromRow Category where
    fromRow = fmap listToCategory $ zip <$> PSR.field <*> PSR.field

defaultCategoryId :: CategoryId
defaultCategoryId = 1

listToCategory :: [(CategoryId, Text.Text)] -> Category
listToCategory [] = Category defaultCategoryId "default" Nothing
listToCategory ((catId, txt):xs) =
    Category catId txt $ foldr f Nothing xs where
    f (cid, t) cat = Just $ Category cid t cat

getCategoryParents :: Category -> [CategoryId]
getCategoryParents Category {..} =
    _cat_categoryId : maybe [] getCategoryParents _cat_parentCategory

data Post =
    Post
        { _p_postId :: PostId
        , _p_title :: Text.Text
        , _p_creationDate :: Time.Day
        , _p_author :: Author
        , _p_tags :: [Tag]
        , _p_category :: Category
        , _p_content :: Text.Text
        , _p_mainPhoto :: Maybe Text.Text
        , _p_extraPhotos :: Maybe [Text.Text]
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (Gettable, GP.PrettyShow)
  deriving Ae.ToJSON via RemovePrefix Post

instance PSR.FromRow Post where
    fromRow = do
        pid <- PSR.field
        title <- PSR.field
        creationDate <- PSR.field
        author <- PSR.fromRow
        tagIds <- PSR.field
        tagNames <- PSR.field
        catIds <- PSR.field
        catNames <- PSR.field
        content <- PSR.field
        photo <- PSR.field
        extraPhotos <- PSR.field
        let category = listToCategory $ zip catIds catNames
            tags = zipWith Tag tagIds tagNames
        pure $
            Post
                pid
                title
                creationDate
                author
                tags
                category
                content
                photo
                extraPhotos

data Tag =
    Tag
        { _t_tagId :: TagId
        , _t_tagName :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.FromRow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via RemovePrefix Tag

data Comment =
    Comment
        { _com_commentId :: CommentId
        , _com_content :: Text.Text
        , _com_user :: User
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via RemovePrefix Comment

instance PSR.FromRow Comment where
    fromRow = do
        cid <- PSR.field
        content <- PSR.field
        user <- PSR.fromRow
        pure
            Comment
                { _com_commentId = cid
                , _com_content = content
                , _com_user = user
                }

data Draft =
    Draft
        { _d_draftId :: DraftId
        , _d_title :: Text.Text
        , _d_creationDate :: Time.Day
        , _d_author :: Author
        , _d_tags :: [Tag]
        , _d_category :: Category
        , _d_content :: Text.Text
        , _d_mainPhoto :: Maybe Text.Text
        , _d_extraPhotos :: Maybe [Text.Text]
        , _d_postId :: Maybe PostId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via RemovePrefix Draft

instance PSR.FromRow Draft where
    fromRow = do
        did <- PSR.field
        title <- PSR.field
        creationDate <- PSR.field
        author <- PSR.fromRow
        tagIds <- PSR.field
        tagNames <- PSR.field
        catIds <- PSR.field
        catNames <- PSR.field
        content <- PSR.field
        photo <- PSR.field
        extraPhotos <- PSR.field
        postId <- PSR.field
        let category = listToCategory $ zip catIds catNames
            tags = zipWith Tag tagIds tagNames
        pure $
            Draft
                did
                title
                creationDate
                author
                tags
                category
                content
                photo
                extraPhotos
                postId

data DraftRaw =
    DraftRaw
        { _dr_draftId :: DraftId
        , _dr_title :: Text.Text
        , _dr_creationDate :: Time.Day
        , _dr_authorId :: Int
        , _dr_categoryId :: Int
        , _dr_tagIds :: [Int]
        , _dr_content :: Text.Text
        , _dr_mainPhoto :: Maybe Text.Text
        , _dr_extraPhotos :: Maybe [Text.Text]
        , _dr_postId :: Maybe PostId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving Ae.ToJSON via RemovePrefix DraftRaw

instance PSR.FromRow DraftRaw where
    fromRow = do
        did <- PSR.field
        title <- PSR.field
        creationDate <- PSR.field
        author <- PSR.field
        categoryId <- PSR.field
        tagIds <- PSR.field
        content <- PSR.field
        photo <- PSR.field
        extraPhotos <- PSR.field
        postId <- PSR.field
        pure
            DraftRaw
                { _dr_draftId = did
                , _dr_title = title
                , _dr_creationDate = creationDate
                , _dr_authorId = author
                , _dr_categoryId = categoryId
                , _dr_tagIds = tagIds
                , _dr_content = content
                , _dr_mainPhoto = photo
                , _dr_extraPhotos = extraPhotos
                , _dr_postId = postId
                }
