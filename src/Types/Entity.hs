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
import DerivingJSON (DropLowerUncap(..), RemovePrefix(..))
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
        { userId :: UserId
        , userFirstname :: Text.Text
        , userLastname :: Text.Text
        , userPictureUrl :: Maybe Text.Text
        , userLogin :: Text.Text
        , userPassHash :: Text.Text
        , userCreationDate :: Time.Day
        , userAdmin :: Maybe Bool
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PSR.FromRow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via DropLowerUncap User

data Author =
    Author
        { aAuthorId :: AuthorId
        , authorDescription :: Maybe Text.Text
        , authorUser :: User
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via DropLowerUncap Author

instance PSR.FromRow Author where
    fromRow = Author <$> PSR.field <*> PSR.field <*> PSR.fromRow

data Category =
    Category
        { cCategoryId :: CategoryId
        , categoryDescription :: Text.Text
        , catParentCategory :: Maybe Category
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via DropLowerUncap Category

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
    cCategoryId : maybe [] getCategoryParents catParentCategory

data Post =
    Post
        { pPostId :: PostId
        , postTitle :: Text.Text
        , postCreationDate :: Time.Day
        , postAuthor :: Author
        , postTags :: [Tag]
        , postCategory :: Category
        , postContent :: Text.Text
        , postMainPhoto :: Maybe Text.Text
        , postExtraPhotos :: Maybe [Text.Text]
        }
  deriving (Show, Eq, Generic)
  deriving anyclass (Gettable, GP.PrettyShow)
  deriving Ae.ToJSON via DropLowerUncap Post

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
        { tTagId :: TagId
        , tTagName :: Text.Text
        }
  deriving (Show, Eq, Generic, GP.PrettyShow, PS.FromRow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via DropLowerUncap Tag

data Comment =
    Comment
        { cCommentId :: CommentId
        , commentContent :: Text.Text
        , commentUser :: User
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via DropLowerUncap Comment

instance PSR.FromRow Comment where
    fromRow = do
        cid <- PSR.field
        content <- PSR.field
        user <- PSR.fromRow
        pure
            Comment
                { cCommentId = cid
                , commentContent = content
                , commentUser = user
                }

data Draft =
    Draft
        { dDraftId :: DraftId
        , draftTitle :: Text.Text
        , draftCreationDate :: Time.Day
        , draftAuthor :: Author
        , draftTags :: [Tag]
        , draftCategory :: Category
        , draftContent :: Text.Text
        , draftMainPhoto :: Maybe Text.Text
        , draftExtraPhotos :: Maybe [Text.Text]
        , draftPostId :: Maybe PostId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)
  deriving anyclass (Gettable)
  deriving Ae.ToJSON via DropLowerUncap Draft

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
        { drDraftId :: DraftId
        , drTitle :: Text.Text
        , drCreationDate :: Time.Day
        , drAuthorId :: Int
        , drCategoryId :: Int
        , drTagIds :: [Int]
        , drContent :: Text.Text
        , drMainPhoto :: Maybe Text.Text
        , drExtraPhotos :: Maybe [Text.Text]
        , drPostId :: Maybe PostId
        }
  deriving (Show, Eq, Generic, GP.PrettyShow)

 -- deriving Ae.ToJSON via RemovePrefix DraftRaw
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
                { drDraftId = did
                , drTitle = title
                , drCreationDate = creationDate
                , drAuthorId = author
                , drCategoryId = categoryId
                , drTagIds = tagIds
                , drContent = content
                , drMainPhoto = photo
                , drExtraPhotos = extraPhotos
                , drPostId = postId
                }
