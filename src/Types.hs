{-# LANGUAGE DeriveAnyClass #-}
module Types (
Token,
WhoWhat(..),
CRUD(..),
Paginated(..),
CategoryId,
ActionCategory, 
GetCategories(..),
CreateCategory(..),
EditCategory(..),
DeleteCategory(..),
TagId,
ActionTags,
GetTags(..),
CreateTag(..),
DeleteTag(..),
EditTag(..),
AuthorId,
ActionAuthors,
GetAuthors(..),
CreateAuthor(..),
DeleteAuthor(..),
EditAuthor(..),
PostId,
ActionPosts1(..),
ActionPosts,
PublishEditPost(..),
GetPosts(..),
CreationDateOptions(..),
TagsOptions(..),
SearchOptions(..),
SortEntity(..),
SortOrder(..),
SortOptions(..),
DraftId,
ActionDrafts,
CreateDraft(..),
GetDrafts(..),
EditDraft(..),
EditDraftPublish(..),
DeleteDraft(..),
Publish(..),
UserId,
ActionUsers,
CreateUser(..),
GetProfile(..),
DeleteUser(..),
Authenticate(..),
CommentId,
ActionComments,
GetComments(..),
CreateComment(..),
DeleteComment(..),
User(..),
Draft(..),
Comment(..),
DraftRaw(..),
Author(..),
Tag(..),
Category(..),
Post(..),
WithAuthor(..),
WithUser(..)

) where

import GHC.Generics
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.FromField as PSF
import qualified Database.PostgreSQL.Simple.FromRow as PSR
import qualified Database.PostgreSQL.Simple.Types as PST
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified GenericPretty as GP
import qualified Data.Aeson as Ae
import General
import Types.Authors
import Types.Category
--import Types.Comments
import Types.Common
import Types.Draft
import Types.Posts
import Types.Tags
import Types.Users


{-
-}


data WithUser a = WithUser {
    _wu_userId :: User,
    _wu_action :: a
    } deriving (Show, Generic)

data WithAuthor a = WithAuthor {
    _wa_authorId :: Int,
    _wa_action :: a
    } deriving (Show, Generic)

type Token = T.Text

data WhoWhat a = WhoWhat {
    _ww_token  :: Maybe Token,
    _ww_action :: a
    } deriving (Show, Generic) 



data User = User {
    _u_id :: UserId,
    _u_firstname :: T.Text,
    _u_lastname :: T.Text,
    _u_pictureUrl :: Maybe T.Text,
    _u_login :: T.Text,
    _u_passHash :: T.Text,
    _u_creationDate :: Time.Day,
    _u_admin :: Maybe Bool
    } deriving (Show, Eq, Generic, GP.PrettyShow, PSR.FromRow)

instance Ae.ToJSON User where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }
 


data Author = Author {
    _a_authorId :: AuthorId,
    _a_description :: Maybe T.Text,
    _a_user :: User
    } deriving (Show, Eq, Generic, GP.PrettyShow)

instance Ae.ToJSON Author where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }
 
instance PSR.FromRow Author where
    fromRow = Author <$> PSR.field <*> PSR.field <*> PSR.fromRow
    {-fromRow = do
        (id, desc, uid, fn, ln, img, login, pass, creat) <-
            (,,,,,,,,) <$> PSR.field <*> PSR.field <*> PSR.field <*> PSR.field
                       <*> PSR.field <*> PSR.field <*> PSR.field <*> PSR.field <*> PSR.field
        return $ Author id desc $ User uid fn ln img login pass creat Nothing
-}
   

data Category = Category {
    _cat_categoryId :: CategoryId,
    _cat_description :: T.Text,
    _cat_parentCategory :: Maybe Category
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
    _p_author :: Author,
    _p_tags :: [Tag],
    _p_category :: Category,
    _p_content :: T.Text,
    _p_mainPhoto :: Maybe T.Text,
    _p_extraPhotos :: Maybe [T.Text]
    } deriving (Show, Eq, Generic)

instance GP.PrettyShow Post

instance Ae.ToJSON Post where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }


instance PSR.FromRow Post where
    --fromRow = Post <$> PSR.field <*> PSR.field <*> PSR.field <*> PSR.fromRow <*> PSR.fromRow <*> PSR.field <*> PSR.field <*> PSR.field
    fromRow = do
        id <- PSR.field
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
        return $ Post id title creationDate author tags category content photo extraPhotos

data Tag = Tag {
    _t_tagId :: TagId,
    _t_tagName :: T.Text
    } deriving (Show, Eq, Generic, GP.PrettyShow, PS.FromRow)


instance Ae.ToJSON Tag where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }



data Comment = Comment {
    _com_commentId :: CommentId,
    _com_content :: T.Text,
    _com_user :: User
    } deriving (Show, Eq, Generic, GP.PrettyShow)


instance Ae.ToJSON Comment where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }


instance PSR.FromRow Comment where
    fromRow = do
        id <- PSR.field
        content <- PSR.field
        user <- PSR.fromRow
        return $ Comment id content user


data Draft = Draft {
    _d_draftId :: DraftId,
    _d_title :: T.Text,
    _d_creationDate :: Time.Day,
    _d_author :: Author,
    _d_tags :: [Tag],
    _d_category :: Category,
    _d_content :: T.Text,
    _d_mainPhoto :: Maybe T.Text,
    _d_extraPhotos :: Maybe [T.Text],
    _d_postId :: Maybe PostId
    } deriving (Show, Eq, Generic, GP.PrettyShow)


instance Ae.ToJSON Draft where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }


instance PSR.FromRow Draft where
    fromRow = do
        id <- PSR.field
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
        return $ Draft id title creationDate author tags category content photo extraPhotos postId


data DraftRaw = DraftRaw {
    _dr_draftId :: DraftId,
    _dr_title :: T.Text,
    _dr_creationDate :: Time.Day,
    _dr_authorId :: Int,
    _dr_categoryId :: Int,
    _dr_tagIds :: [Int],
    _dr_content :: T.Text,
    _dr_mainPhoto :: Maybe T.Text,
    _dr_extraPhotos :: Maybe [T.Text],
    _dr_postId :: Maybe PostId
    } deriving (Show, Eq, Generic, GP.PrettyShow)


instance Ae.ToJSON DraftRaw where
    toJSON = Ae.genericToJSON Ae.defaultOptions { Ae.fieldLabelModifier = defaultModifier }



instance PSR.FromRow DraftRaw where
    fromRow = do
        id <- PSR.field
        title <- PSR.field
        creationDate <- PSR.field
        author <- PSR.field
        categoryId <- PSR.field
        tagIds <- PSR.field
        content <- PSR.field
        photo <- PSR.field
        extraPhotos <- PSR.field
        postId <- PSR.field
        return $ DraftRaw id title creationDate author categoryId tagIds content photo extraPhotos postId






