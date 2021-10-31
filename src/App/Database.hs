{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module App.Database
    ( Handle(..)
    , logDebug
    , logInfo
    , logWarning
    , logError
    , logFatal
    ) where

import qualified App.Logger as L
import qualified Data.Text as T
import Prelude hiding (log)
import qualified Types as Y

data Handle m =
    Handle
    -- logger
        { log :: L.LoggerHandler m
    -- basic
        , createUser :: L.LoggerHandler m -> Y.CreateUser -> m (Either Y.ModifyError Int)
        , deleteUser :: L.LoggerHandler m -> Y.DeleteUser -> m (Either Y.DeleteError Int)
        , getUserByToken :: L.LoggerHandler m -> Y.Token -> m (Maybe Y.User)
        , userAuthor :: L.LoggerHandler m -> Y.User -> m (Maybe Y.Author)
        , getUserByLogin :: L.LoggerHandler m -> T.Text -> m (Maybe Y.User)
        , addToken :: L.LoggerHandler m -> Y.UserId -> T.Text -> m T.Text
        , generateToken :: Int -> m String
    -- entities,
        , getAuthors :: L.LoggerHandler m -> Y.Paginated Y.GetAuthors -> m [Y.Author]
        , createAuthor :: L.LoggerHandler m -> Y.CreateAuthor -> m (Either Y.ModifyError Int)
        , editAuthor :: L.LoggerHandler m -> Y.EditAuthor -> m (Either Y.ModifyError Int)
        , deleteAuthor :: L.LoggerHandler m -> Y.DeleteAuthor -> m (Either Y.DeleteError Int)

        , getTags :: L.LoggerHandler m -> Y.Paginated Y.GetTags -> m [Y.Tag]
        , createTag :: L.LoggerHandler m -> Y.CreateTag -> m (Either Y.ModifyError Int)
        , editTag :: L.LoggerHandler m -> Y.EditTag -> m (Either Y.ModifyError Int)
        , deleteTag :: L.LoggerHandler m -> Y.DeleteTag -> m (Either Y.DeleteError Int)

        , getCategories :: L.LoggerHandler m -> Y.Paginated Y.GetCategories -> m [Y.Category]
        , getCategoryById :: L.LoggerHandler m -> Y.CategoryId -> m (Maybe Y.Category)
        , createCategory :: L.LoggerHandler m -> Y.CreateCategory -> m (Either Y.ModifyError Int)
        , editCategory :: L.LoggerHandler m -> Y.EditCategory -> m (Either Y.ModifyError Int)
        , deleteCategory :: L.LoggerHandler m -> Y.DeleteCategory -> m (Either Y.DeleteError Int)

        , getPosts :: L.LoggerHandler m -> Y.Paginated Y.GetPosts -> m [Y.Post]

        , getComments :: L.LoggerHandler m -> Y.Paginated Y.GetComments -> m [Y.Comment]
        , createComment :: L.LoggerHandler m -> Y.WithUser Y.CreateComment -> m (Either Y.ModifyError Int)
        , deleteComment :: L.LoggerHandler m -> Y.WithUser Y.DeleteComment -> m (Either Y.DeleteError Int)

    -- drafts, posts, publish,
        , withTransaction :: forall a. m a -> m a
        , getDrafts :: L.LoggerHandler m -> Y.Paginated (Y.WithAuthor Y.GetDrafts) -> m [Y.Draft]
        , deleteDraft :: L.LoggerHandler m -> Y.WithAuthor Y.DeleteDraft -> m (Either Y.DeleteError Y.DraftId)
        , createDraft :: L.LoggerHandler m -> Y.WithAuthor Y.CreateDraft -> m (Either Y.ModifyError Y.DraftId)
        , attachTagsToDraft :: L.LoggerHandler m -> Int -> [Y.TagId] -> m (Either Y.TagsError [Y.TagId])
        , editDraft :: L.LoggerHandler m -> Y.WithAuthor Y.EditDraft -> m (Either Y.ModifyError Y.DraftId)
        , removeAllButGivenTagsDraft :: L.LoggerHandler m -> Y.DraftId -> [Y.TagId] -> m [Y.TagId]
        , getDraftRaw :: L.LoggerHandler m -> Y.WithAuthor Y.Publish -> m (Maybe Y.DraftRaw)
        , createPost :: L.LoggerHandler m -> Y.DraftRaw -> m (Either Y.ModifyError Y.PostId)
        , editDraftPublish :: L.LoggerHandler m -> Y.EditDraftPublish -> m (Either Y.ModifyError Int)
        , attachTagsToPost :: L.LoggerHandler m -> Int -> [Y.TagId] -> m (Either Y.TagsError [Y.TagId])
        , editPostPublish :: L.LoggerHandler m -> Y.PublishEditPost -> m (Either Y.ModifyError Int)
        , removeAllButGivenTagsPost :: L.LoggerHandler m -> Y.PostId -> [Y.TagId] -> m [Y.TagId]
        }

logDebug, logInfo, logWarning, logError, logFatal ::
       Handle m -> T.Text -> m ()
logDebug h = L.logDebug (log h)

logInfo h = L.logInfo (log h)

logWarning h = L.logWarning (log h)

logError h = L.logError (log h)

logFatal h = L.logFatal (log h)
