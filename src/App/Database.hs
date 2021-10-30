{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module App.Database where

import qualified App.Logger as Logger
import qualified Data.Text as T
import Prelude hiding (log)
import qualified Types as Y

data Handle m =
    Handle
    -- logger
        { log :: Logger.LoggerHandler m
    -- basic
        , createUser :: Logger.LoggerHandler m -> Y.CreateUser -> m (Either Y.ModifyError Int)
        , deleteUser :: Logger.LoggerHandler m -> Y.DeleteUser -> m (Either Y.DeleteError Int)
        , getUserByToken :: Logger.LoggerHandler m -> Y.Token -> m (Maybe Y.User)
        , userAuthor :: Logger.LoggerHandler m -> Y.User -> m (Maybe Y.Author)
        , getUserByLogin :: Logger.LoggerHandler m -> T.Text -> m (Maybe Y.User)
        , addToken :: Logger.LoggerHandler m -> Y.UserId -> T.Text -> m T.Text
        , generateToken :: Int -> m String
    -- entities,
        , getAuthors :: Logger.LoggerHandler m -> Y.Paginated Y.GetAuthors -> m [Y.Author]
        , createAuthor :: Logger.LoggerHandler m -> Y.CreateAuthor -> m (Either Y.ModifyError Int)
        , editAuthor :: Logger.LoggerHandler m -> Y.EditAuthor -> m (Either Y.ModifyError Int)
        , deleteAuthor :: Logger.LoggerHandler m -> Y.DeleteAuthor -> m (Either Y.DeleteError Int)
        , getTags :: Logger.LoggerHandler m -> Y.Paginated Y.GetTags -> m [Y.Tag]
        , createTag :: Logger.LoggerHandler m -> Y.CreateTag -> m (Either Y.ModifyError Int)
        , editTag :: Logger.LoggerHandler m -> Y.EditTag -> m (Either Y.ModifyError Int)
        , deleteTag :: Logger.LoggerHandler m -> Y.DeleteTag -> m (Either Y.DeleteError Int)
        , getCategories :: Logger.LoggerHandler m -> Y.Paginated Y.GetCategories -> m [Y.Category]
        , createCategory :: Logger.LoggerHandler m -> Y.CreateCategory -> m (Either Y.ModifyError Int)
        , editCategory :: Logger.LoggerHandler m -> Y.EditCategory -> m (Either Y.ModifyError Int)
        , deleteCategory :: Logger.LoggerHandler m -> Y.DeleteCategory -> m (Either Y.DeleteError Int)
        , getPosts :: Logger.LoggerHandler m -> Y.Paginated Y.GetPosts -> m [Y.Post]
        , getComments :: Logger.LoggerHandler m -> Y.Paginated Y.GetComments -> m [Y.Comment]
        , createComment :: Logger.LoggerHandler m -> Y.WithUser Y.CreateComment -> m (Either Y.ModifyError Int)
        , deleteComment :: Logger.LoggerHandler m -> Y.WithUser Y.DeleteComment -> m (Either Y.DeleteError Int)
    -- drafts, posts, publish,
        , withTransaction :: forall a. m a -> m a
        , getDrafts :: Logger.LoggerHandler m -> Y.Paginated (Y.WithAuthor Y.GetDrafts) -> m [Y.Draft]
        , deleteDraft :: Logger.LoggerHandler m -> Y.WithAuthor Y.DeleteDraft -> m (Either Y.DeleteError Y.DraftId)
        , createDraft :: Logger.LoggerHandler m -> Y.WithAuthor Y.CreateDraft -> m (Either Y.ModifyError Y.DraftId)
        , attachTagsToDraft :: Logger.LoggerHandler m -> Int -> [Y.TagId] -> m (Either Y.TagsError [Y.TagId])
        , editDraft :: Logger.LoggerHandler m -> Y.WithAuthor Y.EditDraft -> m (Either Y.ModifyError Y.DraftId)
        , removeAllButGivenTagsDraft :: Logger.LoggerHandler m -> Y.DraftId -> [Y.TagId] -> m [Y.TagId]
        , getDraftRaw :: Logger.LoggerHandler m -> Y.WithAuthor Y.Publish -> m (Maybe Y.DraftRaw)
        , createPost :: Logger.LoggerHandler m -> Y.DraftRaw -> m (Either Y.ModifyError Y.PostId)
        , editDraftPublish :: Logger.LoggerHandler m -> Y.EditDraftPublish -> m (Either Y.ModifyError Int)
        , attachTagsToPost :: Logger.LoggerHandler m -> Int -> [Y.TagId] -> m (Either Y.TagsError [Y.TagId])
        , editPostPublish :: Logger.LoggerHandler m -> Y.PublishEditPost -> m (Either Y.ModifyError Int)
        , removeAllButGivenTagsPost :: Logger.LoggerHandler m -> Y.PostId -> [Y.TagId] -> m [Y.TagId]
        }

logDebug, logInfo, logWarning, logError, logFatal ::
       Handle m -> T.Text -> m ()
logDebug h = Logger.logDebug (log h)

logInfo h = Logger.logInfo (log h)

logWarning h = Logger.logWarning (log h)

logError h = Logger.logError (log h)

logFatal h = Logger.logFatal (log h)
