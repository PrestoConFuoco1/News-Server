{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module App.Database where

import qualified App.Logger as Logger
import qualified Data.Text as T
import Prelude hiding (log)
import qualified Types as Y

data Handle m =
   Handle
    -- logger
      { log :: Logger.Handle m
    -- basic
      , createUser :: Logger.Handle m -> Y.CreateUser -> m (Either Y.ModifyError Int)
      , deleteUser :: Logger.Handle m -> Y.DeleteUser -> m (Either Y.DeleteError Int)
      , getUserByToken :: Logger.Handle m -> Y.Token -> m (Maybe Y.User)
      , userAuthor :: Logger.Handle m -> Y.User -> m (Maybe Y.Author)
      , getUserByLogin :: Logger.Handle m -> T.Text -> m (Maybe Y.User)
      , addToken :: Logger.Handle m -> Y.UserId -> T.Text -> m T.Text
      , generateToken :: Int -> m String
    -- entities,
      , getAuthors :: Logger.Handle m -> Y.Paginated Y.GetAuthors -> m [Y.Author]
      , createAuthor :: Logger.Handle m -> Y.CreateAuthor -> m (Either Y.ModifyError Int)
      , editAuthor :: Logger.Handle m -> Y.EditAuthor -> m (Either Y.ModifyError Int)
      , deleteAuthor :: Logger.Handle m -> Y.DeleteAuthor -> m (Either Y.DeleteError Int)
      , getTags :: Logger.Handle m -> Y.Paginated Y.GetTags -> m [Y.Tag]
      , createTag :: Logger.Handle m -> Y.CreateTag -> m (Either Y.ModifyError Int)
      , editTag :: Logger.Handle m -> Y.EditTag -> m (Either Y.ModifyError Int)
      , deleteTag :: Logger.Handle m -> Y.DeleteTag -> m (Either Y.DeleteError Int)
      , getCategories :: Logger.Handle m -> Y.Paginated Y.GetCategories -> m [Y.Category]
      , createCategory :: Logger.Handle m -> Y.CreateCategory -> m (Either Y.ModifyError Int)
      , editCategory :: Logger.Handle m -> Y.EditCategory -> m (Either Y.ModifyError Int)
      , deleteCategory :: Logger.Handle m -> Y.DeleteCategory -> m (Either Y.DeleteError Int)
      , getPosts :: Logger.Handle m -> Y.Paginated Y.GetPosts -> m [Y.Post]
      , getComments :: Logger.Handle m -> Y.Paginated Y.GetComments -> m [Y.Comment]
      , createComment :: Logger.Handle m -> Y.WithUser Y.CreateComment -> m (Either Y.ModifyError Int)
      , deleteComment :: Logger.Handle m -> Y.WithUser Y.DeleteComment -> m (Either Y.DeleteError Int)
    -- drafts, posts, publish,
      , withTransaction :: forall a. m a -> m a
      , getDrafts :: Logger.Handle m -> Y.Paginated (Y.WithAuthor Y.GetDrafts) -> m [Y.Draft]
      , deleteDraft :: Logger.Handle m -> Y.WithAuthor Y.DeleteDraft -> m (Either Y.DeleteError Y.DraftId)
      , createDraft :: Logger.Handle m -> Y.WithAuthor Y.CreateDraft -> m (Either Y.ModifyError Y.DraftId)
      , attachTagsToDraft :: Logger.Handle m -> Int -> [Y.TagId] -> m (Either Y.TagsError [Y.TagId])
      , editDraft :: Logger.Handle m -> Y.WithAuthor Y.EditDraft -> m (Either Y.ModifyError Y.DraftId)
      , removeAllButGivenTagsDraft :: Logger.Handle m -> Y.DraftId -> [Y.TagId] -> m [Y.TagId]
      , getDraftRaw :: Logger.Handle m -> Y.WithAuthor Y.Publish -> m (Maybe Y.DraftRaw)
      , createPost :: Logger.Handle m -> Y.DraftRaw -> m (Either Y.ModifyError Y.PostId)
      , editDraftPublish :: Logger.Handle m -> Y.EditDraftPublish -> m (Either Y.ModifyError Int)
      , attachTagsToPost :: Logger.Handle m -> Int -> [Y.TagId] -> m (Either Y.TagsError [Y.TagId])
      , editPostPublish :: Logger.Handle m -> Y.PublishEditPost -> m (Either Y.ModifyError Int)
      , removeAllButGivenTagsPost :: Logger.Handle m -> Y.PostId -> [Y.TagId] -> m [Y.TagId]
      }

logDebug, logInfo, logWarning, logError, logFatal ::
      Handle m -> T.Text -> m ()
logDebug h = Logger.logDebug (log h)

logInfo h = Logger.logInfo (log h)

logWarning h = Logger.logWarning (log h)

logError h = Logger.logError (log h)

logFatal h = Logger.logFatal (log h)
