{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module App.Database
    ( Handle(..)
    , AuthHandler(..)
    , AuthorsHandler(..)
    , TagsHandler(..)
    , CatsHandler(..)
    , CommentsHandler(..)
    , DraftsHandler(..)
    , logDebug
    , logInfo
    , logWarning
    , logError
    , logFatal
    ) where

import qualified App.Logger as L
import qualified Data.Text as Text
import Prelude hiding (log)
import qualified Types as T

data AuthHandler m =
    AuthHandler
        { createUser :: L.LoggerHandler m -> T.CreateUser -> m (Either T.ModifyError Int)
        , deleteUser :: L.LoggerHandler m -> T.DeleteUser -> m (Either T.DeleteError Int)
        , getUserByToken :: L.LoggerHandler m -> T.Token -> m (Maybe T.User)
        , userAuthor :: L.LoggerHandler m -> T.User -> m (Maybe T.Author)
        , getUserByLogin :: L.LoggerHandler m -> Text.Text -> m (Maybe T.User)
        , addToken :: L.LoggerHandler m -> T.UserId -> Text.Text -> m Text.Text
        , generateToken :: Int -> m String
        }

data AuthorsHandler m =
    AuthorsHandler
        { getAuthors :: L.LoggerHandler m -> T.Paginated T.GetAuthors -> m [T.Author]
        , createAuthor :: L.LoggerHandler m -> T.CreateAuthor -> m (Either T.ModifyError Int)
        , editAuthor :: L.LoggerHandler m -> T.EditAuthor -> m (Either T.ModifyError Int)
        , deleteAuthor :: L.LoggerHandler m -> T.DeleteAuthor -> m (Either T.DeleteError Int)
        }

data TagsHandler m =
    TagsHandler
        { getTags :: L.LoggerHandler m -> T.Paginated T.GetTags -> m [T.Tag]
        , createTag :: L.LoggerHandler m -> T.CreateTag -> m (Either T.ModifyError Int)
        , editTag :: L.LoggerHandler m -> T.EditTag -> m (Either T.ModifyError Int)
        , deleteTag :: L.LoggerHandler m -> T.DeleteTag -> m (Either T.DeleteError Int)
        }

data CatsHandler m =
    CatsHandler
        { getCategories :: L.LoggerHandler m -> T.Paginated T.GetCategories -> m [T.Category]
        , getCategoryById :: L.LoggerHandler m -> T.CategoryId -> m (Maybe T.Category)
        , createCategory :: L.LoggerHandler m -> T.CreateCategory -> m (Either T.ModifyError Int)
        , editCategory :: L.LoggerHandler m -> T.EditCategory -> m (Either T.ModifyError Int)
        , deleteCategory :: L.LoggerHandler m -> T.DeleteCategory -> m (Either T.DeleteError Int)
        }

data CommentsHandler m =
    CommentsHandler
        { getComments :: L.LoggerHandler m -> T.Paginated T.GetComments -> m [T.Comment]
        , createComment :: L.LoggerHandler m -> T.WithUser T.CreateComment -> m (Either T.ModifyError Int)
        , deleteComment :: L.LoggerHandler m -> T.WithUser T.DeleteComment -> m (Either T.DeleteError Int)
        }

data DraftsHandler m =
    DraftsHandler
        { withTransaction :: forall a. m a -> m a
        , getDrafts :: L.LoggerHandler m -> T.Paginated (T.WithAuthor T.GetDrafts) -> m [T.Draft]
        , deleteDraft :: L.LoggerHandler m -> T.WithAuthor T.DeleteDraft -> m (Either T.DeleteError T.DraftId)
        , createDraft :: L.LoggerHandler m -> T.WithAuthor T.CreateDraft -> m (Either T.ModifyError T.DraftId)
        , attachTagsToDraft :: L.LoggerHandler m -> Int -> [T.TagId] -> m (Either T.TagsError [T.TagId])
        , editDraft :: L.LoggerHandler m -> T.WithAuthor T.EditDraft -> m (Either T.ModifyError T.DraftId)
        , removeAllButGivenTagsDraft :: L.LoggerHandler m -> T.DraftId -> [T.TagId] -> m [T.TagId]
        , getDraftRaw :: L.LoggerHandler m -> T.WithAuthor T.Publish -> m (Maybe T.DraftRaw)
        , createPost :: L.LoggerHandler m -> T.DraftRaw -> m (Either T.ModifyError T.PostId)
        , editDraftPublish :: L.LoggerHandler m -> T.EditDraftPublish -> m (Either T.ModifyError Int)
        , attachTagsToPost :: L.LoggerHandler m -> Int -> [T.TagId] -> m (Either T.TagsError [T.TagId])
        , editPostPublish :: L.LoggerHandler m -> T.PublishEditPost -> m (Either T.ModifyError Int)
        , removeAllButGivenTagsPost :: L.LoggerHandler m -> T.PostId -> [T.TagId] -> m [T.TagId]
        }

data Handle m =
    Handle
    -- logger
        { log :: L.LoggerHandler m
    -- basic
        , authHandler :: AuthHandler m
    -- entities,
        , authorsHandler :: AuthorsHandler m
        , tagsHandler :: TagsHandler m
        , catsHandler :: CatsHandler m
        , getPosts :: L.LoggerHandler m -> T.Paginated T.GetPosts -> m [T.Post]
        , commentsHandler :: CommentsHandler m
    -- drafts, posts, publish,
        , draftsHandler :: DraftsHandler m
        }

logDebug, logInfo, logWarning, logError, logFatal ::
       Handle m -> Text.Text -> m ()
logDebug h = L.logDebug (log h)

logInfo h = L.logInfo (log h)

logWarning h = L.logWarning (log h)

logError h = L.logError (log h)

logFatal h = L.logFatal (log h)
