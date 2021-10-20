{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module App.Database where

import Types
import qualified Data.Text as T
import Prelude hiding (log)
import qualified App.Logger as Logger


data Handle m = Handle
    {

    -- logger
    log :: Logger.Handle m,

    -- basic

    createUser :: Logger.Handle m -> CreateUser -> m (Either ModifyError Int),
    deleteUser :: Logger.Handle m -> DeleteUser -> m (Either DeleteError Int),

    getUserByToken :: Logger.Handle m -> Token -> m (Maybe User),
    userAuthor :: Logger.Handle m -> User -> m (Maybe Author),

    getUserByLogin :: Logger.Handle m -> T.Text -> m (Maybe User),
    addToken :: Logger.Handle m -> UserId -> T.Text -> m T.Text,
    generateToken :: Int -> m String,

    -- entities,

    getAuthors :: Logger.Handle m -> Paginated GetAuthors -> m [Author],
    createAuthor :: Logger.Handle m -> CreateAuthor -> m (Either ModifyError Int),
    editAuthor :: Logger.Handle m -> EditAuthor -> m (Either ModifyError Int),
    deleteAuthor :: Logger.Handle m -> DeleteAuthor -> m (Either DeleteError Int),

    getTags :: Logger.Handle m -> Paginated GetTags -> m [Tag],
    createTag :: Logger.Handle m -> CreateTag -> m (Either ModifyError Int),
    editTag :: Logger.Handle m -> EditTag -> m (Either ModifyError Int),
    deleteTag :: Logger.Handle m -> DeleteTag -> m (Either DeleteError Int),

    getCategories :: Logger.Handle m -> Paginated GetCategories -> m [Category],
    createCategory :: Logger.Handle m -> CreateCategory -> m (Either ModifyError Int),
    editCategory :: Logger.Handle m -> EditCategory -> m (Either ModifyError Int),
    deleteCategory :: Logger.Handle m -> DeleteCategory -> m (Either DeleteError Int),

    getPosts :: Logger.Handle m -> Paginated GetPosts -> m [Post],
    getComments :: Logger.Handle m -> Paginated GetComments -> m [Comment],
    createComment :: Logger.Handle m -> WithUser CreateComment -> m (Either ModifyError Int),
    deleteComment :: Logger.Handle m -> WithUser DeleteComment -> m (Either DeleteError Int),





    -- drafts, posts, publish,

    withTransaction :: (forall a. m a -> m a),


    getDrafts :: Logger.Handle m -> Paginated (WithAuthor GetDrafts) -> m [Draft],
    deleteDraft :: Logger.Handle m -> WithAuthor DeleteDraft -> m (Either DeleteError DraftId),

    createDraft :: Logger.Handle m -> WithAuthor CreateDraft -> m (Either ModifyError DraftId),
    attachTagsToDraft :: Logger.Handle m -> Int -> [TagId] -> m (Either TagsError [TagId]),
    editDraft :: Logger.Handle m -> WithAuthor EditDraft -> m (Either ModifyError DraftId),
    removeAllButGivenTagsDraft :: Logger.Handle m -> DraftId -> [TagId] -> m [TagId],

    getDraftRaw :: Logger.Handle m -> WithAuthor Publish -> m (Maybe DraftRaw),

    createPost :: Logger.Handle m -> DraftRaw -> m (Either ModifyError PostId),
    editDraftPublish :: Logger.Handle m -> EditDraftPublish -> m (Either ModifyError Int),
    attachTagsToPost :: Logger.Handle m -> Int -> [TagId] -> m (Either TagsError [TagId]),

    editPostPublish :: Logger.Handle m -> PublishEditPost -> m (Either ModifyError Int),
    removeAllButGivenTagsPost :: Logger.Handle m -> PostId -> [TagId] -> m [TagId]

    }


logDebug, logInfo, logWarning, logError, logFatal :: Handle m -> T.Text -> m ()

logDebug h = Logger.logDebug (log h)
logInfo h = Logger.logInfo (log h)
logWarning h = Logger.logWarning (log h)
logError h = Logger.logError (log h)
logFatal h = Logger.logFatal (log h)
