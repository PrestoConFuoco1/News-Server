{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module App.Database where

import Types
import qualified Data.Text as T
import Prelude hiding (log)
import qualified App.Logger as Logger


data Handle m = {- forall a. -} Handle
    {

    -- logger
    log :: Logger.Handle m,

    -- basic

    createUser :: CreateUser -> m (Either ModifyError Int),
    deleteUser :: DeleteUser -> m (Either DeleteError Int),

    getUserByToken :: Token -> m (Maybe User),
    userAuthor :: User -> m (Maybe Author),

    getUserByLogin :: T.Text -> m (Maybe User),
    addToken :: UserId -> T.Text -> m T.Text,
    --randomString :: Int -> m String,
    generateToken :: Int -> m String,

    -- entities,

    getAuthors :: Paginated GetAuthors -> m [Author],
    createAuthor :: CreateAuthor -> m (Either ModifyError Int),
    editAuthor :: EditAuthor -> m (Either ModifyError Int),
    deleteAuthor :: DeleteAuthor -> m (Either DeleteError Int),

    getTags :: Paginated GetTags -> m [Tag],
    createTag :: CreateTag -> m (Either ModifyError Int),
    editTag :: EditTag -> m (Either ModifyError Int),
    deleteTag :: DeleteTag -> m (Either DeleteError Int),

    getCategories :: Paginated GetCategories -> m [Category],
    createCategory :: CreateCategory -> m (Either ModifyError Int),
    editCategory :: EditCategory -> m (Either ModifyError Int),
    deleteCategory :: DeleteCategory -> m (Either DeleteError Int),

    getPosts :: Paginated GetPosts -> m [Post],
    getComments :: Paginated GetComments -> m [Comment],
    createComment :: WithUser CreateComment -> m (Either ModifyError Int),
    deleteComment :: WithUser DeleteComment -> m (Either DeleteError Int),





    -- drafts, posts, publish,

    withTransaction :: (forall a. m a -> m a),


    getDrafts :: Paginated (WithAuthor GetDrafts) -> m [Draft],
    deleteDraft :: WithAuthor DeleteDraft -> m (Either DeleteError DraftId),

    createDraft :: WithAuthor CreateDraft -> m (Either ModifyError DraftId),
    attachTagsToDraft :: Int -> [TagId] -> m (Either TagsError [TagId]),
    editDraft :: WithAuthor EditDraft -> m (Either ModifyError DraftId),
    removeAllButGivenTagsDraft :: DraftId -> [TagId] -> m [TagId],

    getDraftRaw :: WithAuthor Publish -> m (Maybe DraftRaw),

    createPost :: DraftRaw -> m (Either ModifyError PostId),
    editDraftPublish :: EditDraftPublish -> m (Either ModifyError Int),
    attachTagsToPost :: Int -> [TagId] -> m (Either TagsError [TagId]),

    editPostPublish :: PublishEditPost -> m (Either ModifyError Int),
    removeAllButGivenTagsPost :: PostId -> [TagId] -> m [TagId]

    }


logDebug, logInfo, logWarning, logError :: Handle m -> T.Text -> m ()

logDebug h = Logger.logDebug (log h)
logInfo h = Logger.logInfo (log h)
logWarning h = Logger.logWarning (log h)
logError h = Logger.logError (log h)
logFatal h = Logger.logFatal (log h)
