{-# LANGUAGE RecordWildCards #-}
module MonadNews where

import Types
import Control.Monad.Catch as CMC
import Exceptions as Ex
import Control.Monad (when)
import qualified Data.Text as T
import MonadLog


class (Monad m, MonadLog m, CMC.MonadCatch m) => MonadAuth m where

    createUser :: CreateUser -> m (Either ModifyError Int)
    deleteUser :: DeleteUser -> m (Either DeleteError Int)

    getUserByToken :: Token -> m (Maybe User)
    userAuthor :: User -> m (Maybe Author)

    getUserByLogin :: T.Text -> m (Maybe User)
    addToken :: UserId -> T.Text -> m T.Text
    randomString1 :: Int -> m String

class MonadAuth m => MonadEntities m where

    getAuthors :: Paginated GetAuthors -> m [Author]
    createAuthor :: CreateAuthor -> m (Either ModifyError Int)
    editAuthor :: EditAuthor -> m (Either ModifyError Int)
    deleteAuthor :: DeleteAuthor -> m (Either DeleteError Int)

    getTags :: Paginated GetTags -> m [Tag]
    createTag :: CreateTag -> m (Either ModifyError Int)
    editTag :: EditTag -> m (Either ModifyError Int)
    deleteTag :: DeleteTag -> m (Either DeleteError Int)

    getCategories :: Paginated GetCategories -> m [Category]
    createCategory :: CreateCategory -> m (Either ModifyError Int)
    editCategory :: EditCategory -> m (Either ModifyError Int)
    deleteCategory :: DeleteCategory -> m (Either DeleteError Int)

    getPosts :: Paginated GetPosts -> m [Post]
    getComments :: Paginated GetComments -> m [Comment]
    createComment :: WithUser CreateComment -> m (Either ModifyError Int)
    deleteComment :: WithUser DeleteComment -> m (Either DeleteError Int)




   

class (Monad m, MonadLog m, CMC.MonadCatch m, MonadEntities m) => MonadNews m where

    withTransaction' :: m a -> m a


    getDrafts :: Paginated (WithAuthor GetDrafts) -> m [Draft]
    deleteDraft :: WithAuthor DeleteDraft -> m (Either DeleteError DraftId)
    
    createDraftN :: WithAuthor CreateDraft -> m (Either ModifyError DraftId)
    attachTagsToDraft :: Int -> [TagId] -> m (Either TagsError [TagId])
    editDraftN :: WithAuthor EditDraft -> m (Either ModifyError DraftId)
    removeAllButGivenTagsDraft :: DraftId -> [TagId] -> m [TagId]

    getDraftRaw :: WithAuthor Publish -> m (Maybe DraftRaw)

    createPost :: DraftRaw -> m (Either ModifyError PostId)
    editDraftPublish :: EditDraftPublish -> m (Either ModifyError Int)
    attachTagsToPost :: Int -> [TagId] -> m (Either TagsError [TagId])

    editPostPublish :: PublishEditPost -> m (Either ModifyError Int)
    removeAllButGivenTagsPost :: PostId -> [TagId] -> m [TagId]


    



