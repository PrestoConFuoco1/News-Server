{-# LANGUAGE RecordWildCards #-}
module MonadNews where

import Types
import Control.Monad.Catch as CMC
import Exceptions as Ex
import Control.Monad (when)
import qualified Data.Text as T
import MonadLog


class (Monad m, MonadLog m, CMC.MonadCatch m) => MonadAuth m where

    createUser :: CreateUser -> m Int
    deleteUser :: DeleteUser -> m [Int]

    getUserByToken :: Token -> m (Maybe User)
    userAuthor :: User -> m Author

    getUserByLogin :: T.Text -> m User
    addToken :: UserId -> T.Text -> m T.Text
    randomString1 :: Int -> m String

class MonadAuth m => MonadEntities m where

    getAuthors :: Paginated GetAuthors -> m [Author]
    createAuthor :: CreateAuthor -> m (Either ModifyError Int)
    editAuthor :: EditAuthor -> m (Either ModifyError Int)
    deleteAuthor :: DeleteAuthor -> m [Int]

    getTags :: Paginated GetTags -> m [Tag]
    createTag :: CreateTag -> m Int
    editTag :: EditTag -> m Int
    deleteTag :: DeleteTag -> m [Int]

    getCategories :: Paginated GetCategories -> m [Category]
    createCategory :: CreateCategory -> m Int
    editCategory :: EditCategory -> m Int
    deleteCategory :: DeleteCategory -> m [Int]

    getPosts :: Paginated GetPosts -> m [Post]
    getComments :: Paginated GetComments -> m [Comment]
    createComment :: WithUser CreateComment -> m Int
    deleteComment :: WithUser DeleteComment -> m [Int]




   

class (Monad m, MonadLog m, CMC.MonadCatch m, MonadEntities m) => MonadNews m where

    withTransaction' :: m a -> m a


    getDrafts :: Paginated (WithAuthor GetDrafts) -> m [Draft]
    deleteDraft :: WithAuthor DeleteDraft -> m [DraftId]
    
    createDraftN :: WithAuthor CreateDraft -> m DraftId
    attachTagsToDraft :: Int -> [TagId] -> m [TagId]
    editDraftN :: WithAuthor EditDraft -> m DraftId
    removeAllButGivenTagsDraft :: DraftId -> [TagId] -> m [TagId]

    getDraftsRaw :: WithAuthor Publish -> m [DraftRaw]

    createPost :: DraftRaw -> m PostId
    editDraftPublish :: EditDraftPublish -> m Int
    attachTagsToPost :: Int -> [TagId] -> m [TagId]

    editPostPublish :: PublishEditPost -> m Int
    removeAllButGivenTagsPost :: PostId -> [TagId] -> m [TagId]


    



