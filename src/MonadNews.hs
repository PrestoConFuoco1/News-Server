{-# LANGUAGE RecordWildCards #-}
module MonadNews where

import MonadTypes
import Execute.Database
import Types
import Database
import Execute.HasTags
import Control.Monad.Catch as CMC
import Exceptions as Ex
import Control.Monad (when)
import qualified Data.Aeson as Ae
import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO, liftIO)

import Result


getUser :: (MonadThrow m) => Maybe User -> m Response
getUser Nothing = Ex.unauthorized
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ ok "Success" val


authenticate :: (MonadNews m) => Authenticate -> m Response
authenticate auth = do
    user <- getUserByLogin $ _au_login auth
    when (_u_passHash user /= _au_passHash auth) $
        CMC.throwM Ex.InvalidPassword
    token <- fmap (T.pack) $ randomString1 10
    token' <- addToken (_u_id user) token
    return $ ok "Success" $ Ae.toJSON token'



class (Monad m, MonadLog m, CMC.MonadCatch m) => MonadNews m where

    withTransaction' :: m a -> m a

    getAuthors :: Paginated GetAuthors -> m [Author]
    createAuthor :: CreateAuthor -> m Int
    editAuthor :: EditAuthor -> m Int
    deleteAuthor :: DeleteAuthor -> m [Int]

    getTags :: Paginated GetTags -> m [Tag]
    createTag :: CreateTag -> m Int
    editTag :: EditTag -> m Int
    deleteTag :: DeleteTag -> m [Int]

    getCategories :: Paginated GetCategories -> m [Category]
    createCategory :: CreateCategory -> m Int
    editCategory :: EditCategory -> m Int
    deleteCategory :: DeleteCategory -> m [Int]

    createUser :: CreateUser -> m Int
    deleteUser :: DeleteUser -> m [Int]

    getComments :: Paginated GetComments -> m [Comment]
    createComment :: WithUser CreateComment -> m Int
    deleteComment :: WithUser DeleteComment -> m [Int]

    getUserByToken :: Token -> m (Maybe User)
    userAuthor :: User -> m Author

    getUserByLogin :: T.Text -> m User
    addToken :: UserId -> T.Text -> m T.Text
    randomString1 :: Int -> m String

    getPosts :: Paginated GetPosts -> m [Post]

    getDrafts :: Paginated (WithAuthor GetDrafts) -> m [Draft]
    deleteDraft :: WithAuthor DeleteDraft -> m [Int]
    
    createDraftN :: WithAuthor CreateDraft -> m Int
    attachTagsToDraftN :: Int -> [Int] -> m [Int]
    editDraftN :: WithAuthor EditDraft -> m Int
    removeAllButGivenTagsDraftN :: Int -> [Int] -> m [Int]

    getDraftsRaw :: WithAuthor Publish -> m [DraftRaw]

    createPost :: DraftRaw -> m Int
    editDraftPublish :: EditDraftPublish -> m Int
    attachTagsToPost :: Int -> [Int] -> m [Int]

    editPostPublish :: PublishEditPost -> m Int
    removeAllButGivenTagsPost :: Int -> [Int] -> m [Int]


    

{-
createDraft :: (MonadNews m) => WithAuthor CreateDraft -> m Response
createDraft x@(WithAuthor a CreateDraft{..}) = Ex.withHandler Ex.draftCreateHandler $
                                                withTransaction' $ do
    draft <- createDraftN x
  --  logDebug $ "Created draft with id = " <> (T.pack $ show draft)
    tags <- attachTagsToDraftN draft _cd_tags
--    tags <- attachTags dummyHDraft draft _cd_tags
    logInfo $ attached "draft" tags draft
    return $ okCreated ("Draft successfully created. " <> idInResult) draft
-}


instance MonadNews ServerIO where

    attachTagsToDraftN = attachTags dummyHDraft
    attachTagsToPost = attachTags dummyHPost
    editDraftN = editThis' draftEditDummy
    removeAllButGivenTagsDraftN = removeAllButGivenTags dummyHDraft
    removeAllButGivenTagsPost = removeAllButGivenTags dummyHPost
    getDraftsRaw = getThis' draftRawDummy

    withTransaction' = withTransaction
    createDraftN = createThis' draftCreateDummy
    editDraftPublish = editThis' draftEditPublishDummy
    editPostPublish = editThis' dummyUPost

    createPost = createThis' dummyCPost

    getComments = getThisPaginated' commentDummy
    createComment = createThis' dummyCComment
    deleteComment = deleteThis' dummyDComment

    getUserByToken token = do
        users <- getThis' userTokenDummy token
        user <- validateUnique2 (return Nothing) (Ex.throwTokenShared $ map _u_id users) $ map Just users
        return user

    getUserByLogin login = do
        let str = "SELECT user_id, firstname, lastname, \
                  \image, login, pass_hash, creation_date, is_admin \
                  \FROM news.users WHERE login = ?"
        users <- query str [login]
        validateUnique Ex.invalidLogin users

    addToken id token = do
        let str = "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
            token' = (T.pack $ show id) <> token
        execute str (id, token', token')
        return token'
    randomString1 len = liftIO $ randomString' len

    getDrafts = getThisPaginated' draftDummy
    deleteDraft = deleteThis' dummyDDraft
    

    userAuthor u = do
        as <- getThis' authorDummy (GetAuthors $ Just $ _u_id u)
        a  <- validateUnique Ex.notAnAuthor as
        return a



    getPosts = getThisPaginated' postDummy

    getAuthors = getThisPaginated' authorDummy
    createAuthor = createThis' dummyCAuthor
    editAuthor = editThis' dummyUAuthor
    deleteAuthor = deleteThis' dummyDAuthor

    getTags = getThisPaginated' tagDummy
    createTag = createThis' dummyCTag
    editTag = editThis' dummyUTag
    deleteTag = deleteThis' dummyDTag

    getCategories = getThisPaginated' catDummy
    createCategory = createThis' dummyCCat
    editCategory = editThis' dummyUCat
    deleteCategory = deleteThis' dummyDCat

    createUser = createThis' dummyCUser
    deleteUser = deleteThis' dummyDUser



withAuthAdmin y = withAuth y >>= checkAdmin
withAuthor y = 
    withAuth y >>= maybeUserToUser >>= userAuthor


getUsersByToken :: (MonadServer m) => Token -> m (Maybe User)
getUsersByToken token = do
    users <- getThis' userTokenDummy token
 --   users <- query str [token]
    user <- validateUnique2 (return Nothing) (Ex.throwTokenShared $ map _u_id users) $ map Just users
    return user


withAuth :: (MonadNews m) => Maybe Token -> m (Maybe User)
withAuth mtoken = case mtoken of
    Nothing -> return Nothing
    Just token -> do
        muser <- getUserByToken token
        return muser

checkAdmin :: (CMC.MonadThrow m) => Maybe User -> m ()
checkAdmin muser =
   when ((muser >>= _u_admin) /= Just True) $ Ex.throwForbidden



maybeUserToUser :: (CMC.MonadThrow m) => Maybe User -> m User
maybeUserToUser Nothing = Ex.unauthorized
maybeUserToUser (Just u) = return u


validateUnique :: (MonadThrow m) => m a -> [a] -> m a
validateUnique x [] = x
validateUnique _ [a] = return a
validateUnique _ us  = Ex.invalidUnique us


validateUnique2 :: (Monad m) => m a -> m a -> [a] -> m a
validateUnique2 empty toomuch [] = empty
validateUnique2 empty toomuch [a] = return a
validateUnique2 empty toomuch us = toomuch

