{-# LANGUAGE RecordWildCards #-}
module MonadNewsInstances where

import Prelude hiding (Read)
import MonadTypes
import MonadNews
import Execute.Database
import Types
import Database
import Execute.HasTags
import Control.Monad.Catch as CMC
import Exceptions as Ex
import Control.Monad (when)
import qualified Data.Aeson as Ae
import qualified Data.Text as T
import Execute.Utils
import MonadLog
import Utils
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Control.Monad.IO.Class (MonadIO, liftIO)

import Result

instance MonadAuth ServerIO where

    userAuthor = userAuthor1

    createUser = createThis' dummyCUser
    deleteUser = deleteThis' dummyDUser

    getUserByToken = getUserByToken1

    getUserByLogin = getUserByLogin1

    addToken = addToken1
    randomString1 len = liftIO $ randomString' len

instance MonadEntities ServerIO where
 
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

    getPosts = getThisPaginated' postDummy

    getComments = getThisPaginated' commentDummy
    createComment = createThis' dummyCComment
    deleteComment = deleteThis' dummyDComment


   
    

instance MonadNews ServerIO where

    attachTagsToDraft = attachTags dummyHDraft
    attachTagsToPost = attachTags dummyHPost
    editDraftN = editThis' draftEditDummy
    removeAllButGivenTagsDraft = removeAllButGivenTags dummyHDraft
    removeAllButGivenTagsPost = removeAllButGivenTags dummyHPost
    getDraftsRaw = getThis' draftRawDummy

    withTransaction' = withTransaction
    createDraftN = createThis' draftCreateDummy
    editDraftPublish = editThis' draftEditPublishDummy
    editPostPublish = editThis' dummyUPost

    createPost = createThis' dummyCPost


    getDrafts = getThisPaginated' draftDummy
    deleteDraft = deleteThis' dummyDDraft
    



userAuthor1 :: User -> ServerIO Author
userAuthor1 u = do
    as <- getThis' authorDummy (GetAuthors $ Just $ _u_id u)
    a  <- validateUnique Ex.notAnAuthor as
    return a

getUserByToken1 :: Token -> ServerIO (Maybe User)
getUserByToken1 token = do
    users <- getThis' userTokenDummy token
    user <- validateUnique2 (return Nothing) (Ex.throwTokenShared $ map _u_id users) $ map Just users
    return user

getUserByLogin1 :: T.Text -> ServerIO User
getUserByLogin1 login = do
        let str = "SELECT user_id, firstname, lastname, \
                  \image, login, pass_hash, creation_date, is_admin \
                  \FROM news.users WHERE login = ?"
        users <- query str [login]
        validateUnique Ex.invalidLogin users


addToken1 :: UserId -> T.Text -> ServerIO T.Text
addToken1 id token = do
    let str = "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
        token' = (T.pack $ show id) <> token
    execute str (id, token', token')
    return token'



getThisPaginated' :: (Read s) => s -> Paginated (Get s) -> ServerIO [MType s]
getThisPaginated' x (Paginated page size g) = do
    let (qu, pars) = selectQuery x g
        (qupag, parspag) = pageingClause page size
        qu' = qu <> qupag
        totalpars = pars ++ parspag
    debugStr <- formatQuery qu' totalpars
    logDebug $ T.pack $ show debugStr
    res <- query qu' totalpars
    logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res

getThis' :: (Read s) => s -> Get s -> ServerIO [MType s]
getThis' x g = do
    let (qu, pars) = selectQuery x g
    debugStr <- formatQuery qu pars
    logDebug $ T.pack $ show debugStr
    res <- query qu pars
    logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res



editThis' :: (UpdateSQL s) => s -> Upd s -> ServerIO (Either ModifyError Int)
editThis' s u = case updateParams s u of
  Nothing -> Ex.invalidUpdDel "No data to edit found, required at least one parameter"
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query str params
    Ex.withHandler Ex.modifyHandler $ case ids of
        [] -> return (Left NotFound)
        [x] -> return (Right x)
        _   -> Ex.invalidUnique
{-
    id <- validateUnique (Ex.throwUpdNotFound $ uName s) ids
    logInfo $ "Updated " <> uName s <> " with id = " <> showText id
    return id
-}
createThis' :: (CreateSQL s) => s -> Create s -> ServerIO Int
createThis' w cres = do
    let (str, params) = createQuery w cres
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ints <- fmap (map PSTy.fromOnly) $ query str params
    int <- validateUnique (Ex.throwBadInsert (cName w)) ints
    logInfo $ "Created " <> cName w <> " with id = " <> showText int
    return int



deleteThis' :: (DeleteSQL s) => s -> Del s -> ServerIO [Int]
deleteThis' s del = do
    let (str, params) = deleteQuery s del
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    --withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
    ids <- fmap (map PSTy.fromOnly) $ query str params
    case ids of
        [] -> logInfo $ "No " <> dName s <> " deleted"
        _  -> logInfo $ "Deleted " <> dName s <> " with id = " <> showText ids
    return ids



