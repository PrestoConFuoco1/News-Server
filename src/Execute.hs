{-# LANGUAGE RecordWildCards #-}

module Execute where



import qualified Network.HTTP.Types as NHT
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception
import Control.Monad (when)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified GenericPretty as GP
import GHC.Generics

import Action.RequestToAction
import Action.Types (WhoWhat (..), Token)
import Action.Common
import FromSQL
import Create
import Delete
import Update

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Types as Ty
import qualified Data.Aeson as Ae
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import ActWithOne (actWithOne, ActWithOne(..), AWOu(..), AWOd(..))
import ExecuteTypes
import ExecuteUtils
import Action.Users.Types
import Action.Comments.Types
import Action.Draft.Types
import Action.Authors.Types

import Exceptions as Ex

import SqlValue

executeAction :: MonadServer m => WhoWhat Action -> m Response
executeAction (WhoWhat y (AAuthors x)) = executeAuthor (WhoWhat y x)
executeAction (WhoWhat y (ACategory x)) = executeCategory (WhoWhat y x)
executeAction (WhoWhat y (APosts x)) = executePosts (WhoWhat y x)
executeAction (WhoWhat y (ATags x)) = executeTags (WhoWhat y x)
executeAction (WhoWhat y (AUsers x)) = executeUsers (WhoWhat y x)
executeAction (WhoWhat y (AAuth x)) = authenticate x
executeAction (WhoWhat y (AComments x)) = executeComments (WhoWhat y x)
executeAction (WhoWhat y (ADrafts x)) = executeDraft (WhoWhat y x)

executePosts (WhoWhat y (Read x)) = getThis postDummy x


executeAuthor (WhoWhat y (Read x)) =
    withAuthAdmin y >> getThis authorDummy x
executeAuthor (WhoWhat y (Create x)) =
    withAuthAdmin y >> createThis dummyCAuthor x
executeAuthor (WhoWhat y (Update x)) =
    withAuthAdmin y >> editThis dummyUAuthor x
executeAuthor (WhoWhat y (Delete x)) =
    withAuthAdmin y >> deleteThis dummyDAuthor x

executeTags (WhoWhat y (Read x)) = getThis tagDummy x
executeTags (WhoWhat y (Create x)) = withAuthAdmin y >> createThis dummyCTag x
executeTags (WhoWhat y (Update x)) =  withAuthAdmin y >> editThis dummyUTag x
executeTags (WhoWhat y (Delete x)) = withAuthAdmin y >> deleteThis dummyDTag x


executeCategory (WhoWhat y (Read x)) = getThis catDummy x
executeCategory (WhoWhat y (Create x)) = withAuthAdmin y >> createThis dummyCCat x
executeCategory (WhoWhat y (Update x)) =  withAuthAdmin y >> editThis dummyUCat x
executeCategory (WhoWhat y (Delete x)) = withAuthAdmin y >> deleteThis dummyDCat x

executeUsers (WhoWhat y (Create x)) = createThis dummyCUser x
executeUsers (WhoWhat y (Delete x)) = withAuthAdmin y >> deleteThis dummyDUser x
executeUsers (WhoWhat y (Read GetProfile)) = withAuth y >>= getUser

executeComments (WhoWhat y (Read x)) = getThis commentDummy x
executeComments (WhoWhat y (Create x)) = withAuth y >>= createComment x
executeComments (WhoWhat y (Delete x)) = withAuth y >>= withUser >>= \u -> deleteThis dummyDComment $ WithUser (Ty._u_id u) x

createComment :: (MonadServer m) => CreateComment -> Maybe Ty.User -> m Response
createComment cc Nothing = Ex.unauthorized
createComment cc (Just u) = createThis dummyCComment $ WithUser (Ty._u_id u) cc

executeDraft :: (MonadServer m) => WhoWhat ActionDrafts -> m Response
executeDraft (WhoWhat y (Create x)) =
    withAuth y >>= withUser >>= withAuthor >>= \a -> createDraft $ WithAuthor (Ty._a_authorId a) x
executeDraft (WhoWhat y _ ) = undefined

createDraft :: (MonadServer m) => WithAuthor CreateDraft -> m Response -- ?
createDraft = undefined
{-
withCreateDraft :: (MonadServer m) => WithAuthor CreateDraft -> m Response
withCreateDraft (WithAuthor a CreateDraft{..}) = do
    let str =
         "INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos)\
         \VALUES(?, ?, ?, ?, ?, ?) "
        args = [SqlValue _cd_title, SqlValue a,
                SqlValue _cd_categoryId, SqlValue _cd_content,
                SqlValue _cd_mainPhoto, SqlValue $ fmap PSTy.PGArray _cd_extraPhotos]
    
    debugStr <- formatQuery str args
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers [CMC.Handler sqlH] $ do
        execute str args
        return (ok $ "Draft successfully created")
      where sqlH :: (MonadServer m) => PS.SqlError -> m Response
            sqlH e
             | otherwise = logError (T.pack $ displayException e)
                >> return (internal "Failed")
-}


getUser :: (MonadServer m) => Maybe Ty.User -> m Response
getUser Nothing = Ex.unauthorized
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ Response NHT.ok200 val

withAuthor :: (MonadServer m) => Ty.User -> m Ty.Author
withAuthor u = do
    as <- getThis' authorDummy (GetAuthors $ Just $ Ty._u_id u)
    a  <- validateUniqueUser Ex.notAnAuthor as
    return a
 {-   case as of
        []  -> return $ bad "You are not an author"
        [a] -> fm a
        _   -> return $ internal "program working incorrectly"
-}
handleError :: MonadServer m => ActionError -> m Response
handleError EInvalidEndpoint = do
    logError $ "Invalid endpoint"
    return $ notFound "Invalid endpoint"
handleError (ERequiredFieldMissing x) = do
    let str =  "Required field missing (" <> x <> ")"
    logError $ E.decodeUtf8 str
    return $ notFound $ E.decodeUtf8 str

withAuthAdmin y = withAuth y >>= withAdmin

withAuth :: (MonadServer m) => Maybe Token -> m (Maybe Ty.User)
withAuth mtoken = case mtoken of
    Nothing -> return Nothing
    Just token -> do
        users <- getUsersByToken token
        case users of 
            [] -> return Nothing
            [u] -> return $ Just u
            us  -> Ex.invalidUnique us

withAdmin :: (MonadServer m) => Maybe Ty.User -> m ()
withAdmin muser =
    when ((muser >>= Ty._u_admin) /= Just True) $ Ex.invalidEndpoint

validateUniqueUser :: (MonadServer m, GP.PrettyShow a) => m a -> [a] -> m a
validateUniqueUser x [] = x
validateUniqueUser _ [a] = return a
validateUniqueUser _ us  = Ex.invalidUnique us


withUser :: (MonadServer m) => Maybe Ty.User -> m Ty.User
withUser Nothing = Ex.unauthorized
withUser (Just u) = return u

getUsersByToken :: (MonadServer m) => Token -> m [Ty.User]
getUsersByToken token = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.token t JOIN news.users u ON t.user_id = u.user_id WHERE t.token = ?"
    users <- query str [token]
    return users


authenticate :: (MonadServer m) => Authenticate -> m Response
authenticate auth = do
    user <- getUserByLogin $ _au_login auth
    token <- fmap (T.pack) $ randomString 10
    token' <- addToken (Ty._u_id user) token
    return $ ok $ Ae.toJSON token'

addToken :: (MonadServer m) => Ty.UserId -> T.Text -> m T.Text
addToken id token = do
    let str = "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
        token' = (T.pack $ show id) <> token
    execute str (id, token', token')
    return token'


getUserByLogin :: (MonadServer m) => T.Text -> m Ty.User
getUserByLogin login = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.users u WHERE u.login = ?"
    users <- query str [login]
    validateUniqueUser Ex.invalidLogin users


createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis w cres = do
    let str = createQuery w
    debugStr <- formatQuery str cres
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers [CMC.Handler (sqlH w)] $ do
        execute str cres
        return (ok $ Ae.toJSON $ E.decodeUtf8 $ cName w <> " successfully created")
  where sqlH :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
        sqlH s e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to create new " <> cName s <> ", " <>
                    cUniqueField s <> " is already in use\n" <>
                    "SqlError: " <> PS.sqlErrorMsg e
                return $ bad $ E.decodeUtf8 $ cName s <> " with such " <> cUniqueField s <> " already exists."
            | foreignKeyViolated e = do
                let errmsg = 
                     "Failed to create new " <> cName s <> ", " <> cForeign s <> " is invalid"
                logError $ E.decodeUtf8 $ errmsg
                return $ bad $ E.decodeUtf8 errmsg
            | otherwise = logError (T.pack $ displayException e)
                >> return (internal "Internal error")



getThis' :: (FromSQL s, MonadServer m) => s -> Get s -> m [MType s]
getThis' x g = do
        let (qu, pars) = selectQuery x g
        debugStr <- formatQuery qu pars
        logDebug $ T.pack $ show debugStr
        query qu pars




-- добавить обработку исключений!!!
getThis :: (FromSQL s, MonadServer m) => s -> Get s -> m Response
getThis x g = do
--    cat <- f x g
    cat <- getThis' x g
    logDebug $ T.pack $ GP.defaultPretty cat
    let val = Ae.toJSON cat
    return $ Response NHT.ok200 val



deleteThis :: (MonadServer m, DeleteSQL s) => s -> Del s -> m Response
deleteThis s d = do
    let (str, params) = deleteQuery s d
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
        num <- execute str params
        actWithOne (AWOd s) num



editThis :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Response
editThis s u = case updateParams s u of
  Nothing -> undefined -- bad request
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers (Ex.defaultHandlers "editThis") $ do
        num <- execute str params
        actWithOne (AWOu s) num

