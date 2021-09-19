

module Execute where



import qualified Network.HTTP.Types as NHT
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception

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

executeAction :: MonadServer m => WhoWhat Action -> m Response
executeAction (WhoWhat y (AAuthors x)) = executeAuthor (WhoWhat y x)
executeAction (WhoWhat y (ACategory x)) = executeCategory (WhoWhat y x)
executeAction (WhoWhat y (APosts x)) = executePosts (WhoWhat y x)
executeAction (WhoWhat y (ATags x)) = executeTags (WhoWhat y x)
executeAction (WhoWhat y (AUsers x)) = executeUsers (WhoWhat y x)
executeAction (WhoWhat y (AAuth x)) = authenticate x

executePosts (WhoWhat y (Read x)) = getThis postDummy x

executeAuthor (WhoWhat y (Read x)) =
    withAuth y . withAdmin $ getThis authorDummy x
executeAuthor (WhoWhat y (Create x)) =
    withAuth y . withAdmin $ createThis dummyCAuthor x
executeAuthor (WhoWhat y (Update x)) =
    withAuth y . withAdmin $ editThis dummyUAuthor x
executeAuthor (WhoWhat y (Delete x)) =
    withAuth y . withAdmin $ deleteThis dummyDAuthor x

executeTags (WhoWhat y (Read x)) = getThis tagDummy x
executeTags (WhoWhat y (Create x)) = withAuth y . withAdmin $ createThis dummyCTag x
executeTags (WhoWhat y (Update x)) =  withAuth y . withAdmin $ editThis dummyUTag x
executeTags (WhoWhat y (Delete x)) = withAuth y . withAdmin $ deleteThis dummyDTag x


executeCategory (WhoWhat y (Read x)) = getThis catDummy x
executeCategory (WhoWhat y (Create x)) = withAuth y . withAdmin $ createThis dummyCCat x
executeCategory (WhoWhat y (Update x)) =  withAuth y . withAdmin $ editThis dummyUCat x
executeCategory (WhoWhat y (Delete x)) = withAuth y . withAdmin $ deleteThis dummyDCat x

executeUsers (WhoWhat y (Create x)) = createThis dummyCUser x
executeUsers (WhoWhat y (Delete x)) = withAuth y . withAdmin $ deleteThis dummyDUser x
executeUsers (WhoWhat y (Read GetProfile)) = withAuth y getUser


withExceptionHandlers :: (Foldable f, CMC.MonadCatch m) => f (CMC.Handler m a) -> m a-> m a
withExceptionHandlers = flip CMC.catches

getUser :: (MonadServer m) => Maybe Ty.User -> m Response
getUser Nothing = return $ unauthorized "Unauthorized, use /auth"
getUser (Just u) = let val = Ae.toJSON u
                   in  return $ Response NHT.ok200 val


handleError :: MonadServer m => ActionError -> m Response
handleError EInvalidEndpoint = do
    logError $ "Invalid endpoint"
    return $ notFound "Invalid endpoint"
handleError (ERequiredFieldMissing x) = do
    let str =  "Required field missing (" <> x <> ")"
    logError $ E.decodeUtf8 str
    return $ notFound str


withAuth :: (MonadServer m) => Maybe Token -> (Maybe Ty.User -> m Response) -> m Response
withAuth mtoken m = case mtoken of
    Nothing -> m Nothing
    Just token -> do
        users <- getUserByToken token
        case users of
            [] -> m Nothing
            [u] -> m (Just u) -- ok
            lst -> undefined

withAdmin :: (MonadServer m) => m Response -> Maybe Ty.User -> m Response
withAdmin m muser
  | (muser >>= Ty._u_admin) == Just True = m
  | otherwise = return $ notFound invalidEndpointMsg

getUserByToken :: (MonadServer m) => Token -> m [Ty.User]
getUserByToken token = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.token t JOIN news.users u ON t.user_id = u.user_id WHERE t.token = ?"
    users <- query str [token]
    return users


uniqueConstraintViolated e = PS.sqlState e == "23505"
foreignKeyViolated e = PS.sqlState e == "23503"


authenticate :: (MonadServer m) => Authenticate -> m Response
authenticate auth = do
    users <- getUserByLogin $ _au_login auth
    case users of
        [] -> return $ bad "Login not found" -- invalid login
        x:y:xs -> undefined -- error in the database
        [x] -> do
            token <- fmap (T.pack) $ randomString 10
            addToken (Ty._u_id x) token

addToken :: (MonadServer m) => Ty.UserId -> T.Text -> m Response
addToken id token = do
    let str = "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
        token' = (T.pack $ show id) <> token
    execute str (id, token', token')
    return $ ok $ E.encodeUtf8 token'


getUserByLogin :: (MonadServer m) => T.Text -> m [Ty.User]
getUserByLogin login = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.users u WHERE u.login = ?"
    users <- query str [login]
    return users



createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis w cres = do
    let str = createQuery w
    debugStr <- formatQuery str cres
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers [CMC.Handler (sqlH w)] $ do
        execute str cres
        return (ok $ cName w <> " successfully created")
  where sqlH :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
        sqlH s e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to create new " <> cName s <> ", " <>
                    cUniqueField s <> " is already in use\n" <>
                    "SqlError: " <> PS.sqlErrorMsg e
                return $ bad $ cName s <> " with such " <> cUniqueField s <> " already exists."
            | foreignKeyViolated e = do
                let errmsg = 
                     "Failed to create new " <> cName s <> ", " <> cForeign s <> " is invalid"
                logError $ E.decodeUtf8 $ errmsg
                return $ bad errmsg
            | otherwise = logError (T.pack $ displayException e)
                >> return (internal "Internal error")






-- добавить обработку исключений!!!
getThis :: (FromSQL s, MonadServer m) => s -> Get s -> m Response
getThis x g = do
    cat <- f x g
    logDebug $ T.pack $ GP.defaultPretty cat
    let val = Ae.toJSON cat
    return $ Response NHT.ok200 val
  where f :: (FromSQL s, MonadServer m) => s -> Get s -> m [MType s]
        f x g = do
            let qu = selectQuery x g
            debugStr <- uncurry formatQuery qu
            logDebug $ T.pack $ show debugStr
            uncurry query qu


deleteThis :: (MonadServer m, DeleteSQL s) => s -> Del s -> m Response
deleteThis s d = do
    let str = deleteQuery s
    debugStr <- formatQuery str d
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers [CMC.Handler (sqlH s)] $ do
        num <- execute str d
        actWithOne (AWOd s) num

  where sqlH :: (MonadServer m, DeleteSQL s) => s -> PS.SqlError -> m Response
        sqlH w e = logError (T.pack $ displayException e) >> return (internal "Internal error")


editThis :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Response
editThis s u = case updateParams s u of
  Nothing -> undefined -- bad request
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    withExceptionHandlers [CMC.Handler sqlH] $ do
        num <- execute str params
        actWithOne (AWOu s) num
--        return (ok $ cName w <> " successfully created")
  --      `CMC.catches` [CMC.Handler (sqlH w)]


 --   (execute str (vals ++ identifParams s u)
  --      >> return (ok $ uName s <> " successfully edited"))
   --     `CMC.catches` [CMC.Handler sqlH]
  where sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e = logError (T.pack $ displayException e) >> return (internal "Internal error")

