module Exceptions where

import Control.Monad.Catch as CMC (Handler(..), catches, SomeException, throwM, Exception(..), MonadCatch(..))
import MonadTypes (MonadServer, logError)
import Database.PostgreSQL.Simple as PS (sqlState, SqlError(..), QueryError)
import qualified Data.Text as T (Text, pack)
import qualified GenericPretty as GP (PrettyShow(..), defaultPretty)

import Result as U
import Data.Text.Encoding as E (decodeUtf8)

data ServerException =
      Default
    | Unauthorized
    | InvalidUniqueEntities
    | Forbidden
    | InvalidLogin
    | InvalidPassword
    | FailedInsertionWithoutException T.Text
    | CreatedMoreThanOneEntity T.Text [Int]
    | DraftNotFound Int
    | NotAnAuthor
    | DeleteNotFound T.Text
    | UpdateNotFound T.Text
    | InvalidUpdateOrDelete T.Text
    | TokenShared [Int]
    deriving (Show)


throwForbidden :: (MonadServer m) => m a
throwForbidden = CMC.throwM Forbidden

mainErrorHandler :: (MonadServer m) => ServerException -> m Response
mainErrorHandler Default = return $ U.internal U.internalErrorMsg
mainErrorHandler Unauthorized = return $ U.unauthorized U.unauthorizedMsg
mainErrorHandler InvalidUniqueEntities = return $ U.internal U.internalErrorMsg
mainErrorHandler Forbidden = return $ U.bad U.invalidEndpointMsg
mainErrorHandler InvalidLogin = return $ U.bad $ U.invalidLoginMsg
mainErrorHandler InvalidPassword = return $ U.bad U.invalidPasswordMsg
mainErrorHandler (FailedInsertionWithoutException text) = return $ U.bad "failed to insert"
mainErrorHandler (CreatedMoreThanOneEntity ent ids) = return $ U.bad U.badInsert
mainErrorHandler (DraftNotFound id) = return $ U.bad $ "draft with id = " <> T.pack (show id) <> " not found"
mainErrorHandler NotAnAuthor = return $ U.bad U.notAnAuthorMsg
mainErrorHandler (DeleteNotFound text) = return $ U.bad "Not found"
mainErrorHandler (UpdateNotFound text) = return $ U.bad "Not found"
mainErrorHandler (InvalidUpdateOrDelete text) = return $ U.bad text
mainErrorHandler (TokenShared xs) = logError ("Token shared between users with id in " <> T.pack (show xs))
        >> return (U.internal U.internalErrorMsg)


instance CMC.Exception ServerException

throwTokenShared :: (MonadServer m) => [Int] -> m a
throwTokenShared lst = CMC.throwM $ TokenShared lst

throwDelNotFound  :: (MonadServer m) => T.Text -> m a
throwDelNotFound ent = CMC.throwM $ DeleteNotFound ent


throwUpdNotFound  :: (MonadServer m) => T.Text -> m a
throwUpdNotFound ent = CMC.throwM $ UpdateNotFound ent


throwDraftNotFound :: (MonadServer m) => Int -> m a
throwDraftNotFound draft = CMC.throwM $ DraftNotFound draft

throwBadInsert :: (MonadServer m) => T.Text -> m a
throwBadInsert ent = CMC.throwM $ FailedInsertionWithoutException ent

throwBad2Insert :: (MonadServer m) => T.Text -> [Int] -> m a
throwBad2Insert ent ids = CMC.throwM $ CreatedMoreThanOneEntity ent ids

throwDefault :: (MonadServer m) => m a
throwDefault = CMC.throwM Default

invalidUpdDel text = CMC.throwM $ InvalidUpdateOrDelete text

unauthorized :: (MonadServer m) => m a
unauthorized = CMC.throwM $ Unauthorized

invalidUnique :: (MonadServer m, GP.PrettyShow a) => [a] -> m b
invalidUnique xs = do
    logError $ T.pack $ GP.defaultPretty xs
    CMC.throwM $ InvalidUniqueEntities


invalidLogin :: (MonadServer m) => m a
invalidLogin = CMC.throwM $ InvalidLogin

notAnAuthor :: (MonadServer m) => m a
notAnAuthor = CMC.throwM $ NotAnAuthor

defaultSqlHandler :: (MonadServer m) => T.Text -> PS.SqlError -> m a
defaultSqlHandler funcMsg e = do
    logError "Some not caught exception"
    logError funcMsg
    logError (T.pack $ displayException e)
    CMC.throwM Default

queryErrorHandler :: (MonadServer m) => T.Text -> PS.QueryError -> m a
queryErrorHandler funcMsg e = do
    logError "Query is used to perform an INSERT-like operation, \
             \or execute is used to perform a SELECT-like operation."
    logError funcMsg
    logError (T.pack $ displayException e)
    CMC.throwM Default

resultErrorHandler :: (MonadServer m) => T.Text -> PS.QueryError -> m a
resultErrorHandler funcMsg e = do
    logError "Conversion from a SQL value to Haskell value failed."
    logError funcMsg
    logError (T.pack $ displayException e)
    CMC.throwM Default

defaultHandlers :: (MonadServer m) => T.Text -> [Handler m a]
defaultHandlers funcMsg = [Handler $ queryErrorHandler funcMsg,
                           Handler $ resultErrorHandler funcMsg,
                           Handler $ defaultSqlHandler funcMsg]


draftActionHandler :: (MonadServer m) => T.Text -> SomeException -> m a
draftActionHandler action e =
    let str = "Failed to " <> action <> " draft."
    in  logError (str <> ", all changes are discarded")
                            >> CMC.throwM e

publishHandler :: (MonadServer m) => SomeException -> m a
publishHandler e = draftActionHandler "publish" e
draftCreateHandler :: (MonadServer m) => SomeException -> m a
draftCreateHandler e = draftActionHandler "create" e
draftEditHandler :: (MonadServer m) => SomeException -> m a
draftEditHandler e = draftActionHandler "edit" e



withExceptionHandlers :: (Foldable f, CMC.MonadCatch m) => f (CMC.Handler m a) -> m a-> m a
withExceptionHandlers = flip CMC.catches

withHandler :: (CMC.MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
withHandler = flip CMC.catch

defaultMainHandler :: (MonadServer m) => SomeException -> m Response
defaultMainHandler e = do
    logError $ T.pack $ displayException e
    return $ U.internal U.internalErrorMsg


uniqueConstraintViolated e = PS.sqlState e == "23505"
foreignKeyViolated e = PS.sqlState e == "23503"
constraintViolated e = PS.sqlState e == "23514"



--SqlError {sqlState = "23514", sqlExecStatus = FatalError,
--sqlErrorMsg = "new row for relation \"author\" violates check constraint
-- \"author_description_check\"", sqlErrorDetail =
-- "Failing row contains (26, 2, ).", sqlErrorHint = ""}

creUpdExceptionHandler :: (MonadServer m) => T.Text -> T.Text -> T.Text -> PS.SqlError -> m Response
creUpdExceptionHandler name unique foreign1 e
    | uniqueConstraintViolated e = do
        logError $
            "Failed to create new " <> name <> ", " <>
            unique <> " is already in use\n" <>
            "SqlError: " <> (E.decodeUtf8 $ PS.sqlErrorMsg e)
        return $ bad $ name <> " with such " <> unique <> " already exists."
    | foreignKeyViolated e = do
        let errmsg = 
             "Failed to create new " <> name <> ", " <> foreign1 <> " is invalid"
        logError $ errmsg
        return $ bad $ errmsg
    | otherwise = CMC.throwM e


