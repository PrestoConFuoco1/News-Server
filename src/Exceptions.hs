module Exceptions where

import Control.Monad.Catch as CMC (Handler(..), catches, SomeException, throwM, Exception(..), MonadCatch(..))
import MonadTypes (MonadServer, logError)
import Database.PostgreSQL.Simple as PS (sqlState, SqlError, QueryError)
import qualified Data.Text as T (Text, pack)
import qualified GenericPretty as GP (PrettyShow(..), defaultPretty)

import Execute.Utils as U
import Execute.Types (Response(..))

data ServerException =
      Default
    | Unauthorized
    | InvalidUniqueEntities
    | InvalidEndpoint
    | InvalidLogin
    | InvalidPassword
    | NotAnAuthor
    | InvalidUpdateOrDelete T.Text
    deriving (Show)

instance CMC.Exception ServerException

throwDefault :: (MonadServer m) => m a
throwDefault = CMC.throwM Default

invalidUpdDel text = CMC.throwM $ InvalidUpdateOrDelete text

unauthorized :: (MonadServer m) => m a
unauthorized = CMC.throwM $ Unauthorized

invalidUnique :: (MonadServer m, GP.PrettyShow a) => [a] -> m b
invalidUnique xs = do
    logError $ T.pack $ GP.defaultPretty xs
    CMC.throwM $ InvalidUniqueEntities

invalidEndpoint :: (MonadServer m) => m a
invalidEndpoint = CMC.throwM $ InvalidEndpoint

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



withExceptionHandlers :: (Foldable f, CMC.MonadCatch m) => f (CMC.Handler m a) -> m a-> m a
withExceptionHandlers = flip CMC.catches



mainErrorHandler :: (Monad m) => ServerException -> m Response
mainErrorHandler Default = return $ U.internal U.internalErrorMsg
mainErrorHandler Unauthorized = return $ U.unauthorized U.unauthorizedMsg
mainErrorHandler InvalidUniqueEntities = return $ U.internal U.internalErrorMsg
mainErrorHandler InvalidEndpoint = return $ U.bad U.invalidEndpointMsg
mainErrorHandler InvalidLogin = return $ U.bad $ U.invalidLoginMsg
mainErrorHandler InvalidPassword = return $ U.bad U.invalidPasswordMsg
mainErrorHandler NotAnAuthor = return $ U.bad U.notAnAuthorMsg
mainErrorHandler (InvalidUpdateOrDelete text) = return $ U.bad text

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

