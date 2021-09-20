module Exceptions where

import Control.Monad.Catch as CMC
import MonadTypes
import Database.PostgreSQL.Simple as PS
import qualified Data.Text as T
import qualified GenericPretty as GP

import qualified Data.Aeson as Ae
import ExecuteUtils as U
import ExecuteTypes

data ServerException =
      Default
    | Unauthorized
    | InvalidUniqueEntities
    | InvalidEndpoint
    | InvalidLogin
    | NotAnAuthor
    deriving (Show)

instance CMC.Exception ServerException



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



uniqueConstraintViolated e = PS.sqlState e == "23505"
foreignKeyViolated e = PS.sqlState e == "23503"
constraintViolated e = PS.sqlState e == "23514"


withExceptionHandlers :: (Foldable f, CMC.MonadCatch m) => f (CMC.Handler m a) -> m a-> m a
withExceptionHandlers = flip CMC.catches



mainErrorHandler :: (Monad m) => ServerException -> m Response
mainErrorHandler Default = return $ U.internal U.internalErrorMsg
mainErrorHandler Unauthorized = return $ U.unauthorized U.unauthorizedMsg
mainErrorHandler InvalidUniqueEntities = return $ U.internal U.internalErrorMsg
mainErrorHandler InvalidEndpoint = return $ U.bad U.invalidEndpointMsg
mainErrorHandler InvalidLogin = return $ U.bad $ U.unauthorizedMsg <> " invalid login"
mainErrorHandler NotAnAuthor = return $ U.bad U.notAnAuthorMsg

defaultMainHandler :: (MonadServer m) => SomeException -> m Response
defaultMainHandler e = do
    logError $ T.pack $ displayException e
    return $ U.internal U.internalErrorMsg
