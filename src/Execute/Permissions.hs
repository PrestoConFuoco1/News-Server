{-# LANGUAGE RecordWildCards #-}
module Execute.Permissions where


import Control.Monad (when)
import Database.Read
import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal, MonadLog)
import Types
import Exceptions as Ex
import Execute.Utils
import qualified Data.Aeson as Ae
import Execute.Database
import qualified Control.Monad.Catch as CMC

withAuthAdmin y = withAuth y >>= checkAdmin
withAuthor y = 
    withAuth y >>= maybeUserToUser >>= userAuthor

userAuthor :: (MonadServer m) => User -> m Author
userAuthor u = do
    as <- getThis' authorDummy (GetAuthors $ Just $ _u_id u)
    a  <- validateUnique Ex.notAnAuthor as
    return a



withAuth :: (MonadServer m) => Maybe Token -> m (Maybe User)
withAuth mtoken = case mtoken of
    Nothing -> return Nothing
    Just token -> do
        muser <- getUsersByToken token
        return muser

checkAdmin :: (MonadServer m) => Maybe User -> m ()
checkAdmin muser =
   when ((muser >>= _u_admin) /= Just True) $ Ex.throwForbidden



maybeUserToUser :: (MonadServer m) => Maybe User -> m User
maybeUserToUser Nothing = Ex.unauthorized
maybeUserToUser (Just u) = return u

getUsersByToken :: (MonadServer m) => Token -> m (Maybe User)
getUsersByToken token = do
    users <- getThis' userTokenDummy token
 --   users <- query str [token]
    user <- validateUnique2 (return Nothing) (Ex.throwTokenShared $ map _u_id users) $ map Just users
    return user


