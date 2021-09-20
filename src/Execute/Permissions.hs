{-# LANGUAGE RecordWildCards #-}
module Execute.Permissions where


import Execute.Actions
import Control.Monad (when)
import Action.Types (WhoWhat (..), Token)
import Database.Read
import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Types as Ty
import Action.Authors.Types
import Exceptions as Ex


withAuthAdmin y = withAuth y >>= withAdmin

userAuthor :: (MonadServer m) => Ty.User -> m Ty.Author
userAuthor u = do
    as <- getThis' authorDummy (GetAuthors $ Just $ Ty._u_id u)
    a  <- validateUnique Ex.notAnAuthor as
    return a



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


