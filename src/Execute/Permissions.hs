{-# LANGUAGE RecordWildCards #-}
module Execute.Permissions where


--import Execute.Actions
import Control.Monad (when)
import Action.Types (WhoWhat (..), Token)
import Database.Read
import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Types as Ty
import Action.Authors
import Exceptions as Ex
import Execute.Utils
import qualified Data.Aeson as Ae

withAuthAdmin y = withAuth y >>= checkAdmin
withAuthor y = 
    withAuth y >>= maybeUserToUser >>= userAuthor

userAuthor :: (MonadServer m) => Ty.User -> m Ty.Author
userAuthor u = do
    as <- getThis' authorDummy (GetAuthors $ Just $ Ty._u_id u)
    a  <- validateUnique Ex.notAnAuthor as
    return a



withAuth :: (MonadServer m) => Maybe Token -> m (Maybe Ty.User)
withAuth mtoken = case mtoken of
    Nothing -> return Nothing
    Just token -> do
        muser <- getUsersByToken token
        return muser

checkAdmin :: (MonadServer m) => Maybe Ty.User -> m ()
checkAdmin muser =
   when ((muser >>= Ty._u_admin) /= Just True) $ Ex.throwForbidden



maybeUserToUser :: (MonadServer m) => Maybe Ty.User -> m Ty.User
maybeUserToUser Nothing = Ex.unauthorized
maybeUserToUser (Just u) = return u

getUsersByToken :: (MonadServer m) => Token -> m (Maybe Ty.User)
getUsersByToken token = do
    let str = "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.get_users_by_token WHERE token = ?"
   
      --     \FROM news.token t JOIN news.users u ON t.user_id = u.user_id WHERE t.token = ?"
    users <- query str [token]
    user <- validateUnique2 (return Nothing) (Ex.throwTokenShared $ map Ty._u_id users) $ map Just users
    return user


getUsersByToken' :: (MonadServer m) => Token -> m (Maybe Ty.User)
getUsersByToken' token = do
    let str = "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.get_users_by_token WHERE token = ?"
   
    users <- query str [token]
    user <- validateUnique2 (return Nothing) (Ex.throwTokenShared $ map Ty._u_id users) $ map Just users
    return user


