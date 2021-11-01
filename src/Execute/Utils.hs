module Execute.Utils
    ( withAuthor
    , withAuthAdmin
    , withAuth
    , maybeUserToUser
    , getUser
    , authenticate
    , checkCategoryUpdate
    , checkCategoryCycles
    ) where

import qualified App.Database as D
import Control.Monad (when)
import qualified Control.Monad.Catch as CMC
import qualified Data.Text as Text
import qualified Exceptions as Ex
import qualified GenericPretty as GP
import qualified Types as T
import qualified Utils as S
import qualified App.Logger as L

withAuthAdmin ::
       (CMC.MonadThrow m) => D.AuthHandler m -> L.LoggerHandler m -> Maybe T.Token -> m ()
withAuthAdmin h logger y = do
    muser <- withAuth h logger y
    checkAdmin h logger muser

withAuthor ::
       (CMC.MonadThrow m) => D.AuthHandler m -> L.LoggerHandler m -> Maybe T.Token -> m T.Author
withAuthor h logger y = do
    muser <- withAuth h logger y
    user <- maybeUserToUser h logger muser
    mauthor <- D.userAuthor h logger user
    maybe Ex.notAnAuthor pure mauthor

withAuth ::
       (Monad m) => D.AuthHandler m -> L.LoggerHandler m -> Maybe T.Token -> m (Maybe T.User)
withAuth h logger mtoken = do
    let fname = "withAuth: "
    L.logDebug logger $ fname <> "trying to get user by token"
    case mtoken of
        Nothing -> do
            L.logDebug logger $ fname <> "no token supplied"
            pure Nothing
        Just token -> do
            L.logDebug logger $
                fname <>
                "searching for user with token = " <> T._t_token token
            D.getUserByToken h logger token

checkAdmin :: (CMC.MonadThrow m) => D.AuthHandler m -> L.LoggerHandler m -> Maybe T.User -> m ()
checkAdmin h logger muser = do
    let fname = "checkAdmin: "
    case muser of
        Nothing -> do
            L.logDebug logger $
                fname <> "no user found, throwing forbidden"
            Ex.throwForbidden
        Just user -> do
            L.logDebug logger $ fname <> "found user"
            L.logDebug logger $ GP.textPretty user
            if T._u_admin user /= Just True
                then do
                    L.logDebug logger $
                        fname <>
                        "user is not admin, throwing forbidden"
                    Ex.throwForbidden
                else L.logDebug logger $ fname <> "ok, user is admin"

maybeUserToUser ::
       (CMC.MonadThrow m) => D.AuthHandler m -> L.LoggerHandler m -> Maybe T.User -> m T.User
maybeUserToUser h logger Nothing = do
    let fname = "maybeUserToUser: "
    L.logDebug logger $ fname <> "no user found, throwing unauthorized"
    Ex.throwUnauthorized
maybeUserToUser h logger (Just u) = do
    let fname = "maybeUserToUser: "
    L.logDebug logger $ fname <> "found user"
    L.logDebug logger $ GP.textPretty u
    pure u

getUser ::
       (CMC.MonadThrow m)
    => D.AuthHandler m -> L.LoggerHandler m
    -> Maybe T.User
    -> m T.APIResult
getUser h logger muser = T.RGetUser <$> maybeUserToUser h logger muser

authenticate ::
       (CMC.MonadThrow m)
    => D.AuthHandler m -> L.LoggerHandler m
    -> T.Authenticate
    -> m T.APIResult
authenticate h logger auth = do
    muser <- D.getUserByLogin h logger $ T._au_login auth
    user <- maybe Ex.throwInvalidLogin pure muser
    when (T._u_passHash user /= T._au_passHash auth) $
        Ex.throwInvalidPassword
    token <- Text.pack <$> D.generateToken h 10
    token' <- D.addToken h logger (T._u_id user) token
    pure $ T.RGetToken token'

checkCategoryUpdate ::
       (CMC.MonadThrow m)
    => D.CatsHandler m
    -> L.LoggerHandler m
    -> T.EditCategory
    -> m (Maybe T.ModifyError)
checkCategoryUpdate catsH logger editCat = do
    let funcName = "checkCategoryUpdate: "
    L.logDebug logger $ funcName <> "action is:"
    L.logDebug logger $ funcName <> GP.textPretty editCat
    S.withMaybe (T._ec_parentId editCat) (pure Nothing) $ \parentCatId -> do
        mParentCat <- D.getCategoryById catsH logger parentCatId
        L.logDebug logger $ funcName <> "new parent category is:"
        L.logDebug logger $ funcName <> GP.textPretty mParentCat
        S.withMaybe
            mParentCat
            (pure $
             Just $
             T.MInvalidForeign $
             T.ForeignViolation "parent_id" $ S.showText parentCatId)
            (pure . checkCategoryCycles (T._ec_catId editCat))

checkCategoryCycles ::
       T.CategoryId -> T.Category -> Maybe T.ModifyError
checkCategoryCycles cid newParentCat =
    if not $ cid `elem` T.getCategoryParents newParentCat
        then Nothing
        else Just $
             T.MConstraintViolated $
             T.ConstraintViolation
                 { T._cv_field = "parent_id"
                 , T._cv_value =
                       S.showText $ T._cat_categoryId newParentCat
                 , T._cv_description = "categories form a cycle"
                 }
