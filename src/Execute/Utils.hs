module Execute.Utils
    ( withAuthor
    , withAuthAdmin
    , withAuth
    , withUser
    , maybeUserToUser
    , getUser
    , authenticate
    , checkCategoryUpdate
    , checkCategoryCycles
    ) where

import qualified Crypto as Crypto
import qualified App.Database as D
import qualified App.Logger as L
import Control.Monad (unless)
import qualified Control.Monad.Catch as CMC
import qualified Data.Text as Text
import qualified Exceptions as Ex
import qualified GenericPretty as GP
import qualified Types as T
import qualified Utils as S

withAuthAdmin ::
       (CMC.MonadThrow m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> Maybe T.Token
    -> m ()
withAuthAdmin h logger y = do
    muser <- withAuth h logger y
    checkAdmin h logger muser

withAuthor ::
       (CMC.MonadThrow m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> Maybe T.Token
    -> m T.Author
withAuthor h logger y = do
    muser <- withAuth h logger y
    user <- maybeUserToUser h logger muser
    mauthor <- D.userAuthor h logger user
    maybe Ex.notAnAuthor pure mauthor

withUser ::
       (CMC.MonadThrow m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> Maybe T.Token
    -> m T.User
withUser h logger y = do
    muser <- withAuth h logger y
    maybeUserToUser h logger muser

withAuth ::
       (Monad m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> Maybe T.Token
    -> m (Maybe T.User)
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
                "searching for user with token = " <> T.tToken token
            D.getUserByToken h logger token

checkAdmin ::
       (CMC.MonadThrow m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> Maybe T.User
    -> m ()
checkAdmin _ logger muser = do
    let fname = "checkAdmin: "
    case muser of
        Nothing -> do
            L.logDebug logger $
                fname <> "no user found, throwing forbidden"
            Ex.throwForbidden
        Just user -> do
            L.logDebug logger $ fname <> "found user"
            L.logDebug logger $ GP.textPretty user
            if T.userAdmin user /= Just True
                then do
                    L.logDebug logger $
                        fname <>
                        "user is not admin, throwing forbidden"
                    Ex.throwForbidden
                else L.logDebug logger $ fname <> "ok, user is admin"

maybeUserToUser ::
       (CMC.MonadThrow m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> Maybe T.User
    -> m T.User
maybeUserToUser _ logger Nothing = do
    let fname = "maybeUserToUser: "
    L.logDebug logger $
        fname <> "no user found, throwing unauthorized"
    Ex.throwUnauthorized
maybeUserToUser _ logger (Just u) = do
    let fname = "maybeUserToUser: "
    L.logDebug logger $ fname <> "found user"
    L.logDebug logger $ GP.textPretty u
    pure u

getUser ::
       (CMC.MonadThrow m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> Maybe T.User
    -> m T.APIResult
getUser h logger muser = T.RGetUser <$> maybeUserToUser h logger muser

authenticate ::
       (CMC.MonadThrow m)
    => D.AuthHandler m
    -> L.LoggerHandler m
    -> T.Authenticate
    -> m T.APIResult
authenticate h logger auth = do
    muser <- D.getUserByLogin h logger $ T.auLogin auth
    user <- maybe Ex.throwInvalidLogin pure muser
    unless
        (Crypto.validatePassword (T.auPassHash auth) (T.userPassHash user))
        Ex.throwInvalidPassword
    token <- Text.pack <$> D.generateToken h 10
    token' <- D.addToken h logger (T.userId user) token
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
    S.withMaybe (T.ecParentId editCat) (pure Nothing) $ \parentCatId -> do
        mParentCat <- D.getCategoryById catsH logger parentCatId
        L.logDebug logger $ funcName <> "new parent category is:"
        L.logDebug logger $ funcName <> GP.textPretty mParentCat
        S.withMaybe
            mParentCat
            (pure $
             Just $
             T.MInvalidForeign $
             T.ForeignViolation "parent_id" $ S.showText parentCatId)
            (pure . checkCategoryCycles (T.ecCategoryId editCat))

checkCategoryCycles ::
       T.CategoryId -> T.Category -> Maybe T.ModifyError
checkCategoryCycles cid newParentCat =
    if cid `notElem` T.getCategoryParents newParentCat
        then Nothing
        else Just $
             T.MConstraintViolated $
             T.ConstraintViolation
                 { T.cvField = "parent_id"
                 , T.cvValue = S.showText $ T.cCategoryId newParentCat
                 , T.cvDescription = "categories form a cycle"
                 }
