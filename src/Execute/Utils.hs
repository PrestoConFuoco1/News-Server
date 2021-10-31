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

withAuthAdmin ::
       (CMC.MonadThrow m) => D.Handle m -> Maybe T.Token -> m ()
withAuthAdmin h y = do
    muser <- withAuth h y
    checkAdmin h muser

withAuthor ::
       (CMC.MonadThrow m) => D.Handle m -> Maybe T.Token -> m T.Author
withAuthor h y = do
    muser <- withAuth h y
    user <- maybeUserToUser h muser
    mauthor <- D.userAuthor h (D.log h) user
    maybe Ex.notAnAuthor pure mauthor

withAuth ::
       (Monad m) => D.Handle m -> Maybe T.Token -> m (Maybe T.User)
withAuth h mtoken = do
    let fname = "withAuth: "
    D.logDebug h $ fname <> "trying to get user by token"
    case mtoken of
        Nothing -> do
            D.logDebug h $ fname <> "no token supplied"
            pure Nothing
        Just token -> do
            D.logDebug h $
                fname <>
                "searching for user with token = " <> T._t_token token
            D.getUserByToken h (D.log h) token

checkAdmin :: (CMC.MonadThrow m) => D.Handle m -> Maybe T.User -> m ()
checkAdmin h muser = do
    let fname = "checkAdmin: "
    case muser of
        Nothing -> do
            D.logDebug h $
                fname <> "no user found, throwing forbidden"
            Ex.throwForbidden
        Just user -> do
            D.logDebug h $ fname <> "found user"
            D.logDebug h $ GP.textPretty user
            if T._u_admin user /= Just True
                then do
                    D.logDebug h $
                        fname <>
                        "user is not admin, throwing forbidden"
                    Ex.throwForbidden
                else D.logDebug h $ fname <> "ok, user is admin"

maybeUserToUser ::
       (CMC.MonadThrow m) => D.Handle m -> Maybe T.User -> m T.User
maybeUserToUser h Nothing = do
    let fname = "maybeUserToUser: "
    D.logDebug h $ fname <> "no user found, throwing unauthorized"
    Ex.throwUnauthorized
maybeUserToUser h (Just u) = do
    let fname = "maybeUserToUser: "
    D.logDebug h $ fname <> "found user"
    D.logDebug h $ GP.textPretty u
    pure u

getUser ::
       (CMC.MonadThrow m)
    => D.Handle m
    -> Maybe T.User
    -> m T.APIResult
getUser h muser = T.RGetUser <$> maybeUserToUser h muser

authenticate ::
       (CMC.MonadThrow m)
    => D.Handle m
    -> T.Authenticate
    -> m T.APIResult
authenticate h auth = do
    muser <- D.getUserByLogin h (D.log h) $ T._au_login auth
    user <- maybe Ex.throwInvalidLogin pure muser
    when (T._u_passHash user /= T._au_passHash auth) $
        Ex.throwInvalidPassword
    token <- Text.pack <$> D.generateToken h 10
    token' <- D.addToken h (D.log h) (T._u_id user) token
    pure $ T.RGetToken token'

checkCategoryUpdate ::
       (CMC.MonadThrow m)
    => D.Handle m
    -> T.EditCategory
    -> m (Maybe T.ModifyError)
checkCategoryUpdate h editCat = do
    let funcName = "checkCategoryUpdate: "
    D.logDebug h $ funcName <> "action is:"
    D.logDebug h $ funcName <> GP.textPretty editCat
    S.withMaybe (T._ec_parentId editCat) (pure Nothing) $ \parentCatId -> do
        mParentCat <- D.getCategoryById h (D.log h) parentCatId
        D.logDebug h $ funcName <> "new parent category is:"
        D.logDebug h $ funcName <> GP.textPretty mParentCat
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
