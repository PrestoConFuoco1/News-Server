module Execute.Utils
    ( modifyErrorToApiResult
    , withAuthor
    , withAuthAdmin
    , withAuth
    , maybeUserToUser
    , getUser
    , authenticate
    , checkCategoryUpdate
    ) where

import qualified App.Database as D
import Control.Monad (when)
import qualified Control.Monad.Catch as CMC
import qualified Data.Text as T
import qualified Exceptions as Ex
import qualified GenericPretty as GP
import qualified Types as Y
import qualified Utils as S

withAuthAdmin ::
       (CMC.MonadThrow m) => D.Handle m -> Maybe Y.Token -> m ()
withAuthAdmin h y = do
    muser <- withAuth h y
    checkAdmin h muser

withAuthor ::
       (CMC.MonadThrow m) => D.Handle m -> Maybe Y.Token -> m Y.Author
withAuthor h y = do
    muser <- withAuth h y
    user <- maybeUserToUser h muser
    mauthor <- D.userAuthor h (D.log h) user
    maybe Ex.notAnAuthor pure mauthor

withAuth ::
       (Monad m) => D.Handle m -> Maybe Y.Token -> m (Maybe Y.User)
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
                "searching for user with token = " <> Y._t_token token
            D.getUserByToken h (D.log h) token

checkAdmin :: (CMC.MonadThrow m) => D.Handle m -> Maybe Y.User -> m ()
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
            if Y._u_admin user /= Just True
                then do
                    D.logDebug h $
                        fname <>
                        "user is not admin, throwing forbidden"
                    Ex.throwForbidden
                else do
                    D.logDebug h $ fname <> "ok, user is admin"
                    pure ()

maybeUserToUser ::
       (CMC.MonadThrow m) => D.Handle m -> Maybe Y.User -> m Y.User
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
    -> Maybe Y.User
    -> m Y.APIResult
getUser h muser = do
    user <- maybeUserToUser h muser
    pure $ Y.RGetUser user

authenticate ::
       (CMC.MonadThrow m)
    => D.Handle m
    -> Y.Authenticate
    -> m Y.APIResult
authenticate h auth = do
    muser <- D.getUserByLogin h (D.log h) $ Y._au_login auth
    user <- maybe Ex.throwInvalidLogin pure muser
    when (Y._u_passHash user /= Y._au_passHash auth) $
        CMC.throwM Ex.InvalidPassword
    token <- T.pack <$> D.generateToken h 10
    token' <- D.addToken h (D.log h) (Y._u_id user) token
    pure $ Y.RGetToken token'

modifyErrorToApiResult :: Y.Entity -> Y.ModifyError -> Y.APIResult
modifyErrorToApiResult ent (Y.MAlreadyInUse (Y.UniqueViolation field value)) =
    Y.RAlreadyInUse ent field value
modifyErrorToApiResult ent (Y.MInvalidForeign (Y.ForeignViolation field value)) =
    Y.RInvalidForeign ent field value
modifyErrorToApiResult ent Y.MConstraintViolated {..} =
    
modifyErrorToApiResult ent Y.MNoAction = Y.RNotFound ent

checkCategoryUpdate :: (CMC.MonadThrow m) => D.Handle m -> Y.EditCategory -> m (Maybe Y.ModifyError)
checkCategoryUpdate h editCat = do
    let funcName = "checkCategoryUpdate: "
    D.logDebug h $ funcName <> "action is:"
    D.logDebug h $ funcName <> GP.textPretty editCat
    S.withMaybe (Y._ec_parentId editCat) (pure Nothing) $ \parentCatId -> do
        mParentCat <- D.getCategoryById h (D.log h) parentCatId
        S.withMaybe mParentCat
            (pure $ Just $ Y.MInvalidForeign $ Y.ForeignViolation "check_parent_id" $ S.showText parentCatId)
            (pure . checkCategoryCycles (Y._ec_catId editCat))

checkCategoryCycles :: Y.CategoryId -> Y.Category -> Maybe Y.ModifyError
checkCategoryCycles cid newParentCat =
    if cid `elem` Y.getCategoryParents newParentCat
        then Just $ Y.MConstraintViolated $ Y.ConstraintViolation {
            Y._cv_field = "parent_id"
            , Y._cv_value = S.showText $ Y._cat_categoryId newParentCat
            , Y._cv_description = "failed: categories form a cycle"
            }
        else Nothing

{-
checkCategoryCycles cid newParentCat =
    if cyclesOk newParentCat
        then Nothing
        else Just $ Y.MInvalidForeign $ Y.ForeignViolation "cycles_parent_id" $ S.showText $ Y._cat_categoryId newParentCat
  where cyclesOk cat
            | cid == Y._cat_categoryId cat = False
            | otherwise = S.withMaybe (Y._cat_parentCategory cat) True cyclesOk
-}

testCat = Y.Category {
    Y._cat_categoryId = 3
    , Y._cat_description = "category_level3"
    , Y._cat_parentCategory = Just Y.Category {
        Y._cat_categoryId = 2
        , Y._cat_description = "category_level2"
        , Y._cat_parentCategory = Just Y.Category {
            Y._cat_categoryId = 1
            , Y._cat_description = "category_level1"
            , Y._cat_parentCategory = Nothing
            }
        }

    }

{-
    D.logDebug h $ funcName <> "checking for the category editing correctness"
    D.logDebug h $ funcName <> "action is:"
    D.logDebug h $ funcName <> GP.textPretty editCat
    --mCatToEdit <- 
    D.logDebug h $ funcName <> "the category to be edited is"
    D.logDebug h $ funcName <> "currently no real check is performed"
    return ()
-}
