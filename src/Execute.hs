{-# LANGUAGE RecordWildCards #-}

module Execute where



import qualified Data.Text as T (pack)


import Action.RequestToAction
import Action.Common
import Database.Read
import Database.Create
import Database.Delete
import Database.Update
import Control.Exception (SomeException)

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal, MonadLog)
import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Types as Ty
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch, catch, MonadThrow)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Result
import Execute.Types
import Execute.Utils
import Execute.Draft

import Types

import Exceptions as Ex
import Execute.Database

import Database.SqlQueryTypes
--import Execute.Permissions
--import Execute.Actions
import MonadNews

--executeAction :: MonadServer m => WhoWhat Action -> m Response
executeAction :: MonadNews m => WhoWhat Action -> m Response
executeAction (WhoWhat y (AAuthors x)) = executeAuthor (WhoWhat y x)
executeAction (WhoWhat y (ACategory x)) = executeCategory (WhoWhat y x)
executeAction (WhoWhat y (APosts x)) = executePosts (WhoWhat y x)
executeAction (WhoWhat y (ATags x)) = executeTags (WhoWhat y x)
executeAction (WhoWhat y (AUsers x)) = executeUsers (WhoWhat y x)
executeAction (WhoWhat y (AAuth x)) = authenticate x
executeAction (WhoWhat y (AComments x)) = executeComments (WhoWhat y x)
executeAction (WhoWhat y (ADrafts x)) = executeDraft (WhoWhat y x)
executeAction (WhoWhat y (APublish x)) = executePublish (WhoWhat y x)
--executeAction _ = undefined

executePosts (WhoWhat y (GC x)) = getThis1 getComments x

executePosts (WhoWhat y (AP (Read x))) = do
--    let where1 = postsWhereClause1 x
--        (SqlQuery clause params) = whereToQuery where1
--    logDebug $ T.pack $ show $ where1
--    evaluated <- formatQuery clause params
--    logDebug $ E.decodeUtf8 evaluated
--    getThis1 (getThisPaginated' postDummy) x
    getThis1 getPosts x


executeAuthor (WhoWhat y (Read x)) =
    withAuthAdmin y >> getThis1 getAuthors x
executeAuthor (WhoWhat y (Create x)) =
    withAuthAdmin y >> createThis1 "author" createAuthor x
executeAuthor (WhoWhat y (Update x)) =
    withAuthAdmin y >> editThis1 "author" editAuthor x
executeAuthor (WhoWhat y (Delete x)) =
    withAuthAdmin y >> deleteThis1 "author" deleteAuthor x
{-
-}
--executeAuthor _ = undefined

executeTags (WhoWhat y (Read x)) = getThis1 getTags x
executeTags (WhoWhat y (Create x)) =
    withAuthAdmin y >> createThis1 "tag" createTag x
executeTags (WhoWhat y (Update x)) = 
    withAuthAdmin y >> editThis1 "tag" editTag x
executeTags (WhoWhat y (Delete x)) =
    withAuthAdmin y >> deleteThis1 "tag" deleteTag x


executeCategory (WhoWhat y (Read x)) = getThis1 getCategories x
executeCategory (WhoWhat y (Create x)) =
    withAuthAdmin y >> createThis1 "category" createCategory x
executeCategory (WhoWhat y (Update x)) =
    withAuthAdmin y >> editThis1 "category" editCategory x
executeCategory (WhoWhat y (Delete x)) =
    withAuthAdmin y >> deleteThis1 "category" deleteCategory x

executeUsers (WhoWhat y (Create x)) =
    createThis1 "user" createUser x
executeUsers (WhoWhat y (Delete x)) =
    withAuthAdmin y >> deleteThis1 "user" deleteUser x
executeUsers (WhoWhat y (Read GetProfile)) =
    withAuth y >>= getUser

executeComments (WhoWhat y (Read x)) =
    getThis1 getComments x
executeComments (WhoWhat y (Create x)) =
    withAuth y >>= maybeUserToUser >>= \u -> createThis1 "comment" createComment $ WithUser u x
executeComments (WhoWhat y (Delete x)) =
    withAuth y >>= maybeUserToUser >>= \u -> deleteThis1 "comment" deleteComment $ WithUser u x

executeDraft :: (MonadNews m) => WhoWhat ActionDrafts -> m Response
{-
-}
executeDraft (WhoWhat y (Create x)) =
    withAuthor y >>=
        \a -> createDraft $ WithAuthor (Ty._a_authorId a) x

executeDraft (WhoWhat y (Read (Paginated p s x))) =
    withAuthor y >>=
       -- \a -> getThis1 (getThisPaginated' draftDummy) $ Paginated p s (WithAuthor (Ty._a_authorId a) x)
        \a -> getThis1 getDrafts $ Paginated p s (WithAuthor (Ty._a_authorId a) x)
executeDraft (WhoWhat y (Delete x)) =
    withAuthor y >>=
       -- \a -> deleteThis dummyDDraft $ WithAuthor (Ty._a_authorId a) x
        \a -> deleteThis1 "draft" deleteDraft $ WithAuthor (Ty._a_authorId a) x
executeDraft (WhoWhat y (Update x)) =
    withAuthor y >>=
        \a -> editDraft $ WithAuthor (Ty._a_authorId a) x
{-
-}

executePublish (WhoWhat y x) =
    withAuthor y >>=
        \a -> publish $ WithAuthor (Ty._a_authorId a) x
{-
-}
--handleError :: (MonadLog m, CMC.MonadThrow m) => (WhoWhat ActionErrorPerms) -> m Response
handleError :: (MonadNews m) => (WhoWhat ActionErrorPerms) -> m Response
handleError (WhoWhat y (ActionErrorPerms admin@(False) (ERequiredFieldMissing x))) =
    handleFieldMissing x
handleError (WhoWhat y (ActionErrorPerms admin@(False) (EInvalidFieldValue x))) =
    handleInvalidValue x
handleError (WhoWhat y (ActionErrorPerms admin@(False) EInvalidEndpoint)) = do
    logError $ "Invalid endpoint"
    return $ notFound "Invalid endpoint"
handleError (WhoWhat y (ActionErrorPerms admin@(True) x)) =
    (withAuthAdmin y >> handleError (WhoWhat y (ActionErrorPerms False x)))
        `CMC.catch` f
  where
    f :: (MonadLog m) => SomeException -> m Response
    f e = handleForbidden

handleForbidden :: (MonadLog m) => m Response
handleForbidden = logError forbidden >> return (notFound "Invalid endpoint")

handleFieldMissing x = do
    let str =  "Required field missing (" <> x <> ")"
    logError $ E.decodeUtf8 str
    return $ bad $ E.decodeUtf8 str
handleInvalidValue x = do
    let str = "Invalid value of the field " <> x
    return $ bad $ E.decodeUtf8 str


