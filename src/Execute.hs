{-# LANGUAGE RecordWildCards #-}

module Execute where



import qualified Data.Text as T (pack)


import Action.RequestToAction
import Action.Types (WhoWhat (..), Token)
import Action.Common
import Database.Read
import Database.Create
import Database.Delete
import Database.Update
import Control.Exception (SomeException)

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Types as Ty
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch, catch)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
--import ActWithOne (actWithOne, ActWithOne(..), AWOu(..), AWOd(..))
import Execute.Result
import Execute.Types
import Execute.Utils
import Action.Users
import Action.Comments
import Action.Draft
import Action.Authors
import Action.Posts
import Execute.Draft

import Exceptions as Ex

import Database.SqlQueryTypes
import Execute.Permissions
import Execute.Actions

executeAction :: MonadServer m => WhoWhat Action -> m Response
executeAction (WhoWhat y (AAuthors x)) = executeAuthor (WhoWhat y x)
executeAction (WhoWhat y (ACategory x)) = executeCategory (WhoWhat y x)
executeAction (WhoWhat y (APosts x)) = executePosts (WhoWhat y x)
executeAction (WhoWhat y (ATags x)) = executeTags (WhoWhat y x)
executeAction (WhoWhat y (AUsers x)) = executeUsers (WhoWhat y x)
executeAction (WhoWhat y (AAuth x)) = authenticate x
executeAction (WhoWhat y (AComments x)) = executeComments (WhoWhat y x)
executeAction (WhoWhat y (ADrafts x)) = executeDraft (WhoWhat y x)
executeAction (WhoWhat y (APublish x)) = executePublish (WhoWhat y x)

executePosts (WhoWhat y (GC x)) = getThis commentDummy x

executePosts (WhoWhat y (AP (Read x))) = do
    let where1 = postsWhereClause1 x
        (SqlQuery clause params) = whereToQuery where1
    logDebug $ T.pack $ show $ where1
    evaluated <- formatQuery clause params
    logDebug $ E.decodeUtf8 evaluated
    getThis postDummy x


executeAuthor (WhoWhat y (Read x)) =
    withAuthAdmin y >> getThis authorDummy x
executeAuthor (WhoWhat y (Create x)) =
    withAuthAdmin y >> createThis dummyCAuthor x
executeAuthor (WhoWhat y (Update x)) =
    withAuthAdmin y >> editThis dummyUAuthor x
executeAuthor (WhoWhat y (Delete x)) =
    withAuthAdmin y >> deleteThis dummyDAuthor x

executeTags (WhoWhat y (Read x)) = getThis tagDummy x
executeTags (WhoWhat y (Create x)) = withAuthAdmin y >> createThis dummyCTag x
executeTags (WhoWhat y (Update x)) =  withAuthAdmin y >> editThis dummyUTag x
executeTags (WhoWhat y (Delete x)) = withAuthAdmin y >> deleteThis dummyDTag x


executeCategory (WhoWhat y (Read x)) = getThis catDummy x
executeCategory (WhoWhat y (Create x)) = withAuthAdmin y >> createThis dummyCCat x
executeCategory (WhoWhat y (Update x)) =  withAuthAdmin y >> editThis dummyUCat x
executeCategory (WhoWhat y (Delete x)) = withAuthAdmin y >> deleteThis dummyDCat x

executeUsers (WhoWhat y (Create x)) = createThis dummyCUser x
executeUsers (WhoWhat y (Delete x)) = withAuthAdmin y >> deleteThis dummyDUser x
executeUsers (WhoWhat y (Read GetProfile)) = withAuth y >>= getUser

executeComments (WhoWhat y (Read x)) = getThis commentDummy x
--executeComments (WhoWhat y (Create x)) = withAuth y >>= createComment x
executeComments (WhoWhat y (Create x)) = withAuth y >>= maybeUserToUser >>= \u -> createThis dummyCComment $ WithUser u x
executeComments (WhoWhat y (Delete x)) = withAuth y >>= maybeUserToUser >>= \u -> deleteThis dummyDComment $ WithUser u x
{-
createComment :: (MonadServer m) => CreateComment -> Maybe Ty.User -> m Response
createComment cc Nothing = Ex.unauthorized
createComment cc (Just u) = createThis dummyCComment $ WithUser u cc
-}
executeDraft :: (MonadServer m) => WhoWhat ActionDrafts -> m Response
executeDraft (WhoWhat y (Create x)) =
    withAuthor y >>=
        \a -> createDraft $ WithAuthor (Ty._a_authorId a) x

executeDraft (WhoWhat y (Read x)) =
    withAuthor y >>=
        \a -> getThis draftDummy $ WithAuthor (Ty._a_authorId a) x
executeDraft (WhoWhat y (Delete x)) =
    withAuthor y >>=
        \a -> deleteThis dummyDDraft $ WithAuthor (Ty._a_authorId a) x
executeDraft (WhoWhat y (Update x)) =
    withAuthor y >>=
        \a -> editDraft $ WithAuthor (Ty._a_authorId a) x

executePublish (WhoWhat y x) =
    withAuthor y >>=
        \a -> publish $ WithAuthor (Ty._a_authorId a) x

handleError :: MonadServer m => (WhoWhat ActionErrorPerms) -> m Response
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
    f :: (MonadServer m) => SomeException -> m Response
    f e = handleForbidden

handleForbidden :: (MonadServer m) => m Response
handleForbidden = logError forbidden >> return (notFound "Invalid endpoint")

handleFieldMissing x = do
    let str =  "Required field missing (" <> x <> ")"
    logError $ E.decodeUtf8 str
    return $ bad $ E.decodeUtf8 str
handleInvalidValue x = do
    let str = "Invalid value of the field " <> x
    return $ bad $ E.decodeUtf8 str


