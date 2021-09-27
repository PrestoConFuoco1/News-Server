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

executePosts (WhoWhat y (GC x)) = getThis1 (getThisPaginated' commentDummy) x

executePosts (WhoWhat y (AP (Read x))) = do
--    let where1 = postsWhereClause1 x
--        (SqlQuery clause params) = whereToQuery where1
--    logDebug $ T.pack $ show $ where1
--    evaluated <- formatQuery clause params
--    logDebug $ E.decodeUtf8 evaluated
    getThis1 (getThisPaginated' postDummy) x


executeAuthor (WhoWhat y (Read x)) =
    --withAuthAdmin y >> getThisPaginated authorDummy x
    withAuthAdmin y >> getThis1 (getThisPaginated' authorDummy) x
executeAuthor (WhoWhat y (Create x)) =
    --withAuthAdmin y >> createThis dummyCAuthor x
    withAuthAdmin y >> createThis1 "author" (createThis' dummyCAuthor) x
executeAuthor (WhoWhat y (Update x)) =
    --withAuthAdmin y >> editThis dummyUAuthor x
    withAuthAdmin y >> editThis1 "author" (editThis' dummyUAuthor) x
executeAuthor (WhoWhat y (Delete x)) =
    --withAuthAdmin y >> deleteThis dummyDAuthor x
    withAuthAdmin y >> deleteThis1 "author" (deleteThis' dummyDAuthor) x

executeTags (WhoWhat y (Read x)) = getThis1 (getThisPaginated' tagDummy) x
executeTags (WhoWhat y (Create x)) =
    --withAuthAdmin y >> createThis dummyCTag x
    withAuthAdmin y >> createThis1 "tag" (createThis' dummyCTag) x
executeTags (WhoWhat y (Update x)) = 
    --withAuthAdmin y >> editThis dummyUTag x
    withAuthAdmin y >> editThis1 "tag" (editThis' dummyUTag) x
executeTags (WhoWhat y (Delete x)) =
    withAuthAdmin y >> deleteThis1 "tag" (deleteThis' dummyDTag) x


executeCategory (WhoWhat y (Read x)) = getThis1 (getThisPaginated' catDummy) x
executeCategory (WhoWhat y (Create x)) =
    --withAuthAdmin y >> createThis dummyCCat x
    withAuthAdmin y >> createThis1 "category" (createThis' dummyCCat) x
executeCategory (WhoWhat y (Update x)) =
    --withAuthAdmin y >> editThis dummyUCat x
    withAuthAdmin y >> editThis1 "category" (editThis' dummyUCat) x
executeCategory (WhoWhat y (Delete x)) =
    --withAuthAdmin y >> deleteThis dummyDCat x
    withAuthAdmin y >> deleteThis1 "category" (deleteThis' dummyDCat) x

executeUsers (WhoWhat y (Create x)) =
    --createThis dummyCUser x
    createThis1 "user" (createThis' dummyCUser) x
executeUsers (WhoWhat y (Delete x)) =
    --withAuthAdmin y >> deleteThis dummyDUser x
    withAuthAdmin y >> deleteThis1 "user" (deleteThis' dummyDUser) x
executeUsers (WhoWhat y (Read GetProfile)) =
    withAuth y >>= getUser

executeComments (WhoWhat y (Read x)) =
    getThis1 (getThisPaginated' commentDummy) x
executeComments (WhoWhat y (Create x)) =
    --withAuth y >>= maybeUserToUser >>= \u -> createThis dummyCComment $ WithUser u x
    withAuth y >>= maybeUserToUser >>= \u -> createThis1 "comment" (createThis' dummyCComment) $ WithUser u x
executeComments (WhoWhat y (Delete x)) =
    --withAuth y >>= maybeUserToUser >>= \u -> deleteThis dummyDComment $ WithUser u x
    withAuth y >>= maybeUserToUser >>= \u -> deleteThis1 "comment" (deleteThis' dummyDComment) $ WithUser u x

executeDraft :: (MonadServer m) => WhoWhat ActionDrafts -> m Response
executeDraft (WhoWhat y (Create x)) =
    withAuthor y >>=
        \a -> createDraft $ WithAuthor (Ty._a_authorId a) x

executeDraft (WhoWhat y (Read (Paginated p s x))) =
    withAuthor y >>=
        \a -> getThis1 (getThisPaginated' draftDummy) $ Paginated p s (WithAuthor (Ty._a_authorId a) x)
executeDraft (WhoWhat y (Delete x)) =
    withAuthor y >>=
        \a -> deleteThis dummyDDraft $ WithAuthor (Ty._a_authorId a) x
executeDraft (WhoWhat y (Update x)) =
    withAuthor y >>=
        \a -> editDraft $ WithAuthor (Ty._a_authorId a) x

executePublish (WhoWhat y x) =
    withAuthor y >>=
        \a -> publish $ WithAuthor (Ty._a_authorId a) x

--handleError :: (MonadLog m, CMC.MonadThrow m) => (WhoWhat ActionErrorPerms) -> m Response
handleError :: (MonadServer m) => (WhoWhat ActionErrorPerms) -> m Response
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


