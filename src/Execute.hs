{-# LANGUAGE RecordWildCards #-}

module Execute where



import qualified Data.Text as T (pack)
import Execute.Utils

import Action.RequestToAction
import Action.Common
import Database.Read
import Database.Create
import Database.Delete
import Database.Update
import Control.Exception (SomeException)

import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Types as Ty
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch, catch, MonadCatch)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Result
import Execute.Types
import Execute.Draft

import Types

import Exceptions as Ex
import Execute.Database

import Database.SqlQueryTypes
import qualified App.Database as D

executeAction :: CMC.MonadCatch m => D.Handle m -> WhoWhat Action -> m APIResult
executeAction h (WhoWhat y (AAuthors x)) = executeAuthor h (WhoWhat y x)
executeAction h (WhoWhat y (ACategory x)) = executeCategory h (WhoWhat y x)
executeAction h (WhoWhat y (APosts x)) = executePosts h (WhoWhat y x)
executeAction h (WhoWhat y (ATags x)) = executeTags h (WhoWhat y x)
executeAction h (WhoWhat y (AUsers x)) = executeUsers h (WhoWhat y x)
executeAction h (WhoWhat y (AAuth x)) = authenticate h x
executeAction h (WhoWhat y (AComments x)) = executeComments h (WhoWhat y x)
executeAction h (WhoWhat y (ADrafts x)) = executeDraft h (WhoWhat y x)
executeAction h (WhoWhat y (APublish x)) = executePublish h (WhoWhat y x)

executePosts h (WhoWhat y (GC x)) = getThis1 (D.getComments h) x

executePosts h (WhoWhat y (AP (Read x))) = do
    getThis1 (D.getPosts h) x


executeAuthor :: CMC.MonadCatch m => D.Handle m -> WhoWhat ActionAuthors -> m APIResult
executeAuthor h (WhoWhat y (Read x)) =
    withAuthAdmin h y >> getThis1 (D.getAuthors h) x
executeAuthor h (WhoWhat y (Create x)) =
    withAuthAdmin h y >> createThis1 EAuthor (D.createAuthor h) x
executeAuthor h (WhoWhat y (Update x)) =
    withAuthAdmin h y >> editThis1 EAuthor (D.editAuthor h) x
executeAuthor h (WhoWhat y (Delete x)) =
    withAuthAdmin h y >> deleteThis1 EAuthor (D.deleteAuthor h) x
{-
-}

executeTags h (WhoWhat y (Read x)) = getThis1 (D.getTags h) x
executeTags h (WhoWhat y (Create x)) =
    withAuthAdmin h y >> createThis1 ETag (D.createTag h) x
executeTags h (WhoWhat y (Update x)) = 
    withAuthAdmin h y >> editThis1 ETag (D.editTag h) x
executeTags h (WhoWhat y (Delete x)) =
    withAuthAdmin h y >> deleteThis1 ETag (D.deleteTag h) x


executeCategory h (WhoWhat y (Read x)) = getThis1 (D.getCategories h) x
executeCategory h (WhoWhat y (Create x)) =
    withAuthAdmin h y >> createThis1 ECategory (D.createCategory h) x
executeCategory h (WhoWhat y (Update x)) =
    withAuthAdmin h y >> editThis1 ECategory (D.editCategory h) x
executeCategory h (WhoWhat y (Delete x)) =
    withAuthAdmin h y >> deleteThis1 ECategory (D.deleteCategory h) x

executeUsers h (WhoWhat y (Create x)) =
    createThis1 EUser (D.createUser h) x
executeUsers h (WhoWhat y (Delete x)) =
    withAuthAdmin h y >> deleteThis1 EUser (D.deleteUser h) x
executeUsers h (WhoWhat y (Read GetProfile)) =
    withAuth h y >>= getUser h

executeComments h (WhoWhat y (Read x)) =
    getThis1 (D.getComments h) x
executeComments h (WhoWhat y (Create x)) =
    withAuth h y >>= maybeUserToUser h >>= \u -> createThis1 EComment (D.createComment h) $ WithUser u x
executeComments h (WhoWhat y (Delete x)) =
    withAuth h y >>= maybeUserToUser h >>= \u -> deleteThis1 EComment (D.deleteComment h) $ WithUser u x

executeDraft :: (CMC.MonadCatch m) => D.Handle m -> WhoWhat ActionDrafts -> m APIResult
executeDraft h (WhoWhat y (Create x)) =
    withAuthor h y >>=
        \a -> createDraft h $ WithAuthor (Ty._a_authorId a) x

executeDraft h (WhoWhat y (Read (Paginated p s x))) =
    withAuthor h y >>=
        \a -> getThis1 (D.getDrafts h) $ Paginated p s (WithAuthor (Ty._a_authorId a) x)
executeDraft h (WhoWhat y (Delete x)) =
    withAuthor h y >>=
        \a -> deleteThis1 EDraft (D.deleteDraft h) $ WithAuthor (Ty._a_authorId a) x
executeDraft h (WhoWhat y (Update x)) =
    withAuthor h y >>=
        \a -> editDraft h $ WithAuthor (Ty._a_authorId a) x

executePublish h (WhoWhat y x) =
    withAuthor h y >>=
        \a -> publish h $ WithAuthor (Ty._a_authorId a) x

handleError :: CMC.MonadCatch m => D.Handle m -> (WhoWhat ActionErrorPerms) -> m Response
handleError h (WhoWhat y (ActionErrorPerms admin@(False) (ERequiredFieldMissing x))) =
    handleFieldMissing h x
handleError h (WhoWhat y (ActionErrorPerms admin@(False) (EInvalidFieldValue x))) =
    handleInvalidValue h x
handleError h (WhoWhat y (ActionErrorPerms admin@(False) EInvalidEndpoint)) = do
    D.logError h $ "Invalid endpoint"
    return $ bad "Invalid endpoint"
handleError h (WhoWhat y (ActionErrorPerms admin@(True) x)) =
    (withAuthAdmin h y >> handleError h (WhoWhat y (ActionErrorPerms False x)))
        `CMC.catch` f h
  where
    f :: (CMC.MonadCatch m) => D.Handle m -> SomeException -> m Response
    f h e = handleForbidden h

handleForbidden :: (CMC.MonadCatch m) => D.Handle m -> m Response
handleForbidden h =
    D.logError h forbidden >>
    return (bad "Invalid endpoint")

handleFieldMissing h x = do
    let str =  "Required field missing (" <> x <> ")"
    D.logError h $ E.decodeUtf8 str
    return $ bad $ E.decodeUtf8 str
handleInvalidValue h x = do
    let str = "Invalid value of the field " <> x
    return $ bad $ E.decodeUtf8 str


