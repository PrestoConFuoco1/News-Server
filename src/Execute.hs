module Execute (executeAction, handleError) where

import Action.Common (ActionErrorPerms(..), ActionError(..))
import Action.RequestToAction (Action(..))
import qualified App.Database as D
import qualified Control.Monad.Catch as CMC
   ( MonadCatch
   , MonadCatch
   , catch
   , SomeException
   )
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text.Encoding as E (decodeUtf8)
import Data.Void (absurd)
import Execute.Database (getThis, deleteThis, createThis, editThis)
import Execute.Draft (publish, editDraft, createDraft)
import qualified Execute.Utils as U
import qualified Result as R
import Types

executeAction ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat Action
   -> m APIResult
executeAction h (WhoWhat token (AAuthors x)) =
   executeAuthor h (WhoWhat token x)
executeAction h (WhoWhat token (ACategory x)) =
   executeCategory h (WhoWhat token x)
executeAction h (WhoWhat token (APosts x)) =
   executePosts h (WhoWhat token x)
executeAction h (WhoWhat token (ATags x)) =
   executeTags h (WhoWhat token x)
executeAction h (WhoWhat token (AUsers x)) =
   executeUsers h (WhoWhat token x)
executeAction h (WhoWhat _ (AAuth x)) = U.authenticate h x
executeAction h (WhoWhat token (AComments x)) =
   executeComments h (WhoWhat token x)
executeAction h (WhoWhat token (ADrafts x)) =
   executeDraft h (WhoWhat token x)
executeAction h (WhoWhat token (APublish x)) =
   executePublish h (WhoWhat token x)

executePosts ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionPosts1
   -> m APIResult
executePosts h (WhoWhat _ (GC x)) =
   getThis (D.getComments h (D.log h)) x
executePosts h (WhoWhat _ (AP (Read x))) = do
   getThis (D.getPosts h (D.log h)) x
executePosts _ (WhoWhat _ (AP (Create x))) =
   pure $ absurd x
executePosts _ (WhoWhat _ (AP (Update x))) =
   pure $ absurd x
executePosts _ (WhoWhat _ (AP (Delete x))) =
   pure $ absurd x

executeAuthor ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionAuthors
   -> m APIResult
executeAuthor h (WhoWhat token (Read x)) =
   U.withAuthAdmin h token >>
   getThis (D.getAuthors h (D.log h)) x
executeAuthor h (WhoWhat token (Create x)) =
   U.withAuthAdmin h token >>
   createThis EAuthor (D.createAuthor h (D.log h)) x
executeAuthor h (WhoWhat token (Update x)) =
   U.withAuthAdmin h token >>
   editThis EAuthor (D.editAuthor h (D.log h)) x
executeAuthor h (WhoWhat token (Delete x)) =
   U.withAuthAdmin h token >>
   deleteThis EAuthor (D.deleteAuthor h (D.log h)) x

{-
-}
executeTags ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionTags
   -> m APIResult
executeTags h (WhoWhat _ (Read x)) =
   getThis (D.getTags h (D.log h)) x
executeTags h (WhoWhat token (Create x)) =
   U.withAuthAdmin h token >>
   createThis ETag (D.createTag h (D.log h)) x
executeTags h (WhoWhat token (Update x)) =
   U.withAuthAdmin h token >>
   editThis ETag (D.editTag h (D.log h)) x
executeTags h (WhoWhat token (Delete x)) =
   U.withAuthAdmin h token >>
   deleteThis ETag (D.deleteTag h (D.log h)) x

executeCategory ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionCategory
   -> m APIResult
executeCategory h (WhoWhat _ (Read x)) =
   getThis (D.getCategories h (D.log h)) x
executeCategory h (WhoWhat token (Create x)) =
   U.withAuthAdmin h token >>
   createThis ECategory (D.createCategory h (D.log h)) x
executeCategory h (WhoWhat token (Update x)) =
   U.withAuthAdmin h token >>
   editThis ECategory (D.editCategory h (D.log h)) x
executeCategory h (WhoWhat token (Delete x)) =
   U.withAuthAdmin h token >>
   deleteThis ECategory (D.deleteCategory h (D.log h)) x

executeUsers ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionUsers
   -> m APIResult
executeUsers h (WhoWhat _ (Create x)) =
   createThis EUser (D.createUser h (D.log h)) x
executeUsers h (WhoWhat token (Delete x)) =
   U.withAuthAdmin h token >>
   deleteThis EUser (D.deleteUser h (D.log h)) x
executeUsers h (WhoWhat token (Read GetProfile)) =
   U.withAuth h token >>= U.getUser h
executeUsers _ (WhoWhat _ (Update x)) = pure $ absurd x

executeComments ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionComments
   -> m APIResult
executeComments h (WhoWhat _ (Read x)) =
   getThis (D.getComments h (D.log h)) x
executeComments h (WhoWhat token (Create x)) =
   U.withAuth h token >>= U.maybeUserToUser h >>= \u ->
      createThis EComment (D.createComment h (D.log h)) $
      WithUser u x
executeComments h (WhoWhat token (Delete x)) =
   U.withAuth h token >>= U.maybeUserToUser h >>= \u ->
      deleteThis EComment (D.deleteComment h (D.log h)) $
      WithUser u x
executeComments _ (WhoWhat _ (Update x)) = pure $ absurd x

executeDraft ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> WhoWhat ActionDrafts
   -> m APIResult
executeDraft h (WhoWhat token (Create x)) =
   U.withAuthor h token >>= \author ->
      createDraft h $ WithAuthor (_a_authorId author) x
executeDraft h (WhoWhat token (Read (Paginated p s x))) =
   U.withAuthor h token >>= \author ->
      getThis (D.getDrafts h (D.log h)) $
      Paginated p s (WithAuthor (_a_authorId author) x)
executeDraft h (WhoWhat token (Delete x)) =
   U.withAuthor h token >>= \author ->
      deleteThis EDraft (D.deleteDraft h (D.log h)) $
      WithAuthor (_a_authorId author) x
executeDraft h (WhoWhat token (Update x)) =
   U.withAuthor h token >>= \author ->
      editDraft h $ WithAuthor (_a_authorId author) x

executePublish ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> WhoWhat Publish
   -> m APIResult
executePublish h (WhoWhat token x) =
   U.withAuthor h token >>= \author ->
      publish h $ WithAuthor (_a_authorId author) x

handleError ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionErrorPerms
   -> m R.Response
handleError h (WhoWhat _ (ActionErrorPerms False (ERequiredFieldMissing x))) =
   handleFieldMissing h x
handleError h (WhoWhat _ (ActionErrorPerms False (EInvalidFieldValue x))) =
   handleInvalidValue h x
handleError h (WhoWhat _ (ActionErrorPerms False EInvalidEndpoint)) = do
   D.logError h "Invalid endpoint"
   pure $ R.notFound "Invalid endpoint"
handleError h (WhoWhat token (ActionErrorPerms True x)) =
   (U.withAuthAdmin h token >>
    handleError h (WhoWhat token (ActionErrorPerms False x))) `CMC.catch`
   f h
  where
    f :: (CMC.MonadCatch m)
      => D.Handle m
      -> CMC.SomeException
      -> m R.Response
    f h' _ = handleForbidden h'

handleForbidden ::
      (CMC.MonadCatch m) => D.Handle m -> m R.Response
handleForbidden h = do
   D.logError h R.forbidden
   pure (R.notFound "Invalid endpoint")

handleFieldMissing ::
      (Monad m) => D.Handle m -> BS.ByteString -> m R.Response
handleFieldMissing h x = do
   let str = "Required field missing (" <> x <> ")"
   D.logError h $ E.decodeUtf8 str
   pure $ R.bad $ E.decodeUtf8 str

handleInvalidValue ::
      (Monad m) => D.Handle m -> BS.ByteString -> m R.Response
handleInvalidValue _ x = do
   let str = "Invalid value of the field " <> x
   pure $ R.bad $ E.decodeUtf8 str
