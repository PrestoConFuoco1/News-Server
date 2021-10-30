module Execute
    ( executeAction
    , handleError
    ) where

import Action.Common (ActionError(..), ActionErrorPerms(..))
import Action.RequestToAction (Action(..))
import qualified App.Database as D
import qualified Control.Monad.Catch as CMC
    ( MonadCatch
    , MonadCatch
    , SomeException
    , catch
    )
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text.Encoding as E (decodeUtf8)
import Data.Void (absurd)
import Execute.Database (createThis, deleteThis, editThis, getThis)
import Execute.Draft (createDraft, editDraft, publish)
import qualified Execute.Utils as U
import qualified Result as R
import qualified Types as Y

executeAction ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat Action
    -> m Y.APIResult
executeAction h (Y.WhoWhat token (AAuthors x)) =
    executeAuthor h (Y.WhoWhat token x)
executeAction h (Y.WhoWhat token (ACategory x)) =
    executeCategory h (Y.WhoWhat token x)
executeAction h (Y.WhoWhat token (APosts x)) =
    executePosts h (Y.WhoWhat token x)
executeAction h (Y.WhoWhat token (ATags x)) =
    executeTags h (Y.WhoWhat token x)
executeAction h (Y.WhoWhat token (AUsers x)) =
    executeUsers h (Y.WhoWhat token x)
executeAction h (Y.WhoWhat _ (AAuth x)) = U.authenticate h x
executeAction h (Y.WhoWhat token (AComments x)) =
    executeComments h (Y.WhoWhat token x)
executeAction h (Y.WhoWhat token (ADrafts x)) =
    executeDraft h (Y.WhoWhat token x)
executeAction h (Y.WhoWhat token (APublish x)) =
    executePublish h (Y.WhoWhat token x)

executePosts ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat Y.ActionPosts1
    -> m Y.APIResult
executePosts h (Y.WhoWhat _ (Y.GC x)) =
    getThis (D.getComments h (D.log h)) x
executePosts h (Y.WhoWhat _ (Y.AP (Y.Read x))) = do
    getThis (D.getPosts h (D.log h)) x
executePosts _ (Y.WhoWhat _ (Y.AP (Y.Create x))) = pure $ absurd x
executePosts _ (Y.WhoWhat _ (Y.AP (Y.Update x))) = pure $ absurd x
executePosts _ (Y.WhoWhat _ (Y.AP (Y.Delete x))) = pure $ absurd x

executeAuthor ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat Y.ActionAuthors
    -> m Y.APIResult
executeAuthor h (Y.WhoWhat token (Y.Read x)) =
    U.withAuthAdmin h token >> getThis (D.getAuthors h (D.log h)) x
executeAuthor h (Y.WhoWhat token (Y.Create x)) =
    U.withAuthAdmin h token >>
    createThis Y.EAuthor (D.createAuthor h (D.log h)) x
executeAuthor h (Y.WhoWhat token (Y.Update x)) =
    U.withAuthAdmin h token >>
    editThis Y.EAuthor (D.editAuthor h (D.log h)) x
executeAuthor h (Y.WhoWhat token (Y.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis Y.EAuthor (D.deleteAuthor h (D.log h)) x

{-
-}
executeTags ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat Y.ActionTags
    -> m Y.APIResult
executeTags h (Y.WhoWhat _ (Y.Read x)) =
    getThis (D.getTags h (D.log h)) x
executeTags h (Y.WhoWhat token (Y.Create x)) =
    U.withAuthAdmin h token >>
    createThis Y.ETag (D.createTag h (D.log h)) x
executeTags h (Y.WhoWhat token (Y.Update x)) =
    U.withAuthAdmin h token >>
    editThis Y.ETag (D.editTag h (D.log h)) x
executeTags h (Y.WhoWhat token (Y.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis Y.ETag (D.deleteTag h (D.log h)) x

executeCategory ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat Y.ActionCategory
    -> m Y.APIResult
executeCategory h (Y.WhoWhat _ (Y.Read x)) =
    getThis (D.getCategories h (D.log h)) x
executeCategory h (Y.WhoWhat token (Y.Create x)) =
    U.withAuthAdmin h token >>
    createThis Y.ECategory (D.createCategory h (D.log h)) x
executeCategory h (Y.WhoWhat token (Y.Update x)) =
    U.withAuthAdmin h token >>
    editThis Y.ECategory (D.editCategory h (D.log h)) x
executeCategory h (Y.WhoWhat token (Y.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis Y.ECategory (D.deleteCategory h (D.log h)) x

executeUsers ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat Y.ActionUsers
    -> m Y.APIResult
executeUsers h (Y.WhoWhat _ (Y.Create x)) =
    createThis Y.EUser (D.createUser h (D.log h)) x
executeUsers h (Y.WhoWhat token (Y.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis Y.EUser (D.deleteUser h (D.log h)) x
executeUsers h (Y.WhoWhat token (Y.Read Y.GetProfile)) =
    U.withAuth h token >>= U.getUser h
executeUsers _ (Y.WhoWhat _ (Y.Update x)) = pure $ absurd x

executeComments ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat Y.ActionComments
    -> m Y.APIResult
executeComments h (Y.WhoWhat _ (Y.Read x)) =
    getThis (D.getComments h (D.log h)) x
executeComments h (Y.WhoWhat token (Y.Create x)) =
    U.withAuth h token >>= U.maybeUserToUser h >>= \u ->
        createThis Y.EComment (D.createComment h (D.log h)) $
        Y.WithUser u x
executeComments h (Y.WhoWhat token (Y.Delete x)) =
    U.withAuth h token >>= U.maybeUserToUser h >>= \u ->
        deleteThis Y.EComment (D.deleteComment h (D.log h)) $
        Y.WithUser u x
executeComments _ (Y.WhoWhat _ (Y.Update x)) = pure $ absurd x

executeDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> Y.WhoWhat Y.ActionDrafts
    -> m Y.APIResult
executeDraft h (Y.WhoWhat token (Y.Create x)) =
    U.withAuthor h token >>= \author ->
        createDraft h $ Y.WithAuthor (Y._a_authorId author) x
executeDraft h (Y.WhoWhat token (Y.Read (Y.Paginated p s x))) =
    U.withAuthor h token >>= \author ->
        getThis (D.getDrafts h (D.log h)) $
        Y.Paginated p s (Y.WithAuthor (Y._a_authorId author) x)
executeDraft h (Y.WhoWhat token (Y.Delete x)) =
    U.withAuthor h token >>= \author ->
        deleteThis Y.EDraft (D.deleteDraft h (D.log h)) $
        Y.WithAuthor (Y._a_authorId author) x
executeDraft h (Y.WhoWhat token (Y.Update x)) =
    U.withAuthor h token >>= \author ->
        editDraft h $ Y.WithAuthor (Y._a_authorId author) x

executePublish ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> Y.WhoWhat Y.Publish
    -> m Y.APIResult
executePublish h (Y.WhoWhat token x) =
    U.withAuthor h token >>= \author ->
        publish h $ Y.WithAuthor (Y._a_authorId author) x

handleError ::
       CMC.MonadCatch m
    => D.Handle m
    -> Y.WhoWhat ActionErrorPerms
    -> m R.Response
handleError h (Y.WhoWhat _ (ActionErrorPerms False (ERequiredFieldMissing x))) =
    handleFieldMissing h x
handleError h (Y.WhoWhat _ (ActionErrorPerms False (EInvalidFieldValue x))) =
    handleInvalidValue h x
handleError h (Y.WhoWhat _ (ActionErrorPerms False EInvalidEndpoint)) = do
    D.logError h "Invalid endpoint"
    pure $ R.notFound "Invalid endpoint"
handleError h (Y.WhoWhat token (ActionErrorPerms True x)) =
    (U.withAuthAdmin h token >>
     handleError h (Y.WhoWhat token (ActionErrorPerms False x))) `CMC.catch`
    f h
  where
    f :: (CMC.MonadCatch m)
      => D.Handle m
      -> CMC.SomeException
      -> m R.Response
    f h' _ = handleForbidden h'

handleForbidden :: (CMC.MonadCatch m) => D.Handle m -> m R.Response
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
