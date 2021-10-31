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
import qualified GenericPretty as GP
import qualified Result as R
import qualified Types as T
import qualified Utils as S

executeAction ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat Action
    -> m T.APIResult
executeAction h (T.WhoWhat token (AAuthors x)) =
    executeAuthor h (T.WhoWhat token x)
executeAction h (T.WhoWhat token (ACategory x)) =
    executeCategory h (T.WhoWhat token x)
executeAction h (T.WhoWhat token (APosts x)) =
    executePosts h (T.WhoWhat token x)
executeAction h (T.WhoWhat token (ATags x)) =
    executeTags h (T.WhoWhat token x)
executeAction h (T.WhoWhat token (AUsers x)) =
    executeUsers h (T.WhoWhat token x)
executeAction h (T.WhoWhat _ (AAuth x)) = U.authenticate h x
executeAction h (T.WhoWhat token (AComments x)) =
    executeComments h (T.WhoWhat token x)
executeAction h (T.WhoWhat token (ADrafts x)) =
    executeDraft h (T.WhoWhat token x)
executeAction h (T.WhoWhat token (APublish x)) =
    executePublish h (T.WhoWhat token x)

executePosts ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionPosts1
    -> m T.APIResult
executePosts h (T.WhoWhat _ (T.GC x)) =
    getThis (D.getComments h (D.log h)) x
executePosts h (T.WhoWhat _ (T.AP (T.Read x))) = do
    getThis (D.getPosts h (D.log h)) x
executePosts _ (T.WhoWhat _ (T.AP (T.Create x))) = pure $ absurd x
executePosts _ (T.WhoWhat _ (T.AP (T.Update x))) = pure $ absurd x
executePosts _ (T.WhoWhat _ (T.AP (T.Delete x))) = pure $ absurd x

executeAuthor ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionAuthors
    -> m T.APIResult
executeAuthor h (T.WhoWhat token (T.Read x)) =
    U.withAuthAdmin h token >> getThis (D.getAuthors h (D.log h)) x
executeAuthor h (T.WhoWhat token (T.Create x)) =
    U.withAuthAdmin h token >>
    createThis T.EAuthor (D.createAuthor h (D.log h)) x
executeAuthor h (T.WhoWhat token (T.Update x)) =
    U.withAuthAdmin h token >>
    editThis T.EAuthor (D.editAuthor h (D.log h)) x
executeAuthor h (T.WhoWhat token (T.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis T.EAuthor (D.deleteAuthor h (D.log h)) x

{-
-}
executeTags ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionTags
    -> m T.APIResult
executeTags h (T.WhoWhat _ (T.Read x)) =
    getThis (D.getTags h (D.log h)) x
executeTags h (T.WhoWhat token (T.Create x)) =
    U.withAuthAdmin h token >>
    createThis T.ETag (D.createTag h (D.log h)) x
executeTags h (T.WhoWhat token (T.Update x)) =
    U.withAuthAdmin h token >>
    editThis T.ETag (D.editTag h (D.log h)) x
executeTags h (T.WhoWhat token (T.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis T.ETag (D.deleteTag h (D.log h)) x

executeCategory ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionCategory
    -> m T.APIResult
executeCategory h (T.WhoWhat _ (T.Read x)) =
    getThis (D.getCategories h (D.log h)) x
executeCategory h (T.WhoWhat token (T.Create x)) =
    U.withAuthAdmin h token >>
    createThis T.ECategory (D.createCategory h (D.log h)) x
executeCategory h (T.WhoWhat token (T.Update x)) = do
    U.withAuthAdmin h token
    mModifError <- U.checkCategoryUpdate h x
    let action = editThis T.ECategory (D.editCategory h (D.log h)) x
    maybe action (pure . T.RFailed T.ECategory) mModifError
executeCategory h (T.WhoWhat token (T.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis T.ECategory (D.deleteCategory h (D.log h)) x

executeUsers ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionUsers
    -> m T.APIResult
executeUsers h (T.WhoWhat _ (T.Create x)) =
    createThis T.EUser (D.createUser h (D.log h)) x
executeUsers h (T.WhoWhat token (T.Delete x)) =
    U.withAuthAdmin h token >>
    deleteThis T.EUser (D.deleteUser h (D.log h)) x
executeUsers h (T.WhoWhat token (T.Read T.GetProfile)) =
    U.withAuth h token >>= U.getUser h
executeUsers _ (T.WhoWhat _ (T.Update x)) = pure $ absurd x

executeComments ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionComments
    -> m T.APIResult
executeComments h (T.WhoWhat _ (T.Read x)) =
    getThis (D.getComments h (D.log h)) x
executeComments h (T.WhoWhat token (T.Create x)) =
    U.withAuth h token >>= U.maybeUserToUser h >>= \u ->
        createThis T.EComment (D.createComment h (D.log h)) $
        T.WithUser u x
executeComments h (T.WhoWhat token (T.Delete x)) =
    U.withAuth h token >>= U.maybeUserToUser h >>= \u ->
        deleteThis T.EComment (D.deleteComment h (D.log h)) $
        T.WithUser u x
executeComments _ (T.WhoWhat _ (T.Update x)) = pure $ absurd x

executeDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WhoWhat T.ActionDrafts
    -> m T.APIResult
executeDraft h (T.WhoWhat token (T.Create x)) =
    U.withAuthor h token >>= \author ->
        createDraft h $ T.WithAuthor (T._a_authorId author) x
executeDraft h (T.WhoWhat token (T.Read (T.Paginated p s x))) =
    U.withAuthor h token >>= \author ->
        getThis (D.getDrafts h (D.log h)) $
        T.Paginated p s (T.WithAuthor (T._a_authorId author) x)
executeDraft h (T.WhoWhat token (T.Delete x)) =
    U.withAuthor h token >>= \author ->
        deleteThis T.EDraft (D.deleteDraft h (D.log h)) $
        T.WithAuthor (T._a_authorId author) x
executeDraft h (T.WhoWhat token (T.Update x)) =
    U.withAuthor h token >>= \author ->
        editDraft h $ T.WithAuthor (T._a_authorId author) x

executePublish ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WhoWhat T.Publish
    -> m T.APIResult
executePublish h (T.WhoWhat token x) =
    U.withAuthor h token >>= \author ->
        publish h $ T.WithAuthor (T._a_authorId author) x

handleError ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat ActionErrorPerms
    -> m R.Response
handleError h (T.WhoWhat _ (ActionErrorPerms False (ERequiredFieldMissing x))) =
    handleFieldMissing h x
handleError h (T.WhoWhat _ (ActionErrorPerms False (EInvalidFieldValue x))) =
    handleInvalidValue h x
handleError h (T.WhoWhat _ (ActionErrorPerms False EInvalidEndpoint)) = do
    D.logError h "Invalid endpoint"
    pure $ R.notFound "Invalid endpoint"
handleError h (T.WhoWhat token (ActionErrorPerms True x)) =
    (U.withAuthAdmin h token >>
     handleError h (T.WhoWhat token (ActionErrorPerms False x))) `CMC.catch`
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
