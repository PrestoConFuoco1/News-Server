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
executeAction h (T.WhoWhat _ (AAuth x)) =
    U.authenticate (D.authHandler h) (D.log h) x
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
    getThis (D.getComments (D.commentsHandler h) (D.log h)) x
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
executeAuthor h (T.WhoWhat token (T.Read x)) = do
    let logger = D.log h
        authH = D.authHandler h
        authorsH = D.authorsHandler h
    U.withAuthAdmin authH logger token
    getThis (D.getAuthors authorsH logger) x
executeAuthor h (T.WhoWhat token (T.Create x)) = do
    let logger = D.log h
        authH = D.authHandler h
        authorsH = D.authorsHandler h
    U.withAuthAdmin authH logger token
    createThis T.EAuthor (D.createAuthor authorsH logger) x
executeAuthor h (T.WhoWhat token (T.Update x)) = do
    let logger = D.log h
        authH = D.authHandler h
        authorsH = D.authorsHandler h
    U.withAuthAdmin authH logger token
    editThis T.EAuthor (D.editAuthor authorsH logger) x
executeAuthor h (T.WhoWhat token (T.Delete x)) = do
    let logger = D.log h
        authH = D.authHandler h
        authorsH = D.authorsHandler h
    U.withAuthAdmin authH logger token
    deleteThis T.EAuthor (D.deleteAuthor authorsH logger) x

{-
-}
executeTags ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionTags
    -> m T.APIResult
executeTags h (T.WhoWhat _ (T.Read x)) =
    getThis (D.getTags (D.tagsHandler h) (D.log h)) x
executeTags h (T.WhoWhat token (T.Create x)) = do
    let logger = D.log h
        authH = D.authHandler h
        tagsH = D.tagsHandler h
    U.withAuthAdmin authH logger token
    createThis T.ETag (D.createTag tagsH logger) x
executeTags h (T.WhoWhat token (T.Update x)) = do
    let logger = D.log h
        authH = D.authHandler h
        tagsH = D.tagsHandler h
    U.withAuthAdmin authH logger token
    editThis T.ETag (D.editTag tagsH logger) x
executeTags h (T.WhoWhat token (T.Delete x)) = do
    let logger = D.log h
        authH = D.authHandler h
        tagsH = D.tagsHandler h
    U.withAuthAdmin authH logger token
    deleteThis T.ETag (D.deleteTag tagsH logger) x

executeCategory ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionCategory
    -> m T.APIResult
executeCategory h (T.WhoWhat _ (T.Read x)) =
    getThis (D.getCategories (D.catsHandler h) (D.log h)) x
executeCategory h (T.WhoWhat token (T.Create x)) = do
    let logger = D.log h
        authH = D.authHandler h
        catsH = D.catsHandler h
    U.withAuthAdmin authH logger token
    createThis T.ECategory (D.createCategory catsH logger) x
executeCategory h (T.WhoWhat token (T.Update x)) = do
    let logger = D.log h
        authH = D.authHandler h
        catsH = D.catsHandler h
    U.withAuthAdmin authH logger token
    mModifError <- U.checkCategoryUpdate catsH logger x
    let action = editThis T.ECategory (D.editCategory catsH logger) x
    maybe action (pure . T.RFailed T.ECategory) mModifError
executeCategory h (T.WhoWhat token (T.Delete x)) = do
    let logger = D.log h
        authH = D.authHandler h
        catsH = D.catsHandler h
    U.withAuthAdmin authH logger token
    deleteThis T.ECategory (D.deleteCategory catsH logger) x

executeUsers ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionUsers
    -> m T.APIResult
executeUsers h (T.WhoWhat _ (T.Create x)) = do
    let logger = D.log h
        authH = D.authHandler h
    createThis T.EUser (D.createUser authH logger) x
executeUsers h (T.WhoWhat token (T.Delete x)) = do
    let logger = D.log h
        authH = D.authHandler h
    U.withAuthAdmin authH logger token
    deleteThis T.EUser (D.deleteUser authH logger) x
executeUsers h (T.WhoWhat token (T.Read T.GetProfile)) = do
    let logger = D.log h
        authH = D.authHandler h
    mUser <- U.withAuth authH logger token
    U.getUser authH logger mUser
executeUsers _ (T.WhoWhat _ (T.Update x)) = pure $ absurd x

executeComments ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionComments
    -> m T.APIResult
executeComments h (T.WhoWhat _ (T.Read x)) =
    getThis (D.getComments (D.commentsHandler h) (D.log h)) x
executeComments h (T.WhoWhat token (T.Create x)) = do
    let logger = D.log h
        authH = D.authHandler h
        commentsH = D.commentsHandler h
    mUser <- U.withAuth authH logger token
    user <- U.maybeUserToUser authH logger mUser
    createThis T.EComment (D.createComment commentsH logger) $
        T.WithUser user x
executeComments h (T.WhoWhat token (T.Delete x)) = do
    let logger = D.log h
        authH = D.authHandler h
        commentsH = D.commentsHandler h
    mUser <- U.withAuth authH logger token
    user <- U.maybeUserToUser authH logger mUser
    deleteThis T.EComment (D.deleteComment commentsH logger) $
        T.WithUser user x
executeComments _ (T.WhoWhat _ (T.Update x)) = pure $ absurd x

executeDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WhoWhat T.ActionDrafts
    -> m T.APIResult
executeDraft h (T.WhoWhat token (T.Create x)) = do
    let logger = D.log h
        draftsH = D.draftsHandler h
        authH = D.authHandler h
    author <- U.withAuthor authH logger token
    createDraft draftsH logger $ T.WithAuthor (T._a_authorId author) x
executeDraft h (T.WhoWhat token (T.Read (T.Paginated p s x))) = do
    let logger = D.log h
        draftsH = D.draftsHandler h
        authH = D.authHandler h
    author <- U.withAuthor (D.authHandler h) (D.log h) token
    getThis (D.getDrafts draftsH logger) $
        T.Paginated p s (T.WithAuthor (T._a_authorId author) x)
executeDraft h (T.WhoWhat token (T.Delete x)) = do
    let logger = D.log h
        draftsH = D.draftsHandler h
        authH = D.authHandler h
    author <- U.withAuthor (D.authHandler h) (D.log h) token
    deleteThis T.EDraft (D.deleteDraft draftsH logger) $
        T.WithAuthor (T._a_authorId author) x
executeDraft h (T.WhoWhat token (T.Update x)) = do
    let logger = D.log h
        draftsH = D.draftsHandler h
        authH = D.authHandler h
    author <- U.withAuthor authH logger token
    editDraft draftsH logger $ T.WithAuthor (T._a_authorId author) x

executePublish ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WhoWhat T.Publish
    -> m T.APIResult
executePublish h (T.WhoWhat token x) = do
    let logger = D.log h
        draftsH = D.draftsHandler h
        authH = D.authHandler h
    author <- U.withAuthor authH logger token
    publish draftsH logger $ T.WithAuthor (T._a_authorId author) x

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
    (U.withAuthAdmin (D.authHandler h) (D.log h) token >>
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
