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

executeAuthor h (T.WhoWhat token crud) = do
    let logger = D.log h
        authH = D.authHandler h
        authorsH = D.authorsHandler h
    U.withAuthAdmin authH logger token
    T.foldCRUD
        crud
        (createThis T.EAuthor $ D.createAuthor authorsH logger)
        (getThis $ D.getAuthors authorsH logger)
        (editThis T.EAuthor $ D.editAuthor authorsH logger)
        (deleteThis T.EAuthor $ D.deleteAuthor authorsH logger)

executeTags ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionTags
    -> m T.APIResult
executeTags h (T.WhoWhat token crud) = do
    let logger = D.log h
        authH = D.authHandler h
        tagsH = D.tagsHandler h
        withAdmin = (U.withAuthAdmin authH logger token >>)
    T.foldCRUD
        crud
        (withAdmin . createThis T.ETag (D.createTag tagsH logger))
        (getThis $ D.getTags tagsH logger)
        (withAdmin . editThis T.ETag (D.editTag tagsH logger))
        (withAdmin . deleteThis T.ETag (D.deleteTag tagsH logger))

executeCategory ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionCategory
    -> m T.APIResult
executeCategory h (T.WhoWhat token crud) = do
    let logger = D.log h
        authH = D.authHandler h
        catsH = D.catsHandler h
        withAdmin = (U.withAuthAdmin authH logger token >>)
    T.foldCRUD
        crud
        (withAdmin .
         createThis T.ECategory (D.createCategory catsH logger))
        (getThis $ D.getCategories catsH logger)
        (\u -> do
             U.withAuthAdmin authH logger token
             mModifError <- U.checkCategoryUpdate catsH logger u
             let action =
                     editThis
                         T.ECategory
                         (D.editCategory catsH logger)
                         u
             maybe action (pure . T.RFailed T.ECategory) mModifError)
        (withAdmin .
         deleteThis T.ECategory (D.deleteCategory catsH logger))

executeUsers ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionUsers
    -> m T.APIResult
executeUsers h (T.WhoWhat token crud) = do
    let logger = D.log h
        authH = D.authHandler h
        withAdmin = (U.withAuthAdmin authH logger token >>)
    T.foldCRUD
        crud
        (createThis T.EUser $ D.createUser authH logger)
        (\_ -> do
             mUser <- U.withAuth authH logger token
             U.getUser authH logger mUser)
        (pure . absurd)
        (withAdmin . deleteThis T.EUser (D.deleteUser authH logger))

executeComments ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionComments
    -> m T.APIResult
executeComments h (T.WhoWhat token crud) = do
    let logger = D.log h
        authH = D.authHandler h
        commentsH = D.commentsHandler h
    T.foldCRUD
        crud
        (\c -> do
             mUser <- U.withAuth authH logger token
             user <- U.maybeUserToUser authH logger mUser
             createThis T.EComment (D.createComment commentsH logger) $
                 T.WithUser user c)
        (getThis $ D.getComments commentsH logger)
        (pure . absurd)
        (\d -> do
             mUser <- U.withAuth authH logger token
             user <- U.maybeUserToUser authH logger mUser
             deleteThis T.EComment (D.deleteComment commentsH logger) $
                 T.WithUser user d)

executeDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WhoWhat T.ActionDrafts
    -> m T.APIResult
executeDraft h (T.WhoWhat token crud) = do
    let logger = D.log h
        draftsH = D.draftsHandler h
        authH = D.authHandler h
    author <- U.withAuthor authH logger token
    T.foldCRUD
        crud
        (createDraft draftsH logger .
         T.WithAuthor (T._a_authorId author))
        (\r ->
             getThis (D.getDrafts draftsH logger) .
             T.Paginated (T._pag_page r) (T._pag_size r) .
             T.WithAuthor (T._a_authorId author) $
             T._pag_data r)
        (editDraft draftsH logger .
         T.WithAuthor (T._a_authorId author))
        (deleteThis T.EDraft (D.deleteDraft draftsH logger) .
         T.WithAuthor (T._a_authorId author))

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
    handleForbidden h

handleForbidden ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> CMC.SomeException
    -> m R.Response
handleForbidden h _ = do
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
