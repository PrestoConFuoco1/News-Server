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
    , handle
    )
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text.Encoding as E (decodeUtf8)
import Data.Void (absurd)
import Execute.Database (createThis, deleteThis, editThis, getThis)
import Execute.Draft (createDraft, editDraft, publish)
import qualified Execute.Utils as U
import qualified Result as R
import qualified Types as T
import Prelude hiding (read)

executeAction ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat Action
    -> m T.APIResult
executeAction h (T.WhoWhat token action) =
  let withToken f arg = f h (T.WhoWhat token arg)
  in case action of
    AAuthors x -> withToken executeAuthor x
    ACategory x -> withToken executeCategory x
    APosts x -> withToken executePosts x
    ATags x -> withToken executeTags x
    AUsers x -> withToken executeUsers x
    AAuth x -> U.authenticate (D.authHandler h) (D.log h) x
    AComments x -> withToken executeComments x
    ADrafts x -> withToken executeDraft x
    APublish x -> withToken executePublish x

executePosts ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionPosts1
    -> m T.APIResult
executePosts h (T.WhoWhat _ (T.GC x)) =
    getThis (D.getComments (D.commentsHandler h) (D.log h)) x
executePosts h (T.WhoWhat _ (T.AP crud)) =
    T.foldCRUD
        crud
        absurd
        (getThis $ D.getPosts h (D.log h))
        absurd
        absurd

executeAuthor ::
       CMC.MonadCatch m
    => D.Handle m
    -> T.WhoWhat T.ActionAuthors
    -> m T.APIResult
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
executeCategory h (T.WhoWhat token crud) =
    T.foldCRUD
        crud
        (withAdmin .
         createThis T.ECategory (D.createCategory catsH logger))
        (getThis $ D.getCategories catsH logger)
        update
        (withAdmin .
         deleteThis T.ECategory (D.deleteCategory catsH logger))
  where
    logger = D.log h
    authH = D.authHandler h
    catsH = D.catsHandler h
    withAdmin = (U.withAuthAdmin authH logger token >>)

    update u = do
        U.withAuthAdmin authH logger token
        mModifError <- U.checkCategoryUpdate catsH logger u
        let action =
              editThis
                T.ECategory (D.editCategory catsH logger) u
        maybe action (pure . T.RFailed T.ECategory) mModifError


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
        absurd
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
        withUser action x = do
            user <- U.withUser authH logger token
            action $ T.WithUser user x
    T.foldCRUD
        crud
        (withUser $ createThis T.EComment (D.createComment commentsH logger))
        (getThis $ D.getComments commentsH logger)
        absurd
        (withUser $ deleteThis T.EComment (D.deleteComment commentsH logger))

executeDraft ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> T.WhoWhat T.ActionDrafts
    -> m T.APIResult
executeDraft h (T.WhoWhat token crud) =
    T.foldCRUD
        crud
        (withAuthor $ createDraft draftsH logger)
        read
        (withAuthor $ editDraft draftsH logger)
        (withAuthor $ deleteThis T.EDraft (D.deleteDraft draftsH logger))
  where
    logger = D.log h
    draftsH = D.draftsHandler h
    authH = D.authHandler h

    withAuthor action x = do
        authorId <- T._a_authorId <$>
            U.withAuthor authH logger token
        action (T.WithAuthor authorId x)

    read r = do
        authorId <- T._a_authorId <$>
            U.withAuthor authH logger token
        getThis (D.getDrafts draftsH logger) $
            fmap (T.WithAuthor authorId) r
  

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
handleError h (T.WhoWhat _ (ActionErrorPerms False err)) =
    case err of
        ERequiredFieldMissing x -> handleFieldMissing h x
        EInvalidFieldValue x    -> handleInvalidValue h x
        EInvalidEndpoint        -> handleInvalidEndpoint h
handleError h (T.WhoWhat token (ActionErrorPerms True err)) =
    CMC.handle (handleForbidden h) $ do
        U.withAuthAdmin (D.authHandler h) (D.log h) token
        handleError h (T.WhoWhat token (ActionErrorPerms False err))

handleForbidden ::
       (CMC.MonadCatch m)
    => D.Handle m
    -> CMC.SomeException
    -> m R.Response
handleForbidden h _ = do
    D.logError h R.forbidden
    pure (R.notFound "Invalid endpoint")

handleInvalidEndpoint :: (Monad m) => D.Handle m -> m R.Response
handleInvalidEndpoint h = do
    let msg =  "Invalid endpont"
    D.logError h msg
    pure $ R.notFound msg

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
