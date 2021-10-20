module Execute where

import Action.Common
import Action.RequestToAction
import qualified App.Database as D
import Control.Exception (SomeException)
import qualified Control.Monad.Catch as CMC
   ( MonadCatch
   , MonadCatch
   , catch
   )
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text.Encoding as E (decodeUtf8)
import Data.Void (absurd)
import Execute.Database
import Execute.Draft
import Execute.Utils
import Result
import qualified Types as Ty
import Types

executeAction ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat Action
   -> m APIResult
executeAction h (WhoWhat y (AAuthors x)) =
   executeAuthor h (WhoWhat y x)
executeAction h (WhoWhat y (ACategory x)) =
   executeCategory h (WhoWhat y x)
executeAction h (WhoWhat y (APosts x)) =
   executePosts h (WhoWhat y x)
executeAction h (WhoWhat y (ATags x)) =
   executeTags h (WhoWhat y x)
executeAction h (WhoWhat y (AUsers x)) =
   executeUsers h (WhoWhat y x)
executeAction h (WhoWhat _ (AAuth x)) = authenticate h x
executeAction h (WhoWhat y (AComments x)) =
   executeComments h (WhoWhat y x)
executeAction h (WhoWhat y (ADrafts x)) =
   executeDraft h (WhoWhat y x)
executeAction h (WhoWhat y (APublish x)) =
   executePublish h (WhoWhat y x)

executePosts ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionPosts1
   -> m APIResult
executePosts h (WhoWhat _ (GC x)) =
   getThis1 (D.getComments h (D.log h)) x
executePosts h (WhoWhat _ (AP (Read x))) = do
   getThis1 (D.getPosts h (D.log h)) x
executePosts _ (WhoWhat _ (AP (Create x))) =
   return $ absurd x
executePosts _ (WhoWhat _ (AP (Update x))) =
   return $ absurd x
executePosts _ (WhoWhat _ (AP (Delete x))) =
   return $ absurd x

executeAuthor ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionAuthors
   -> m APIResult
executeAuthor h (WhoWhat y (Read x)) =
   withAuthAdmin h y >>
   getThis1 (D.getAuthors h (D.log h)) x
executeAuthor h (WhoWhat y (Create x)) =
   withAuthAdmin h y >>
   createThis1 EAuthor (D.createAuthor h (D.log h)) x
executeAuthor h (WhoWhat y (Update x)) =
   withAuthAdmin h y >>
   editThis1 EAuthor (D.editAuthor h (D.log h)) x
executeAuthor h (WhoWhat y (Delete x)) =
   withAuthAdmin h y >>
   deleteThis1 EAuthor (D.deleteAuthor h (D.log h)) x

{-
-}
executeTags ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionTags
   -> m APIResult
executeTags h (WhoWhat _ (Read x)) =
   getThis1 (D.getTags h (D.log h)) x
executeTags h (WhoWhat y (Create x)) =
   withAuthAdmin h y >>
   createThis1 ETag (D.createTag h (D.log h)) x
executeTags h (WhoWhat y (Update x)) =
   withAuthAdmin h y >>
   editThis1 ETag (D.editTag h (D.log h)) x
executeTags h (WhoWhat y (Delete x)) =
   withAuthAdmin h y >>
   deleteThis1 ETag (D.deleteTag h (D.log h)) x

executeCategory ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionCategory
   -> m APIResult
executeCategory h (WhoWhat _ (Read x)) =
   getThis1 (D.getCategories h (D.log h)) x
executeCategory h (WhoWhat y (Create x)) =
   withAuthAdmin h y >>
   createThis1 ECategory (D.createCategory h (D.log h)) x
executeCategory h (WhoWhat y (Update x)) =
   withAuthAdmin h y >>
   editThis1 ECategory (D.editCategory h (D.log h)) x
executeCategory h (WhoWhat y (Delete x)) =
   withAuthAdmin h y >>
   deleteThis1 ECategory (D.deleteCategory h (D.log h)) x

executeUsers ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionUsers
   -> m APIResult
executeUsers h (WhoWhat _ (Create x)) =
   createThis1 EUser (D.createUser h (D.log h)) x
executeUsers h (WhoWhat y (Delete x)) =
   withAuthAdmin h y >>
   deleteThis1 EUser (D.deleteUser h (D.log h)) x
executeUsers h (WhoWhat y (Read GetProfile)) =
   withAuth h y >>= getUser h
executeUsers _ (WhoWhat _ (Update x)) = return $ absurd x

executeComments ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionComments
   -> m APIResult
executeComments h (WhoWhat _ (Read x)) =
   getThis1 (D.getComments h (D.log h)) x
executeComments h (WhoWhat y (Create x)) =
   withAuth h y >>= maybeUserToUser h >>= \u ->
      createThis1 EComment (D.createComment h (D.log h)) $
      WithUser u x
executeComments h (WhoWhat y (Delete x)) =
   withAuth h y >>= maybeUserToUser h >>= \u ->
      deleteThis1 EComment (D.deleteComment h (D.log h)) $
      WithUser u x
executeComments _ (WhoWhat _ (Update x)) = return $ absurd x

executeDraft ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> WhoWhat ActionDrafts
   -> m APIResult
executeDraft h (WhoWhat y (Create x)) =
   withAuthor h y >>= \a ->
      createDraft h $ WithAuthor (Ty._a_authorId a) x
executeDraft h (WhoWhat y (Read (Paginated p s x))) =
   withAuthor h y >>= \a ->
      getThis1 (D.getDrafts h (D.log h)) $
      Paginated p s (WithAuthor (Ty._a_authorId a) x)
executeDraft h (WhoWhat y (Delete x)) =
   withAuthor h y >>= \a ->
      deleteThis1 EDraft (D.deleteDraft h (D.log h)) $
      WithAuthor (Ty._a_authorId a) x
executeDraft h (WhoWhat y (Update x)) =
   withAuthor h y >>= \a ->
      editDraft h $ WithAuthor (Ty._a_authorId a) x

executePublish ::
      (CMC.MonadCatch m)
   => D.Handle m
   -> WhoWhat Publish
   -> m APIResult
executePublish h (WhoWhat y x) =
   withAuthor h y >>= \a ->
      publish h $ WithAuthor (Ty._a_authorId a) x

handleError ::
      CMC.MonadCatch m
   => D.Handle m
   -> WhoWhat ActionErrorPerms
   -> m Response
handleError h (WhoWhat _ (ActionErrorPerms False (ERequiredFieldMissing x))) =
   handleFieldMissing h x
handleError h (WhoWhat _ (ActionErrorPerms False (EInvalidFieldValue x))) =
   handleInvalidValue h x
handleError h (WhoWhat _ (ActionErrorPerms False EInvalidEndpoint)) = do
   D.logError h "Invalid endpoint"
   return $ notFound "Invalid endpoint"
handleError h (WhoWhat y (ActionErrorPerms True x)) =
   (withAuthAdmin h y >>
    handleError h (WhoWhat y (ActionErrorPerms False x))) `CMC.catch`
   f h
  where
    f :: (CMC.MonadCatch m)
      => D.Handle m
      -> SomeException
      -> m Response
    f h' _ = handleForbidden h'

handleForbidden ::
      (CMC.MonadCatch m) => D.Handle m -> m Response
handleForbidden h =
   D.logError h forbidden >>
   return (notFound "Invalid endpoint")

handleFieldMissing ::
      (Monad m) => D.Handle m -> BS.ByteString -> m Response
handleFieldMissing h x = do
   let str = "Required field missing (" <> x <> ")"
   D.logError h $ E.decodeUtf8 str
   return $ bad $ E.decodeUtf8 str

handleInvalidValue ::
      (Monad m) => D.Handle m -> BS.ByteString -> m Response
handleInvalidValue _ x = do
   let str = "Invalid value of the field " <> x
   return $ bad $ E.decodeUtf8 str
