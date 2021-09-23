{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts
#-}
module Database.Delete where


import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS

import Action.Tags
import Action.Authors
import Action.Category
import Action.Users
import Action.Comments
import Action.Draft
import Execute.Types
import Database.SqlValue
import MonadTypes
import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Exceptions as Ex
import qualified Types as Ty

--import Execute.Permissions

deleteThis' :: (MonadServer m, DeleteSQL s) => s -> Del s -> m [Int]
deleteThis' s del = do
    let (str, params) = deleteQuery s del
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    --withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
    ids <- fmap (map PSTy.fromOnly) $ query str params
    logInfo $ "Deleted " <> dName s <> " with id = " <> showList ids
    return ids



class DeleteSQL s where
    type Del s :: *
    deleteQuery :: s -> Del s -> (PS.Query, [SqlValue])
    dName :: s -> T.Text

newtype DTag = DTag ()
dummyDTag = DTag ()

instance DeleteSQL DTag where
    type Del DTag = DeleteTag
    deleteQuery _ dt = ("DELETE FROM news.tag WHERE tag_id = ? RETURNING tag_id", [SqlValue $ _dt_tagId dt])
    dName _ = "tag"


newtype DCat = DCat ()
dummyDCat = DCat ()

instance DeleteSQL DCat where
    type Del DCat = DeleteCategory
    deleteQuery _ dc = ("DELETE FROM news.category WHERE category_id = ? RETURNING category_id", [SqlValue $ _dc_catId dc])
    dName _ = "category"


newtype DAuthor = DAuthor ()
dummyDAuthor = DAuthor ()

instance DeleteSQL DAuthor where
    type Del DAuthor = DeleteAuthor
    deleteQuery _ da = ("DELETE FROM news.author WHERE author_id = ? RETURNING author_id", [SqlValue $ _da_authorId da])
    dName _ = "author"

newtype DUser = DUser ()
dummyDUser = DUser ()

instance DeleteSQL DUser where
    type Del DUser = DeleteUser
    deleteQuery _ du = ("DELETE FROM news.users WHERE user_id = ? RETURNING user_id", [SqlValue $ _du_userId du])
    dName _ = "user"


newtype DComment = DComment ()
dummyDComment = DComment ()


isAdmin :: Ty.User -> Bool
--isAdmin u = Ty._u_admin u /= Just True
isAdmin u = maybe False id $ Ty._u_admin u



instance DeleteSQL DComment where
    type Del DComment = WithUser DeleteComment
    deleteQuery _ (WithUser u dc) = let
        str = " DELETE FROM news.comment WHERE comment_id = ? "
        userWhere = " AND user_id = ? "
        returning = " RETURNING comment_id"
        userParam = SqlValue $ Ty._u_id u
        commentParam = SqlValue $ _dc_commentId dc
        in  if   isAdmin u
            then (str <> returning, [commentParam])
            else (str <> userWhere <> returning, [commentParam, userParam])


    dName _ = "comment"



newtype DDraft = DDraft ()
dummyDDraft = DDraft ()

instance DeleteSQL DDraft where
    type Del DDraft = WithAuthor DeleteDraft
    deleteQuery _ (WithAuthor a (DeleteDraft d)) = (
        "\
\ DELETE FROM news.draft d \
\ WHERE d.draft_id = ? AND d.author_id = ?"
        , [SqlValue d, SqlValue a])


    dName _ = "draft"
