{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts
#-}
module Database.Delete where


import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as PS

import Action.Tags
import Action.Authors
import Action.Category
import Action.Users
import Action.Comments
import Action.Draft
import Execute.Types
import Database.SqlValue

class (PS.ToRow (Del s)) => DeleteSQL s where
    type Del s :: *
    deleteQuery :: s -> Del s -> (PS.Query, [SqlValue])
    dName :: s -> B.ByteString

newtype DTag = DTag ()
dummyDTag = DTag ()

instance DeleteSQL DTag where
    type Del DTag = DeleteTag
    deleteQuery _ dt = ("DELETE FROM news.tag WHERE tag_id = ?", [SqlValue $ _dt_tagId dt])
    dName _ = "tag"


newtype DCat = DCat ()
dummyDCat = DCat ()

instance DeleteSQL DCat where
    type Del DCat = DeleteCategory
    deleteQuery _ dc = ("DELETE FROM news.category WHERE category_id = ?", [SqlValue $ _dc_catId dc])
    dName _ = "category"


newtype DAuthor = DAuthor ()
dummyDAuthor = DAuthor ()

instance DeleteSQL DAuthor where
    type Del DAuthor = DeleteAuthor
    deleteQuery _ da = ("DELETE FROM news.author WHERE author_id = ?", [SqlValue $ _da_authorId da])
    dName _ = "author"

newtype DUser = DUser ()
dummyDUser = DUser ()

instance DeleteSQL DUser where
    type Del DUser = DeleteUser
    deleteQuery _ du = ("DELETE FROM news.users WHERE user_id = ?", [SqlValue $ _du_userId du])
    dName _ = "user"


newtype DComment = DComment ()
dummyDComment = DComment ()

instance DeleteSQL DComment where
    type Del DComment = WithUser DeleteComment
    deleteQuery _ (WithUser u dc) = (
        "\
\ DELETE FROM news.comment c \
\ WHERE c.comment_id = ? AND EXISTS \
\    (SELECT * FROM news.users u WHERE u.user_id = ? AND (u.is_admin = true OR u.user_id = c.user_id))"
        , [SqlValue $ _dc_commentId dc, SqlValue u])


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
