{-# LANGUAGE
    TypeFamilies
#-}
module Database.Delete where


import qualified Database.PostgreSQL.Simple as PS
import Database.SqlValue
import Types


class DeleteSQL s where
    type Del s :: *
    deleteQuery :: s -> Del s -> (PS.Query, [SqlValue])
    dName :: s -> Entity

newtype DTag = DTag ()
dummyDTag :: DTag
dummyDTag = DTag ()

instance DeleteSQL DTag where
    type Del DTag = DeleteTag
    deleteQuery _ dt = ("DELETE FROM news.tag WHERE tag_id = ? RETURNING tag_id", [SqlValue $ _dt_tagId dt])
    dName _ = ETag


newtype DCat = DCat ()
dummyDCat :: DCat
dummyDCat = DCat ()

instance DeleteSQL DCat where
    type Del DCat = DeleteCategory
    deleteQuery _ dc = ("DELETE FROM news.category WHERE category_id = ? RETURNING category_id", [SqlValue $ _dc_catId dc])
    dName _ = ECategory


newtype DAuthor = DAuthor ()
dummyDAuthor :: DAuthor
dummyDAuthor = DAuthor ()

instance DeleteSQL DAuthor where
    type Del DAuthor = DeleteAuthor
    deleteQuery _ da = ("DELETE FROM news.author WHERE author_id = ? RETURNING author_id", [SqlValue $ _da_authorId da])
    dName _ = EAuthor

newtype DUser = DUser ()
dummyDUser :: DUser
dummyDUser = DUser ()

instance DeleteSQL DUser where
    type Del DUser = DeleteUser
    deleteQuery _ du = ("DELETE FROM news.users WHERE user_id = ? RETURNING user_id", [SqlValue $ _du_userId du])
    dName _ = EUser


newtype DComment = DComment ()
dummyDComment :: DComment
dummyDComment = DComment ()


isAdmin :: User -> Bool
isAdmin u = maybe False id $ _u_admin u



instance DeleteSQL DComment where
    type Del DComment = WithUser DeleteComment
    deleteQuery _ (WithUser u dc) = let
        str = " DELETE FROM news.comment WHERE comment_id = ? "
        userWhere = " AND user_id = ? "
        returning = " RETURNING comment_id"
        userParam = SqlValue $ _u_id u
        commentParam = SqlValue $ _dc_commentId dc
        in  if   isAdmin u
            then (str <> returning, [commentParam])
            else (str <> userWhere <> returning, [commentParam, userParam])


    dName _ = EComment



newtype DDraft = DDraft ()
dummyDDraft :: DDraft
dummyDDraft = DDraft ()

instance DeleteSQL DDraft where
    type Del DDraft = WithAuthor DeleteDraft
    deleteQuery _ (WithAuthor a (DeleteDraft d)) = (
        "\
\ DELETE FROM news.draft \
\ WHERE draft_id = ? AND author_id = ? RETURNING draft_id"
        , [SqlValue d, SqlValue a])


    dName _ = EDraft
