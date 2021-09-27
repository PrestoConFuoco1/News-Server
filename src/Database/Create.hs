{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts,
RecordWildCards
#-}


module Database.Create where


import qualified Database.PostgreSQL.Simple as PS
import qualified Data.ByteString as B
import Data.Text as T (Text, pack)
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import qualified Types as Ty
import Utils
import Types

class CreateSQL s where
    type Create s :: *
    createQuery :: s -> Create s -> (PS.Query, [SqlValue])
    cName :: s -> T.Text
    cUniqueField :: s -> T.Text -- ???
    cForeign :: s -> T.Text -- ?????



newtype CTag = CTag ()
dummyCTag = CTag ()

instance CreateSQL CTag where
    type Create CTag = CreateTag
    createQuery _ CreateTag{..} =
        ("INSERT INTO news.tag (name) VALUES (?) RETURNING tag_id", [SqlValue _ct_tagName])
    cName _ = "tag"
    cUniqueField _ = "name"
    cForeign _ = "error"


newtype CCat = CCat ()
dummyCCat = CCat ()

instance CreateSQL CCat where
    type Create CCat = CreateCategory
    createQuery _ CreateCategory{..} =
        ("INSERT INTO news.category (name, parent_category_Id) VALUES (?, ?) RETURNING category_id",
        [SqlValue _cc_catName, SqlValue _cc_parentCat])
    cName _ = "category"
    cUniqueField _ = "name"
    cForeign _ = "parent_id"

newtype CUser = CUser ()
dummyCUser = CUser ()

instance CreateSQL CUser where
    type Create CUser = CreateUser
    createQuery _ CreateUser{..} =
        ("INSERT INTO news.users (login, pass_hash, firstname, lastname) VALUES (?, ?, ?, ?) RETURNING user_id",
        [SqlValue _cu_login, SqlValue _cu_passHash, SqlValue _cu_firstName, SqlValue _cu_lastName])
    cName _ = "user"
    cUniqueField _ = "login"
    cForeign _ = "error"

newtype CAuthor = CAuthor ()
dummyCAuthor = CAuthor ()

instance CreateSQL CAuthor where
    type Create CAuthor = CreateAuthor
    createQuery _ CreateAuthor{..} =
        ("INSERT INTO news.author (user_id, description) VALUES (?, ?) RETURNING author_id",
        [SqlValue _ca_userId, SqlValue _ca_description])
    cName _ = "author"
    cUniqueField _ = "user_id"
    cForeign _ = "user_id"

newtype CComment = CComment ()
dummyCComment = CComment ()

instance CreateSQL CComment where
    type Create CComment = WithUser CreateComment
    createQuery _ (WithUser u CreateComment{..}) =
        ("INSERT INTO news.comment (user_id, post_id, content) values (?, ?, ?) RETURNING comment_id",
        [SqlValue $ _u_id u, SqlValue _ccom_postId, SqlValue _ccom_content])
--insert into comment (post_id, content, user_id) values (1, 'comment to first post', 2);
    cName _ = "comment"
    cUniqueField _ = "unique error"
    cForeign _ = "post_id"



newtype CPost = CPost ()
dummyCPost = CPost ()

instance CreateSQL CPost where
    type Create CPost = DraftRaw
    createQuery _ DraftRaw{..} =
        ("INSERT INTO news.post ( title, \
                                 \ author_id,\
                                 \ category_id,\
                                 \ content,\
                                 \ photo,\
                                 \ extra_photos\
                           \ ) values (?, ?, ?, ?, ?, ?) RETURNING post_id",
        [SqlValue _dr_title, SqlValue _dr_authorId, SqlValue _dr_categoryId, SqlValue _dr_content, SqlValue _dr_mainPhoto,
        SqlValue $ fmap PSTy.PGArray _dr_extraPhotos])
--insert into comment (post_id, content, user_id) values (1, 'comment to first post', 2);
    cName _ = "post"
    cUniqueField _ = "unique error"
    cForeign _ = "foreign error"


newtype CDraft = CDraft ()
draftCreateDummy = CDraft ()


instance CreateSQL CDraft where
    type Create CDraft = WithAuthor CreateDraft
    createQuery s (WithAuthor a CreateDraft{..}) = ("INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos) \
         \VALUES (?, ?, ?, ?, ?, ?) RETURNING draft_id",
        [SqlValue _cd_title, SqlValue a, SqlValue _cd_categoryId, SqlValue _cd_content,
         SqlValue _cd_mainPhoto, SqlValue $ fmap PSTy.PGArray _cd_extraPhotos])
    cName _ = "draft"
    cUniqueField _ = "for what the fuck i created this function? it's wothless"
    cForeign _ = "author_id or category_id"


