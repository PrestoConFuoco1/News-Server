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
    cName :: s -> Ty.Entity



newtype CTag = CTag ()
dummyCTag = CTag ()

instance CreateSQL CTag where
    type Create CTag = CreateTag
    createQuery _ CreateTag{..} =
        ("INSERT INTO news.tag (name) VALUES (?) RETURNING tag_id", [SqlValue _ct_tagName])
    cName _ = Ty.ETag


newtype CCat = CCat ()
dummyCCat = CCat ()

instance CreateSQL CCat where
    type Create CCat = CreateCategory
    createQuery _ CreateCategory{..} =
        ("INSERT INTO news.category (name, parent_category_Id) VALUES (?, ?) RETURNING category_id",
        [SqlValue _cc_catName, SqlValue _cc_parentCat])
    cName _ = Ty.ECategory

newtype CUser = CUser ()
dummyCUser = CUser ()

instance CreateSQL CUser where
    type Create CUser = CreateUser
    createQuery _ CreateUser{..} =
        ("INSERT INTO news.users (login, pass_hash, firstname, lastname) VALUES (?, ?, ?, ?) RETURNING user_id",
        [SqlValue _cu_login, SqlValue _cu_passHash, SqlValue _cu_firstName, SqlValue _cu_lastName])
    cName _ = Ty.EUser

newtype CAuthor = CAuthor ()
dummyCAuthor = CAuthor ()

instance CreateSQL CAuthor where
    type Create CAuthor = CreateAuthor
    createQuery _ CreateAuthor{..} =
        ("INSERT INTO news.author (user_id, description) VALUES (?, ?) RETURNING author_id",
        --("INSERT INTO author (user_id, description) VALUES (?, ?) RETURNING author_id",
        -- this is bad query
        [SqlValue _ca_userId, SqlValue _ca_description])
    cName _ = Ty.EAuthor

newtype CComment = CComment ()
dummyCComment = CComment ()

instance CreateSQL CComment where
    type Create CComment = WithUser CreateComment
    createQuery _ (WithUser u CreateComment{..}) =
        ("INSERT INTO news.comment (user_id, post_id, content) values (?, ?, ?) RETURNING comment_id",
        [SqlValue $ _u_id u, SqlValue _ccom_postId, SqlValue _ccom_content])
--insert into comment (post_id, content, user_id) values (1, 'comment to first post', 2);
    cName _ = Ty.EComment



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
    cName _ = Ty.EPost


newtype CDraft = CDraft ()
draftCreateDummy = CDraft ()


instance CreateSQL CDraft where
    type Create CDraft = WithAuthor CreateDraft
    createQuery s (WithAuthor a CreateDraft{..}) = ("INSERT INTO news.draft (title, author_id, category_id, content, photo, extra_photos) \
         \VALUES (?, ?, ?, ?, ?, ?) RETURNING draft_id",
        [SqlValue _cd_title, SqlValue a, SqlValue _cd_categoryId, SqlValue _cd_content,
         SqlValue _cd_mainPhoto, SqlValue $ fmap PSTy.PGArray _cd_extraPhotos])
    cName _ = Ty.EDraft


