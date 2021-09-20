{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts,
RecordWildCards
#-}


module Database.Create where

--import Action.RequestToAction

import Action.Tags.Types
import Action.Category.Types
import Action.Users.Types
import Action.Authors.Types
import Action.Comments.Types
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.ByteString as B
import ExecuteTypes

class (PS.ToRow (Create s)) => CreateSQL s where
    type Create s :: *
    createQuery :: s -> PS.Query
    cName :: s -> B.ByteString
    cUniqueField :: s -> B.ByteString -- ???
    cForeign :: s -> B.ByteString -- ?????



newtype CTag = CTag ()
dummyCTag = CTag ()

instance CreateSQL CTag where
    type Create CTag = CreateTag
    createQuery _ = "INSERT INTO news.tag (name) VALUES (?)"
    cName _ = "tag"
    cUniqueField _ = "name"
    cForeign _ = "error"


newtype CCat = CCat ()
dummyCCat = CCat ()

instance CreateSQL CCat where
    type Create CCat = CreateCategory
    createQuery _ = "INSERT INTO news.category (name, parent_category_Id) VALUES (?, ?)"
    cName _ = "category"
    cUniqueField _ = "name"
    cForeign _ = "parent_id"

newtype CUser = CUser ()
dummyCUser = CUser ()

instance CreateSQL CUser where
    type Create CUser = CreateUser
    createQuery _ = "INSERT INTO news.users (firstname, lastname, login, pass_hash) VALUES (?, ?, ?, ?)"
    cName _ = "user"
    cUniqueField _ = "login"
    cForeign _ = "error"

newtype CAuthor = CAuthor ()
dummyCAuthor = CAuthor ()

instance CreateSQL CAuthor where
    type Create CAuthor = CreateAuthor
    createQuery _ = "INSERT INTO news.author (user_id, description) VALUES (?, ?)"
    cName _ = "author"
    cUniqueField _ = "user_id"
    cForeign _ = "user_id"

newtype CComment = CComment ()
dummyCComment = CComment ()

instance CreateSQL CComment where
    type Create CComment = WithUser CreateComment
    createQuery _ = "INSERT INTO news.comment (user_id, post_id, content) values (?, ?, ?)"
--insert into comment (post_id, content, user_id) values (1, 'comment to first post', 2);
    cName _ = "comment"
    cUniqueField _ = "unique error"
    cForeign _ = "foreign error"



