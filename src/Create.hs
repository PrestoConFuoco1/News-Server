{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts,
RecordWildCards
#-}


module Create where

--import Action.RequestToAction

import Action.Tags.Types
import Action.Category.Types
import Action.Users.Types
import qualified Database.PostgreSQL.Simple as PS
import qualified Data.ByteString as B

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

{-
createUser :: (MonadServer m) => CreateUser -> m Response
createUser CreateUser{..} =
    let str = "INSERT INTO news.users (firstname, lastname, login, pass_hash) VALUES (?, ?, ?, ?)"

    in  (execute str (_cu_firstName, _cu_lastName, _cu_login, _cu_passHash) >> return  (ok "User successfully created"))
            `CMC.catches` [CMC.Handler sqlH]
  where sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                        "Failed to create new user, login is already in use\n\
                        \login = " <> E.encodeUtf8 _cu_login <> 
                        "\nSqlError: " <> PS.sqlErrorMsg e 
                return $ bad "User with such login already exists."
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")


createTag :: (MonadServer m) => CreateTag -> m Response
createTag CreateTag{..} = do

        let str = "INSERT INTO news.tag (name) VALUES (?)"
        (execute str [_ct_tagName] >> (return $ ok "Tag successfully created"))
            `CMC.catches` [CMC.Handler sqlH]

  where name = _ct_tagName
        sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                        "Failed to create new tag, there is one with such name\n\
                       \ tag_name = " <> E.encodeUtf8 name <> 
                        "\nSqlError: " <> PS.sqlErrorMsg e 
                return $ Response NHT.status400 $ "Tag with such name already exists."
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")



createCategory :: (MonadServer m) => CreateCategory -> m Response
createCategory CreateCategory{..} = do

        let str = "INSERT INTO news.category (name, parent_category_Id) VALUES (?, ?)"
        (execute str (_cc_catName, _cc_parentCat) >> (return $ ok "Category successfully created"))
            `CMC.catches` [CMC.Handler sqlH]

  where name = _cc_catName --crecat
        sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                        "Failed to create new category, there is one with such name\n\
                        \category_name = " <> E.encodeUtf8 name <> 
                        "\nSqlError: " <> PS.sqlErrorMsg e 
                return $ Response NHT.status400 $ "Category with such name already exists."
            | foreignKeyViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to created new category, parent category id is invalid"
                return $ bad "Failed to created new category, parent category id is invalid"
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")



-}


