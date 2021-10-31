module Handlers where

import App.Database
import qualified App.Logger as L
import Prelude hiding (log)
import Types
import qualified Utils as S

noUserByToken :: L.LoggerHandler IO -> Token -> IO (Maybe User)
noUserByToken _ _ = return Nothing

notAdminByToken :: L.LoggerHandler IO -> Token -> IO (Maybe User)
notAdminByToken _ _ = return (Just $ User { _u_admin = Just False })

adminByToken :: L.LoggerHandler IO -> Token -> IO (Maybe User)
adminByToken _ _ = return (Just $ User { _u_admin = Just True })

userByToken :: L.LoggerHandler IO -> Token -> IO (Maybe User)
userByToken _ _ = return $ Just defaultUser

fromJust (Just x) = x

defaultUser = User {
    _u_id = 1
    , _u_firstname = "defaultFirstname"
    , _u_lastname = "defaultLastname"
    , _u_pictureUrl = Just "defaultPicture"
    , _u_login = "defaultLogin"
    , _u_passHash = "defaultPassword"
    , _u_creationDate = fromJust $ S.readDay "2001-09-11"
    , _u_admin = Just False
    }




defaultHandler :: Handle IO
defaultHandler = Handle
    {

    -- logger
    log = L.emptyLogger,

    -- basic

    createUser = error "function createUser should not be called in this test",
    deleteUser = error "function deleteUser should not be called in this test",

    getUserByToken = error "function getUserByToken should not be called in this test",
    userAuthor = error "function userAuthor should not be called in this test",

    getUserByLogin = error "function getUserByLogin should not be called in this test",
    addToken = error "function addToken should not be called in this test",
    --randomString = error "this function should not be called in this test",
    generateToken = error "function generateToken should not be called in this test",

    -- entities,

    getAuthors = error "function getAuthors should not be called in this test",
    createAuthor = error "function createAuthor should not be called in this test",
    editAuthor = error "function editAuthor should not be called in this test",
    deleteAuthor = error "function deleteAuthor should not be called in this test",

    getTags = error "function getTags should not be called in this test",
    createTag = error "function createTag should not be called in this test",
    editTag = error "function editTag should not be called in this test",
    deleteTag = error "function deleteTag should not be called in this test",

    getCategories = error "function getCategories should not be called in this test",
    createCategory = error "function createCategory should not be called in this test",
    editCategory = error "function editCategory should not be called in this test",
    deleteCategory = error "function deleteCategory should not be called in this test",

    getPosts = error "function getPosts should not be called in this test",
    getComments = error "function getComments should not be called in this test",
    createComment = error "function createComment should not be called in this test",
    deleteComment = error "function deleteComment should not be called in this test",

    -- drafts, posts, publish,

    withTransaction = error "function withTransaction should not be called in this test",


    getDrafts = error "function getDrafts should not be called in this test",
    deleteDraft = error "function deleteDraft should not be called in this test",

    createDraft = error "function createDraft should not be called in this test",
    attachTagsToDraft = error "function attachTagsToDraft should not be called in this test",
    editDraft = error "function editDraft should not be called in this test",
    removeAllButGivenTagsDraft = error "function removeAllButGivenTags should not be called in this test",

    getDraftRaw = error "function getDraftRaw should not be called in this test",

    createPost = error "function createPost should not be called in this test",
    editDraftPublish = error "function editDraftPublish should not be called in this test",
    attachTagsToPost = error "function attachTagsToPost should not be called in this test",

    editPostPublish = error "function editPostPublish should not be called in this test",
    removeAllButGivenTagsPost = error "function removeAllButGivenTagsPost should not be called in this test"

    }


