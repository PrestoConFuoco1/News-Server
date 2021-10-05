module Handlers where

import App.Database
import qualified App.Logger as L
import Prelude hiding (log)
import Types

noUserByToken :: Token -> IO (Maybe User)
noUserByToken _ = return Nothing

notAdminByToken :: Token -> IO (Maybe User)
notAdminByToken _ = return (Just $ User { _u_admin = Just False })

adminByToken :: Token -> IO (Maybe User)
adminByToken _ = return (Just $ User { _u_admin = Just True })



defaultHandler :: Handle IO
defaultHandler = Handle
    {

    -- logger
    log = L.emptyLogger,

    -- basic

    createUser = undefined,
    deleteUser = undefined,

    getUserByToken = undefined,
    userAuthor = undefined,

    getUserByLogin = undefined,
    addToken = undefined,
    --randomString = undefined,
    generateToken = undefined,

    -- entities,

    getAuthors = undefined,
    createAuthor = undefined,
    editAuthor = undefined,
    deleteAuthor = undefined,

    getTags = undefined,
    createTag = undefined,
    editTag = undefined,
    deleteTag = undefined,

    getCategories = undefined,
    createCategory = undefined,
    editCategory = undefined,
    deleteCategory = undefined,

    getPosts = undefined,
    getComments = undefined,
    createComment = undefined,
    deleteComment = undefined,

    -- drafts, posts, publish,

    withTransaction = undefined,


    getDrafts = undefined,
    deleteDraft = undefined,

    createDraft = undefined,
    attachTagsToDraft = undefined,
    editDraft = undefined,
    removeAllButGivenTagsDraft = undefined,

    getDraftRaw = undefined,

    createPost = undefined,
    editDraftPublish = undefined,
    attachTagsToPost = undefined,

    editPostPublish = undefined,
    removeAllButGivenTagsPost = undefined

    }


