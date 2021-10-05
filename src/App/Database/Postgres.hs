module App.Database.Postgres where

--import qualified App.Database as D
import App.Database

import IO.Postgres as IOP

import qualified Data.ByteString as B
import qualified Database.PostgreSQL.Simple as PS
import qualified App.Logger as Logger
import Prelude hiding (log)
import Database
import Database.HasTags
import Types
import Control.Monad ((>=>))

data Config = Config {
    databaseName :: B.ByteString,
    userName :: B.ByteString,
    password :: B.ByteString
    } deriving (Show)


data Connection = Connection {
    postgresConnection :: PS.Connection
    } -- deriving (Show)

--withHandle :: Connection -> (Handle -> IO a) -> IO a
--withHandle (Connection conn) func = do

connectionToHandle :: Connection -> Logger.Handle IO -> Handle IO
connectionToHandle (Connection con) logger =
    Handle {


        log = logger,
    
        userAuthor = userAuthor1 con,
    
        createUser = createThis con dummyCUser,
        deleteUser = deleteThis con dummyDUser,
    
        getUserByToken = getUserByToken1 con,
    
        getUserByLogin = getUserByLogin1 con,
    
        addToken = addToken1 con,
        --randomString1 len = liftIO $ randomString' len,
        --randomString1 = \len -> undefined,
        generateToken = generateToken1,
    
     
        getAuthors = getThisPaginated con authorDummy,
        createAuthor = createThis con dummyCAuthor,
        editAuthor = editThis con dummyUAuthor,
        deleteAuthor = deleteThis con dummyDAuthor,
    
        getTags = getThisPaginated con tagDummy,
        createTag = createThis con dummyCTag,
        editTag = editThis con dummyUTag,
        deleteTag = deleteThis con dummyDTag,
    
        getCategories = getThisPaginated con catDummy,
        createCategory = createThis con dummyCCat,
        editCategory = editThis con dummyUCat,
        deleteCategory = deleteThis con dummyDCat,
    
        getPosts = getThisPaginated con postDummy,
    
        getComments = getThisPaginated con commentDummy,
        createComment = createThis con dummyCComment,
        deleteComment = deleteThis con dummyDComment,
    
    
       
        
        withTransaction = withTransaction1 con,
        attachTagsToDraft = IOP.attachTags con dummyHDraft,
        attachTagsToPost = IOP.attachTags con dummyHPost,
        editDraft = editThis con draftEditDummy,
        removeAllButGivenTagsDraft = IOP.removeAllButGivenTags con dummyHDraft,
        removeAllButGivenTagsPost = IOP.removeAllButGivenTags con dummyHPost,
        getDraftRaw = getThis con draftRawDummy >=> checkUnique Nothing Just EDraft _dr_draftId,

        createDraft = createThis con draftCreateDummy,
        editDraftPublish = editThis con draftEditPublishDummy,
        editPostPublish = editThis con dummyUPost,

        createPost = createThis con dummyCPost,


        getDrafts = getThisPaginated con draftDummy,
        deleteDraft = deleteThis con dummyDDraft



        }
