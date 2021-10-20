{-# LANGUAGE RecordWildCards, DeriveAnyClass #-}

module App.Database.Postgres where

import App.Database
import qualified App.Logger as Logger
import Control.Monad ((>=>))
import qualified Control.Monad.Catch as C (bracket)
import qualified Data.ByteString as B
import Database
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import GenericPretty
import IO.Postgres as IOP
import Types

data Config =
   Config
      { databaseName :: B.ByteString
      , userName :: B.ByteString
      , password :: B.ByteString
      , port :: Int
      }
   deriving (Show, PrettyShow, Generic)

data Resources =
   Resources
      { postgresConnection :: PS.Connection
      } -- deriving (Show)

--withHandle :: Connection -> (Handle -> IO a) -> IO a
--withHandle (Connection conn) func = do
connectionString :: Config -> B.ByteString
connectionString Config {..} =
   "dbname=" <>
   databaseName <>
   " user=" <> userName <> " password='" <> password <> "'"

--withResources :: 
withPostgresHandle ::
      Logger.Handle IO
   -> Config
   -> (Resources -> IO a)
   -> IO a
withPostgresHandle logger conf =
   C.bracket (initResources logger conf) closeResources

initResources :: Logger.Handle IO -> Config -> IO Resources
initResources _ conf = do
   let conStr = connectionString conf
   con <- PS.connectPostgreSQL conStr
   return Resources {postgresConnection = con}

closeResources :: Resources -> IO ()
closeResources resources =
   let conn = postgresConnection resources
    in PS.close conn -- close connection

resourcesToHandle ::
      Resources -> Logger.Handle IO -> Handle IO
resourcesToHandle (Resources con) logger =
   Handle
      { log = logger
      , userAuthor = userAuthor1 con
      , createUser = createThis con dummyCUser
      , deleteUser = deleteThis con dummyDUser
      , getUserByToken = getUserByToken1 con
      , getUserByLogin = getUserByLogin1 con
      , addToken = addToken1 con
      , generateToken = generateToken1
      , getAuthors = getThisPaginated con authorDummy
      , createAuthor = createThis con dummyCAuthor
      , editAuthor = editThis con dummyUAuthor
      , deleteAuthor = deleteThis con dummyDAuthor
      , getTags = getThisPaginated con tagDummy
      , createTag = createThis con dummyCTag
      , editTag = editThis con dummyUTag
      , deleteTag = deleteThis con dummyDTag
      , getCategories = getThisPaginated con catDummy
      , createCategory = createThis con dummyCCat
      , editCategory = editThis con dummyUCat
      , deleteCategory = deleteThis con dummyDCat
      , getPosts = getThisPaginated con postDummy
      , getComments = getThisPaginated con commentDummy
      , createComment = createThis con dummyCComment
      , deleteComment = deleteThis con dummyDComment
      , withTransaction = withTransaction1 con
      , attachTagsToDraft = IOP.attachTags con dummyHDraft
      , attachTagsToPost = IOP.attachTags con dummyHPost
      , editDraft = editThis con draftEditDummy
      , removeAllButGivenTagsDraft =
           IOP.removeAllButGivenTags con dummyHDraft
      , removeAllButGivenTagsPost =
           IOP.removeAllButGivenTags con dummyHPost
      , getDraftRaw =
           \l ->
              getThis con draftRawDummy l >=>
              checkUnique Nothing Just EDraft _dr_draftId
      , createDraft = createThis con draftCreateDummy
      , editDraftPublish =
           editThis con draftEditPublishDummy
      , editPostPublish = editThis con dummyUPost
      , createPost = createThis con dummyCPost
      , getDrafts = getThisPaginated con draftDummy
      , deleteDraft = deleteThis con dummyDDraft
      }
