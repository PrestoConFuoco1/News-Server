{-# LANGUAGE RecordWildCards, DeriveAnyClass, TypeApplications #-}

module App.Database.Postgres where

import App.Database (Handle(..))
import qualified App.Logger as Logger
import Control.Monad ((>=>))
import qualified Control.Monad.Catch as C (bracket)
import qualified Data.ByteString as B
import Database
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import qualified IO.Postgres as IOP
import Types

data Config =
   Config
      { databaseName :: B.ByteString
      , userName :: B.ByteString
      , password :: B.ByteString
      , port :: Int
      }
   deriving (Show, GP.PrettyShow, Generic)

data Resources =
   Resources
      { postgresConnection :: PS.Connection
      }

connectionString :: Config -> B.ByteString
connectionString Config {..} =
   "dbname=" <>
   databaseName <>
   " user=" <> userName <> " password='" <> password <> "'"

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
   pure Resources {postgresConnection = con}

closeResources :: Resources -> IO ()
closeResources resources =
   let conn = postgresConnection resources
    in PS.close conn

resourcesToHandle ::
      Resources -> Logger.Handle IO -> Handle IO
resourcesToHandle (Resources con) logger =
   Handle
      { log = logger
      , userAuthor = IOP.userAuthor con
      , createUser = IOP.createThis @CreateUser con
      , deleteUser = IOP.deleteThis @DeleteUser con
      , getUserByToken = IOP.getUserByToken con
      , getUserByLogin = IOP.getUserByLogin con
      , addToken = IOP.addToken con
      , generateToken = IOP.generateToken
      , getAuthors = IOP.getThisPaginated @GetAuthors con
      , createAuthor = IOP.createThis @CreateAuthor con
      , editAuthor = IOP.editThis @EditAuthor con
      , deleteAuthor = IOP.deleteThis @DeleteAuthor con
      , getTags = IOP.getThisPaginated @GetTags con
      , createTag = IOP.createThis @CreateTag con
      , editTag = IOP.editThis @EditTag con
      , deleteTag = IOP.deleteThis @DeleteTag con
      , getCategories = IOP.getThisPaginated @GetCategories con
      , createCategory = IOP.createThis @CreateCategory con
      , editCategory = IOP.editThis @EditCategory con
      , deleteCategory = IOP.deleteThis @DeleteCategory con
      , getPosts = IOP.getThisPaginated @GetPosts con
      , getComments = IOP.getThisPaginated @GetComments con
      , createComment = IOP.createThis @(WithUser CreateComment) con
      , deleteComment = IOP.deleteThis @(WithUser DeleteComment) con
      , withTransaction = IOP.withTransaction con
      , attachTagsToDraft = IOP.attachTags con HDraft
      , attachTagsToPost = IOP.attachTags con HPost
      , editDraft = IOP.editThis @(WithAuthor EditDraft) con
      , removeAllButGivenTagsDraft =
           IOP.removeAllButGivenTags con HDraft
      , removeAllButGivenTagsPost =
           IOP.removeAllButGivenTags con HPost
      , getDraftRaw =
           \l ->
              IOP.getThis @(WithAuthor Publish) con l >=>
              IOP.checkUnique Nothing Just EDraft _dr_draftId
      , createDraft = IOP.createThis @(WithAuthor CreateDraft) con
      , editDraftPublish =
           IOP.editThis @EditDraftPublish con
      , editPostPublish = IOP.editThis @PublishEditPost con
      , createPost = IOP.createThis @DraftRaw con
      , getDrafts = IOP.getThisPaginated @(WithAuthor GetDrafts) con
      , deleteDraft = IOP.deleteThis @(WithAuthor DeleteDraft) con
      }

