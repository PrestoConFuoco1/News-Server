{-# LANGUAGE RecordWildCards, DeriveAnyClass, TypeApplications #-}

module App.Database.Postgres
    ( Config(..)
    , withPostgresHandle
    , Resources(..)
    , resourcesToHandle
    ) where

import App.Database (Handle(..))
import qualified App.Database.Postgres.Internal as IOP
import qualified App.Logger as L
import Control.Monad ((>=>))
import qualified Control.Monad.Catch as C (bracket)
import qualified Data.ByteString as B
import Database
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import qualified Types as T

data Config =
    Config
        { databaseName :: B.ByteString
        , userName :: B.ByteString
        , password :: B.ByteString
        , port :: Int
        }
  deriving (Show, GP.PrettyShow, Generic)

newtype Resources =
    Resources
        { postgresConnection :: PS.Connection
        }

connectionString :: Config -> B.ByteString
connectionString Config {..} =
    "dbname=" <>
    databaseName <>
    " user=" <> userName <> " password='" <> password <> "'"

withPostgresHandle ::
       L.LoggerHandler IO -> Config -> (Resources -> IO a) -> IO a
withPostgresHandle logger conf =
    C.bracket (initResources logger conf) closeResources

initResources :: L.LoggerHandler IO -> Config -> IO Resources
initResources _ conf = do
    let conStr = connectionString conf
    con <- PS.connectPostgreSQL conStr
    pure Resources {postgresConnection = con}

closeResources :: Resources -> IO ()
closeResources resources =
    let conn = postgresConnection resources
     in PS.close conn

resourcesToHandle :: Resources -> L.LoggerHandler IO -> Handle IO
resourcesToHandle (Resources con) logger =
    Handle
        { log = logger
        , userAuthor = IOP.userAuthor con
        , createUser = IOP.createThis @T.CreateUser con
        , deleteUser = IOP.deleteThis @T.DeleteUser con
        , getUserByToken = IOP.getUserByToken con
        , getUserByLogin = IOP.getUserByLogin con
        , addToken = IOP.addToken con
        , generateToken = IOP.generateToken
        , getAuthors = IOP.getThisPaginated @T.GetAuthors con
        , createAuthor = IOP.createThis @T.CreateAuthor con
        , editAuthor = IOP.editThis @T.EditAuthor con
        , deleteAuthor = IOP.deleteThis @T.DeleteAuthor con
        , getTags = IOP.getThisPaginated @T.GetTags con
        , createTag = IOP.createThis @T.CreateTag con
        , editTag = IOP.editThis @T.EditTag con
        , deleteTag = IOP.deleteThis @T.DeleteTag con
        , getCategories = IOP.getThisPaginated @T.GetCategories con
        , getCategoryById = IOP.getCategoryById con
        , createCategory = IOP.createThis @T.CreateCategory con
        , editCategory = IOP.editThis @T.EditCategory con
        , deleteCategory = IOP.deleteThis @T.DeleteCategory con
        , getPosts = IOP.getThisPaginated @T.GetPosts con
        , getComments = IOP.getThisPaginated @T.GetComments con
        , createComment =
              IOP.createThis @(T.WithUser T.CreateComment) con
        , deleteComment =
              IOP.deleteThis @(T.WithUser T.DeleteComment) con
        , withTransaction = IOP.withTransaction con
        , attachTagsToDraft = IOP.attachTags con HDraft
        , attachTagsToPost = IOP.attachTags con HPost
        , editDraft = IOP.editThis @(T.WithAuthor T.EditDraft) con
        , removeAllButGivenTagsDraft =
              IOP.removeAllButGivenTags con HDraft
        , removeAllButGivenTagsPost =
              IOP.removeAllButGivenTags con HPost
        , getDraftRaw =
              \l ->
                  IOP.getThis @(T.WithAuthor T.Publish) con l >=>
                  IOP.checkUnique Nothing Just T.EDraft T._dr_draftId
        , createDraft =
              IOP.createThis @(T.WithAuthor T.CreateDraft) con
        , editDraftPublish = IOP.editThis @T.EditDraftPublish con
        , editPostPublish = IOP.editThis @T.PublishEditPost con
        , createPost = IOP.createThis @T.DraftRaw con
        , getDrafts =
              IOP.getThisPaginated @(T.WithAuthor T.GetDrafts) con
        , deleteDraft =
              IOP.deleteThis @(T.WithAuthor T.DeleteDraft) con
        }
