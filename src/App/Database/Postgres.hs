{-# LANGUAGE RecordWildCards, DeriveAnyClass, TypeApplications #-}

module App.Database.Postgres
    ( Config(..)
    , withPostgresHandle
    , Resources(..)
    , resourcesToHandle
    ) where

import App.Database (Handle(..))
import qualified App.Logger as L
import Control.Monad ((>=>))
import qualified Control.Monad.Catch as C (bracket)
import qualified Data.ByteString as B
import Database
import qualified Database.PostgreSQL.Simple as PS
import GHC.Generics
import qualified GenericPretty as GP
import qualified IO.Postgres as IOP
import qualified Types as Y

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
        , createUser = IOP.createThis @Y.CreateUser con
        , deleteUser = IOP.deleteThis @Y.DeleteUser con
        , getUserByToken = IOP.getUserByToken con
        , getUserByLogin = IOP.getUserByLogin con
        , addToken = IOP.addToken con
        , generateToken = IOP.generateToken
        , getAuthors = IOP.getThisPaginated @Y.GetAuthors con
        , createAuthor = IOP.createThis @Y.CreateAuthor con
        , editAuthor = IOP.editThis @Y.EditAuthor con
        , deleteAuthor = IOP.deleteThis @Y.DeleteAuthor con
        , getTags = IOP.getThisPaginated @Y.GetTags con
        , createTag = IOP.createThis @Y.CreateTag con
        , editTag = IOP.editThis @Y.EditTag con
        , deleteTag = IOP.deleteThis @Y.DeleteTag con
        , getCategories = IOP.getThisPaginated @Y.GetCategories con
        , getCategoryById = IOP.getCategoryById con
        , createCategory = IOP.createThis @Y.CreateCategory con
        , editCategory = IOP.editThis @Y.EditCategory con
        , deleteCategory = IOP.deleteThis @Y.DeleteCategory con
        , getPosts = IOP.getThisPaginated @Y.GetPosts con
        , getComments = IOP.getThisPaginated @Y.GetComments con
        , createComment =
              IOP.createThis @(Y.WithUser Y.CreateComment) con
        , deleteComment =
              IOP.deleteThis @(Y.WithUser Y.DeleteComment) con
        , withTransaction = IOP.withTransaction con
        , attachTagsToDraft = IOP.attachTags con HDraft
        , attachTagsToPost = IOP.attachTags con HPost
        , editDraft = IOP.editThis @(Y.WithAuthor Y.EditDraft) con
        , removeAllButGivenTagsDraft =
              IOP.removeAllButGivenTags con HDraft
        , removeAllButGivenTagsPost =
              IOP.removeAllButGivenTags con HPost
        , getDraftRaw =
              \l ->
                  IOP.getThis @(Y.WithAuthor Y.Publish) con l >=>
                  IOP.checkUnique Nothing Just Y.EDraft Y._dr_draftId
        , createDraft =
              IOP.createThis @(Y.WithAuthor Y.CreateDraft) con
        , editDraftPublish = IOP.editThis @Y.EditDraftPublish con
        , editPostPublish = IOP.editThis @Y.PublishEditPost con
        , createPost = IOP.createThis @Y.DraftRaw con
        , getDrafts =
              IOP.getThisPaginated @(Y.WithAuthor Y.GetDrafts) con
        , deleteDraft =
              IOP.deleteThis @(Y.WithAuthor Y.DeleteDraft) con
        }
