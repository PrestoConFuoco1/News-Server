module IO.Postgres where

import qualified App.Logger as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue
import qualified Exceptions as Ex
import Prelude hiding (Read)
import Types
import qualified Utils as U

generateToken1 :: Int -> IO String
generateToken1 = U.randomString'

withTransaction1 :: PS.Connection -> IO a -> IO a
withTransaction1 = PS.withTransaction

userAuthor1 ::
      PS.Connection
   -> L.Handle IO
   -> User
   -> IO (Maybe Author)
userAuthor1 con logger u = do
   as <-
      getThis
         con
         authorDummy
         logger
         (GetAuthors $ Just $ _u_id u)
   case as of
      [] -> pure Nothing
      [a] -> pure $ Just a
      _ ->
         Ex.throwInvalidUnique EAuthor (map _a_authorId as)

getUserByToken1 ::
      PS.Connection
   -> L.Handle IO
   -> Token
   -> IO (Maybe User)
getUserByToken1 con logger token = do
   users <- getThis con userTokenDummy logger token
   case users of
      [] -> pure Nothing
      [u] -> pure $ Just u
      _ -> Ex.throwTokenShared $ map _u_id users

getUserByLogin1 ::
      PS.Connection
   -> L.Handle IO
   -> T.Text
   -> IO (Maybe User)
getUserByLogin1 con logger login = do
   let str =
          "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.users WHERE login = ?"
       params = [login]
   Ex.withExceptionHandlers
      (Ex.sqlHandlers logger str params) $ do
      users <- PS.query con str params
      case users of
         [] -> pure Nothing
         [x] -> pure $ Just x
         _ -> Ex.throwInvalidUnique EUser $ map _u_id users

addToken1 ::
      PS.Connection
   -> L.Handle IO
   -> UserId
   -> T.Text
   -> IO T.Text
addToken1 con logger uid token = do
   let str =
          "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
       token' = T.pack (show uid) <> token
       params =
          [SqlValue uid, SqlValue token', SqlValue token']
   Ex.withExceptionHandlers
      (Ex.sqlHandlers logger str params) $ do
      _ <- PS.execute con str params
      pure token'

getThisPaginated ::
      (Read s)
   => PS.Connection
   -> s
   -> L.Handle IO
   -> Paginated (Get s)
   -> IO [MType s]
getThisPaginated con s logger (Paginated page size g) = do
   let (qu, pars) = selectQuery s g
       (qupag, parspag) = pageingClause page size
       qu' = qu <> qupag
       totalPars = pars ++ parspag
   Ex.withExceptionHandlers
      (Ex.sqlHandlers logger qu' totalPars) $ do
      res <- PS.query con qu' totalPars
      pure res

getThis ::
      (Read s)
   => PS.Connection
   -> s
   -> L.Handle IO
   -> Get s
   -> IO [MType s]
getThis con s logger g = do
   let (qu, pars) = selectQuery s g
   Ex.withExceptionHandlers (Ex.sqlHandlers logger qu pars) $ do
      res <- PS.query con qu pars
      pure res

editThis ::
      (UpdateSQL s)
   => PS.Connection
   -> s
   -> L.Handle IO
   -> Upd s
   -> IO (Either ModifyError Int)
editThis con s logger u =
   case updateParams s u of
      Nothing -> Ex.throwInvalidUpdate
      Just (q, vals) -> do
         let str = updateQuery s q
             params = vals ++ identifParams s u
             modifyHandler =
                fmap Left . Ex.modifyErrorHandler
         Ex.withExceptionHandlers
            (Ex.sqlHandlers logger str params) $
            Ex.withHandler modifyHandler $ do
               ids <-
                  map PSTy.fromOnly <$>
                  PS.query con str params
               case ids of
                  [] -> pure (Left MNoAction)
                  [x] -> pure (Right x)
                  _ -> Ex.throwInvalidUnique (uName s) ids

createThis ::
      (CreateSQL s)
   => PS.Connection
   -> s
   -> L.Handle IO
   -> Create s
   -> IO (Either ModifyError Int)
createThis con s logger cres = do
   let (str, params) = createQuery s cres
       modifyHandler = fmap Left . Ex.modifyErrorHandler
   Ex.withExceptionHandlers
      (Ex.sqlHandlers logger str params) $
      Ex.withHandler modifyHandler $ do
         ints <-
            map PSTy.fromOnly <$> PS.query con str params
         case ints of
            [] -> pure $ Left MNoAction
            [x] -> pure $ Right x
            _ -> Ex.throwInvalidUnique (cName s) ints

deleteThis ::
      (DeleteSQL s)
   => PS.Connection
   -> s
   -> L.Handle IO
   -> Del s
   -> IO (Either DeleteError Int)
deleteThis con s logger del = do
   let (str, params) = deleteQuery s del
   Ex.withExceptionHandlers
      (Ex.sqlHandlers logger str params) $ do
      ids <- map PSTy.fromOnly <$> PS.query con str params
      case ids of
         [] -> pure $ Left DNoAction
         [eid] -> pure $ Right eid
         _ -> Ex.throwInvalidUnique (dName s) ids

checkUnique ::
      a -> (b -> a) -> Entity -> (b -> Int) -> [b] -> IO a
checkUnique empty one entity getId xs =
   case xs of
      [] -> pure empty
      [x] -> pure $ one x
      _ -> Ex.throwInvalidUnique entity (map getId xs)

attachTags ::
      (HasTags s)
   => PS.Connection
   -> s
   -> L.Handle IO
   -> HIdent s
   -> [Int]
   -> IO (Either TagsError [Int])
attachTags _ _ _ _ []
 = do
   pure (Right [])
attachTags con s logger hasTagsId tags = do
   let strChunks =
          [ "INSERT INTO news."
          , "_tag ("
          , "_id, tag_id) VALUES "
          ]
       returningChunks =
          [ "ON CONFLICT ON CONSTRAINT "
          , "_tag_"
          , "_id_tag_id_key DO NOTHING RETURNING tag_id"
          ]
       count = length tags
       insertUnit = " ( ?, ? ) "
       insertUnits =
          fromMaybe "" $
          intercalateQ $ replicate count insertUnit :: PS.Query
       insertParams =
          map SqlValue $ foldr f [] tags :: [SqlValue]
       f x acc = hToInt s hasTagsId : x : acc
       str =
          fromMaybe "" $ intercalateWith (hName s) strChunks
       returning =
          fromMaybe "" $
          intercalateWith (hName s) returningChunks
       qu = str <> insertUnits <> returning :: PS.Query
       modifyHandler = fmap Left . Ex.tagsErrorHandler
   Ex.withExceptionHandlers
      (Ex.sqlHandlers logger qu insertParams) $
      Ex.withHandler modifyHandler $ do
         ids <-
            map PSTy.fromOnly <$>
            PS.query con qu insertParams
         pure $ Right ids

removeAllButGivenTags ::
      (HasTags s)
   => PS.Connection
   -> s
   -> L.Handle IO
   -> HIdent s
   -> [Int]
   -> IO [Int]
removeAllButGivenTags con s logger hasTagsId tags = do
   let inClause [] = ""
       inClause _ = " AND NOT tag_id IN ? "
       inParams [] = []
       inParams ts = [SqlValue $ PS.In ts]
       strChunks =
          [ "DELETE FROM news."
          , "_tag WHERE "
          , "_id = ? " <>
            inClause tags <> "RETURNING tag_id"
          ]
       params =
          SqlValue (hToInt s hasTagsId) : inParams tags
       str =
          fromMaybe "" $ intercalateWith (hName s) strChunks
   Ex.withExceptionHandlers
      (Ex.sqlHandlers logger str params) $ do
      tagsDel <-
         map PSTy.fromOnly <$> PS.query con str params
      pure tagsDel
