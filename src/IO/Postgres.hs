module IO.Postgres where

import Database
import Database.Update
import Database.HasTags
import Database.SqlValue
import Prelude hiding (Read)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Exceptions as Ex
import Types
import qualified Utils as U
--import qualified App.Logger as L

generateToken1 :: Int -> IO String
generateToken1 = U.randomString'

withTransaction1 :: PS.Connection -> IO a -> IO a
withTransaction1 = PS.withTransaction

userAuthor1 :: PS.Connection -> User -> IO (Maybe Author)
userAuthor1 con u = do
    as <- getThis con authorDummy (GetAuthors $ Just $ _u_id u)
    case as of
        [] -> return Nothing
        [a] -> return $ Just a
        _ -> Ex.throwInvalidUnique EAuthor (map _a_authorId as)

getUserByToken1 :: PS.Connection -> Token -> IO (Maybe User)
getUserByToken1 con token = do
    users <- getThis con userTokenDummy token
    case users of
        [] -> return Nothing
        [u] -> return $ Just u
        _ -> Ex.throwTokenShared $ map _u_id users

getUserByLogin1 :: PS.Connection -> T.Text -> IO (Maybe User)
getUserByLogin1 con login = do
    let str = "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.users WHERE login = ?"
    users <- PS.query con str [login]
    case users of
        [] -> return Nothing
        [x] -> return $ Just x
        _ -> Ex.throwInvalidUnique EUser $ map _u_id users


addToken1 :: PS.Connection -> UserId -> T.Text -> IO T.Text
addToken1 con id token = do
    let str = "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
        token' = (T.pack $ show id) <> token
    PS.execute con str (id, token', token')
    return token'



getThisPaginated :: (Read s) => PS.Connection -> s -> Paginated (Get s) -> IO [MType s]
getThisPaginated con x (Paginated page size g) = do
    let (qu, pars) = selectQuery x g
        (qupag, parspag) = pageingClause page size
        qu' = qu <> qupag
        totalpars = pars ++ parspag
    --debugStr <- PS.formatQuery con qu' totalpars
    --logDebug $ T.pack $ show debugStr
    res <- PS.query con qu' totalpars
    --logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res

getThis :: (Read s) => PS.Connection -> s -> Get s -> IO [MType s]
getThis con x g = do
    let (qu, pars) = selectQuery x g
    --debugStr <- PS.formatQuery con qu pars
    --logDebug $ T.pack $ show debugStr
    res <- PS.query con qu pars
    --logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res



editThis :: (UpdateSQL s) => PS.Connection -> s -> Upd s -> IO (Either ModifyError Int)
editThis con s u = case updateParams s u of
  Nothing -> Ex.throwInvalidUpdate
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    --debugStr <- PS.formatQuery con str params
    --logDebug $ T.pack $ show debugStr

    Ex.withHandler (fmap Left . Ex.modifyErrorHandler) $ do
        ids <- fmap (map PSTy.fromOnly) $ PS.query con str params
        case ids of
            [] -> return (Left MNoAction)
            [x] -> return (Right x)
            _   -> Ex.throwInvalidUnique (uName s) ids

createThis :: (CreateSQL s) => PS.Connection -> s -> Create s -> IO (Either ModifyError Int)
createThis con s cres = do
    let (str, params) = createQuery s cres
    --debugStr <- PS.formatQuery con str params
    --logDebug $ T.pack $ show debugStr

    Ex.withHandler (fmap Left . Ex.modifyErrorHandler) $ do
        ints <- fmap (map PSTy.fromOnly) $ PS.query con str params
        case ints of
            [] -> return $ Left MNoAction
            [x] -> return $ Right x
            _ -> Ex.throwInvalidUnique (cName s) ints




deleteThis :: (DeleteSQL s) => PS.Connection -> s -> Del s -> IO (Either DeleteError Int)
deleteThis con s del = do
    let (str, params) = deleteQuery s del
    --debugStr <- PS.formatQuery con str params
    --logDebug $ T.pack $ show debugStr

    --withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
    ids <- fmap (map PSTy.fromOnly) $ PS.query con str params
    case ids of
        [] -> return $ Left DNoAction
        [id] -> return $ Right id
        _ -> Ex.throwInvalidUnique (dName s) ids


checkUnique :: a -> (b -> a) -> Entity -> (b -> Int) -> [b] -> IO a
checkUnique empty one entity getId xs =
    case xs of
        [] -> return empty
        [x] -> return $ one x
        _ -> Ex.throwInvalidUnique entity (map getId xs)

-------------------------------------------------------------------

attachTags :: (HasTags s) => PS.Connection -> s -> HIdent s -> [Int] -> IO (Either TagsError [Int])
attachTags con s hasTagsId [] = do
    --logInfo $ "No tags attached to " <> hName' s <> " with id = " <> (T.pack $ show hasTagsId)
    return (Right [])
attachTags con s hasTagsId tags = do
    let strChunks = ["INSERT INTO news.", "_tag (", "_id, tag_id) VALUES "]
        returningChunks = ["ON CONFLICT ON CONSTRAINT ", "_tag_", "_id_tag_id_key DO NOTHING RETURNING tag_id"]
        count = length tags
        insertUnit = " ( ?, ? ) "
        insertUnits = maybe "" id $ intercalateQ $ replicate count insertUnit :: PS.Query
        insertParams = map SqlValue $ foldr f [] tags :: [SqlValue]
        f x acc = hToInt s hasTagsId : x : acc
        str = maybe "" id $ intercalateWith (hName s) strChunks
        returning = maybe "" id $ intercalateWith (hName s) returningChunks
        qu = str <> insertUnits <> returning :: PS.Query

    
    --debugStr <- formatQuery qu insertParams
    --logDebug $ T.pack $ show debugStr

    Ex.withHandler (fmap Left . Ex.tagsErrorHandler) $ do
        ids <- fmap (map PSTy.fromOnly) $ PS.query con qu insertParams
        return $ Right ids


removeAllButGivenTags :: (HasTags s) => PS.Connection -> s -> HIdent s -> [Int] -> IO [Int]
-- remove all but given tags
removeAllButGivenTags con s hasTagsId tags = do
    let inClause [] = ""
        inClause ts = " AND NOT tag_id IN ? "
        inParams [] = []
        inParams ts = [SqlValue $ PS.In ts]
        strChunks = ["DELETE FROM news.", "_tag WHERE ", "_id = ? " <> inClause tags <> "RETURNING tag_id"]
        params = [SqlValue $ hToInt s hasTagsId] ++ inParams tags
        str = maybe "" id $ intercalateWith (hName s) strChunks
--    debugStr <- PS.formatQuery con str params
--    logDebug $ T.pack $ show debugStr

    tagsDel <- fmap (map PSTy.fromOnly) $ PS.query con str params
    --logDebug $ "Removed tags with id in " <> (T.pack $ show tagsDel) <> " from " <> hName' s <> " with id = " <> (T.pack $ show hasTagsId)
    return tagsDel




