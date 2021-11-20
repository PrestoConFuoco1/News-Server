{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Database.Postgres.Internal
    ( module App.Database.Postgres.Internal
    ) where

import qualified App.Logger as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Database
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PSTy
import Database.SqlValue (SqlValue(..))
import qualified Exceptions as Ex
import qualified Types as T
import qualified Utils as U

generateToken :: Int -> IO String
generateToken = U.randomString

withTransaction :: PS.Connection -> IO a -> IO a
withTransaction = PS.withTransaction

userAuthor ::
       PS.Connection
    -> L.LoggerHandler IO
    -> T.User
    -> IO (Maybe T.Author)
userAuthor con logger u = do
    let getauthors = T.GetAuthors $ Just $ T.userId u
    as <- getThis @T.GetAuthors con logger getauthors
    case as of
        [] -> pure Nothing
        [a] -> pure $ Just a
        _ -> Ex.throwInvalidUnique T.EAuthor (map T.aAuthorId as)

getUserByToken ::
       PS.Connection
    -> L.LoggerHandler IO
    -> T.Token
    -> IO (Maybe T.User)
getUserByToken con logger token = do
    users <- getThis @T.Token con logger token
    case users of
        [] -> pure Nothing
        [u] -> pure $ Just u
        _ -> Ex.throwTokenShared $ map T.userId users

getUserByLogin ::
       PS.Connection
    -> L.LoggerHandler IO
    -> Text.Text
    -> IO (Maybe T.User)
getUserByLogin con logger login = do
    let str =
            "SELECT user_id, firstname, lastname, \
              \image, login, pass_hash, creation_date, is_admin \
              \FROM news.users WHERE login = ?"
        params = [login]
    Ex.withExceptionHandlers (Ex.sqlHandlers logger str params) $ do
        users <- PS.query con str params
        case users of
            [] -> pure Nothing
            [x] -> pure $ Just x
            _ -> Ex.throwInvalidUnique T.EUser $ map T.userId users

addToken ::
       PS.Connection
    -> L.LoggerHandler IO
    -> T.UserId
    -> Text.Text
    -> IO Text.Text
addToken con logger uid token = do
    let str =
            "INSERT INTO news.token (user_id, token) VALUES (?, ?) ON CONFLICT (user_id) DO UPDATE SET token = ?"
        token' = Text.pack (show uid) <> token
        params = [SqlValue uid, SqlValue token', SqlValue token']
    Ex.withExceptionHandlers (Ex.sqlHandlers logger str params) $ do
        _ <- PS.execute con str params
        pure token'

getCategoryById ::
       PS.Connection
    -> L.LoggerHandler IO
    -> T.CategoryId
    -> IO (Maybe T.Category)
getCategoryById con logger cid = do
    let getcats = T.GetCategories {T.gcCategoryId = Just cid}
    cats <- getThis con logger getcats
    case cats of
        [] -> pure Nothing
        [c] -> pure $ Just c
        _ ->
            Ex.throwInvalidUnique T.ECategory $ map T.cCategoryId cats

getThisPaginated ::
       (ReadSQL a)
    => PS.Connection
    -> L.LoggerHandler IO
    -> T.Paginated a
    -> IO [MType a]
getThisPaginated con logger (T.Paginated page size g) = do
    let (qu, pars) = selectQuery g
        (qupag, parspag) = pageingClause page size
        qu' = qu <> qupag
        totalPars = pars ++ parspag
    Ex.withExceptionHandlers
        (Ex.sqlHandlers logger qu' totalPars)
        (PS.query con qu' totalPars)

getThis ::
       (ReadSQL a)
    => PS.Connection
    -> L.LoggerHandler IO
    -> a
    -> IO [MType a]
getThis con logger g = do
    let (qu, pars) = selectQuery g
    Ex.withExceptionHandlers
        (Ex.sqlHandlers logger qu pars)
        (PS.query con qu pars)

editThis ::
       forall a. (UpdateSQL a)
    => PS.Connection
    -> L.LoggerHandler IO
    -> a
    -> IO (Either T.ModifyError Int)
editThis con logger u =
    case updateParams u of
        Nothing -> Ex.throwInvalidUpdate "no properties to update"
        Just (q, vals) -> do
            let str = updateQuery @a q
                params = vals ++ identifParams u
                modifyHandler = fmap Left . Ex.modifyErrorHandler
            Ex.withExceptionHandlers
                (Ex.sqlHandlers logger str params) $
                Ex.withHandler modifyHandler $ do
                    ids <-
                        map PSTy.fromOnly <$> PS.query con str params
                    case ids of
                        [] -> pure (Left T.MNoAction)
                        [x] -> pure (Right x)
                        _ -> Ex.throwInvalidUnique (uName @a) ids

createThis ::
       forall a. (CreateSQL a)
    => PS.Connection
    -> L.LoggerHandler IO
    -> a
    -> IO (Either T.ModifyError Int)
createThis con logger cres = do
    let (str, params) = createQuery cres
        modifyHandler = fmap Left . Ex.modifyErrorHandler
    Ex.withExceptionHandlers (Ex.sqlHandlers logger str params) $
        Ex.withHandler modifyHandler $ do
            ints <- map PSTy.fromOnly <$> PS.query con str params
            case ints of
                [] -> pure $ Left T.MNoAction
                [x] -> pure $ Right x
                _ -> Ex.throwInvalidUnique (cName @a) ints

deleteThis ::
       forall a. (DeleteSQL a)
    => PS.Connection
    -> L.LoggerHandler IO
    -> a
    -> IO (Either T.DeleteError Int)
deleteThis con logger del = do
    let (str, params) = deleteQuery del
    Ex.withExceptionHandlers (Ex.sqlHandlers logger str params) $ do
        ids <- map PSTy.fromOnly <$> PS.query con str params
        case ids of
            [] -> pure $ Left T.DeleteNoAction
            [eid] -> pure $ Right eid
            _ -> Ex.throwInvalidUnique (dName @a) ids

checkUnique :: a -> (b -> a) -> T.Entity -> (b -> Int) -> [b] -> IO a
checkUnique empty one entity getId xs =
    case xs of
        [] -> pure empty
        [x] -> pure $ one x
        _ -> Ex.throwInvalidUnique entity (map getId xs)

attachTags ::
       (HasTags s)
    => PS.Connection
    -> s
    -> L.LoggerHandler IO
    -> HIdent s
    -> [Int]
    -> IO (Either T.TagsError [Int])
attachTags _ _ _ _ [] = do
    pure (Right [])
attachTags con s logger hasTagsId tags = do
    let strChunks =
            ["INSERT INTO news.", "_tag (", "_id, tag_id) VALUES "]
        returningChunks =
            [ "ON CONFLICT ON CONSTRAINT "
            , "_tag_"
            , "_id_tag_id_key DO NOTHING RETURNING tag_id"
            ]
        count = length tags
        insertUnit = " ( ?, ? ) "
        insertUnits =
            fromMaybe "" $ intercalateQ $ replicate count insertUnit :: PS.Query
        insertParams = map SqlValue $ foldr f [] tags :: [SqlValue]
        f x acc = hToInt s hasTagsId : x : acc
        str = fromMaybe "" $ intercalateWith (hName s) strChunks
        returning =
            fromMaybe "" $ intercalateWith (hName s) returningChunks
        qu = str <> insertUnits <> returning :: PS.Query
        modifyHandler = fmap Left . Ex.tagsErrorHandler
    Ex.withExceptionHandlers (Ex.sqlHandlers logger qu insertParams) $
        Ex.withHandler modifyHandler $ do
            ids <- map PSTy.fromOnly <$> PS.query con qu insertParams
            pure $ Right ids

removeAllButGivenTags ::
       (HasTags s)
    => PS.Connection
    -> s
    -> L.LoggerHandler IO
    -> HIdent s
    -> [Int]
    -> IO [Int]
removeAllButGivenTags con s logger hasTagsId tags = do
    let (inClause, inParams) =
            case tags of
                [] -> ("", [])
                ts -> (" AND NOT tag_id IN ? ", [SqlValue $ PS.In ts])
        strChunks =
            [ "DELETE FROM news."
            , "_tag WHERE "
            , "_id = ? " <> inClause <> "RETURNING tag_id"
            ]
        params = SqlValue (hToInt s hasTagsId) : inParams
        str = fromMaybe "" $ intercalateWith (hName s) strChunks
    Ex.withExceptionHandlers
        (Ex.sqlHandlers logger str params)
        (map PSTy.fromOnly <$> PS.query con str params)
