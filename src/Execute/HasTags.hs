{-# LANGUAGE TypeFamilies,
             FlexibleContexts #-}
module Execute.HasTags where


import qualified Data.Text as T (pack, Text)

import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Database.PostgreSQL.Simple as PS
--import qualified GenericPretty as GP

import Database.Update

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import Execute.Types
import Execute.Utils


import Database.SqlValue
--import Execute.Actions

import Database.Update
--import ActWithOne

import qualified Exceptions as Ex

import qualified Data.Aeson as Ae (Value(..))







class (Show (HIdent s)) => HasTags s where
    type HIdent s :: *
    hToInt :: s -> HIdent s -> Int
    hName :: s -> PS.Query
    hName' :: s -> T.Text

newtype HDraft = HDraft ()
dummyHDraft = HDraft ()

instance HasTags HDraft where
    type HIdent HDraft = Int
    hToInt _ = id
    hName _ = "draft"
    hName' _ = "draft"

newtype HPost = HPost ()
dummyHPost = HPost ()

instance HasTags HPost where
    type HIdent HPost = Int
    hToInt _ = id
    hName _ = "post"
    hName' _ = "post"



attachTags :: (MonadServer m, HasTags s) => s -> HIdent s -> [Int] -> m [Int]
attachTags s hasTagsId [] = do
    logInfo $ "No tags attached to " <> hName' s <> " with id = " <> (T.pack $ show hasTagsId)
    return []
attachTags s hasTagsId tags = do
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

    
    debugStr <- formatQuery qu insertParams
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query qu insertParams
    return ids


removeAllButGivenTags :: (MonadServer m, HasTags s) => s -> HIdent s -> [Int] -> m [Int]
-- remove all but given tags
removeAllButGivenTags s hasTagsId tags = do
    let inClause [] = ""
        inClause ts = " AND NOT tag_id IN ? "
        inParams [] = []
        inParams ts = [SqlValue $ PS.In ts]
        strChunks = ["DELETE FROM news.", "_tag WHERE ", "_id = ? " <> inClause tags <> "RETURNING tag_id"]
        params = [SqlValue $ hToInt s hasTagsId] ++ inParams tags
        str = maybe "" id $ intercalateWith (hName s) strChunks
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    tagsDel <- fmap (map PSTy.fromOnly) $ query str params
    logDebug $ "Removed tags with id in " <> (T.pack $ show tagsDel) <> " from " <> hName' s <> " with id = " <> (T.pack $ show hasTagsId)
    return tagsDel


 
 
