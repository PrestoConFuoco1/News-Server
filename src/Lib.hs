{-# LANGUAGE
ScopedTypeVariables,
TypeFamilies,
FlexibleContexts,
RecordWildCards
#-}


module Lib
    ( someFunc
    ) where



import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as W
import qualified Network.HTTP.Types as NHT
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception
import qualified Data.Aeson as Ae (Value, encode)

import qualified GenericPretty as GP
import GHC.Generics

import Action.RequestToAction
import Action.Types
import Action.Common
import FromSQL
import Create
import Delete

import qualified Logger as L
import MonadTypes
import qualified Database.PostgreSQL.Simple as PS
import qualified DatabaseHandler as DB
import qualified DBTypes as DBT
import qualified Types as Ty
import qualified Data.Aeson as Ae
import Data.Proxy
import qualified Control.Monad.Catch as CMC
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)

import Action.Tags.Types

port :: Int
port = 5555

someFunc :: IO ()
someFunc = do
    conn <- PS.connectPostgreSQL "dbname='batadase'"
    let serverH = ServerHandlers L.simpleLog (DB.Handle conn)
    Warp.run port $ mainFunc1 serverH

connectToDB :: IO PS.Connection
connectToDB = PS.connectPostgreSQL "dbname='batadase'"

mainFunc1 :: ServerHandlers -> W.Application
mainFunc1 handlers req respond = do
    response <- runServer handlers $ (mainServer req :: ServerIO W.Response)
    respond response

data Response = Response {
    _r_status :: NHT.Status,
    _r_message :: Ae.Value
    }



mainServer :: MonadServer m => W.Request -> m W.Response
mainServer req = do
    logDebug $ T.pack $ show req
    --let whowhat@(WhoWhat maybeToken action) = requestToAction req
    let eithWhowhat = requestToAction req
    case eithWhowhat of
        Left err -> fmap coerceResponse $ handleError err
        Right whowhat -> do
            logDebug "Action type is"
            logDebug $ T.pack $ GP.defaultPretty $ _ww_action whowhat

            val <- executeAction whowhat
            return $ coerceResponse val
 --           let (Response status msg) = val
   --         return $ W.responseLBS status [] $ Ae.encode msg

coerceResponse :: Response -> W.Response
coerceResponse (Response status msg) =
    W.responseLBS status [] $ Ae.encode msg

--executeAction' (WhoWhat y (AError x)) = handleError x
{-
data Action = AAuthors ActionAuthors
            | ACategory ActionCategory
            | APosts ActionPosts
            | ATags  ActionTags
            | AUsers ActionUsers
-}

executeAction :: MonadServer m => WhoWhat Action -> m Response
executeAction (WhoWhat y (AAuthors x)) = executeAuthor (WhoWhat y x)
executeAction (WhoWhat y (ACategory x)) = executeCategory (WhoWhat y x)
executeAction (WhoWhat y (APosts x)) = executePosts (WhoWhat y x)
executeAction (WhoWhat y (ATags x)) = executeTags (WhoWhat y x)
executeAction (WhoWhat y (AUsers x)) = executeUsers (WhoWhat y x)

executePosts (WhoWhat y (Read x)) = getThis postDummy x

executeAuthor (WhoWhat y (Read x)) =
    withAuth y . withAdmin $ getThis authorDummy x

executeTags (WhoWhat y (Read x)) = getThis tagDummy x
executeTags (WhoWhat y (Create x)) = withAuth y . withAdmin $ createThis dummyCTag x
executeTags (WhoWhat y (Update x)) =  withAuth y . withAdmin $ editTag x
executeTags (WhoWhat y (Delete x)) = withAuth y . withAdmin $ deleteThis dummyDTag x


executeCategory (WhoWhat y (Read x)) = getThis catDummy x
executeCategory (WhoWhat y (Create x)) = withAuth y . withAdmin $ createThis dummyCCat x
executeCategory (WhoWhat y (Update x)) =  withAuth y . withAdmin $ editCategory x
executeCategory (WhoWhat y (Delete x)) = withAuth y . withAdmin $ deleteCategory x

executeUsers (WhoWhat y (Create x)) = createThis dummyCUser x




editCategory = undefined
deleteCategory = undefined

handleError :: MonadServer m => ActionError -> m Response
handleError EInvalidEndpoint = do
    logError $ "Invalid endpoint"
    return $ notFound "Invalid endpoint"
handleError (ERequiredFieldMissing x) = do
    let str =  "Required field missing (" <> x <> ")"
    logError $ E.decodeUtf8 str
    return $ bad str

ok = Response NHT.ok200 . msgValue "ok"
bad = Response NHT.status400 . errValue
unauthorized = Response NHT.unauthorized401 . errValue
notFound = Response NHT.status404 . errValue
internal = Response NHT.internalServerError500 . errValue

msgValue :: T.Text -> B.ByteString -> Ae.Value
msgValue field str = Ae.object [(field, Ae.String $ E.decodeUtf8 str)]

errValue str = msgValue "errmsg" str

unauthorizedMsg = "Unauthorized, use /auth"
invalidEndpointMsg = "Invalid endpoint"



withAuth :: (MonadServer m) => Maybe Token -> (Maybe Ty.User -> m Response) -> m Response
withAuth mtoken m = case mtoken of
    Nothing -> m Nothing
    Just token -> do
        users <- getUserByToken token
        case users of
            [] -> m Nothing
            [u] -> m (Just u) -- ok
            lst -> undefined

withAdmin :: (MonadServer m) => m Response -> Maybe Ty.User -> m Response
withAdmin m muser
  | (muser >>= Ty._u_admin) == Just True = m
  | otherwise = return $ notFound invalidEndpointMsg

getUserByToken :: (MonadServer m) => Token -> m [Ty.User]
getUserByToken token = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.token t JOIN news.users u ON t.user_id = u.user_id WHERE t.token = ?"
    users <- query str [token]
    return users


uniqueConstraintViolated e = PS.sqlState e == "23505"
foreignKeyViolated e = PS.sqlState e == "23503"



createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis w cres = do
    let str = createQuery w
    (execute str cres >> return (ok $ cName w <> " successfully created"))
        `CMC.catches` [CMC.Handler (sqlH w)]
  where sqlH :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
        sqlH s e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to create new " <> cName s <> ", " <>
                    cUniqueField s <> " is already in use\n" <>
                    "SqlError: " <> PS.sqlErrorMsg e
                return $ bad $ cName s <> " with such " <> cUniqueField s <> " already exists."
            | foreignKeyViolated e = do
                let errmsg = 
                     "Failed to create new " <> cName s <> ", " <> cForeign s <> " is invalid"
                logError $ E.decodeUtf8 $ errmsg
                return $ bad errmsg
            | otherwise = logError (T.pack $ displayException e)
                >> return (internal "Internal error")






-- добавить обработку исключений!!!
getThis :: (FromSQL s, MonadServer m) => s -> Get s -> m Response
getThis x g = do
    cat <- f x g
    logDebug $ T.pack $ GP.defaultPretty cat
    let val = Ae.toJSON cat
    return $ Response NHT.ok200 val
  where f :: (FromSQL s, MonadServer m) => s -> Get s -> m [MType s]
        f x g = do
            let qu = selectQuery x g
            debugStr <- uncurry formatQuery qu
            logDebug $ T.pack $ show debugStr
            uncurry query qu


deleteThis :: (MonadServer m, DeleteSQL s) => s -> Del s -> m Response
deleteThis s d = do
    let str = deleteQuery s
    (execute str d >> return (ok $ dName s <> " successfully deleted"))
        `CMC.catches` [CMC.Handler (sqlH s)]
  where sqlH :: (MonadServer m, DeleteSQL s) => s -> PS.SqlError -> m Response
        sqlH w e = logError (T.pack $ displayException e) >> return (internal "Internal error")
 


editTag :: (MonadServer m) => EditTag -> m Response
editTag EditTag{..} = do
    let str = "UPDATE news.tag SET name = ? WHERE tag_id = ?"
    (execute str (_et_tagName, _et_tagId) >> return (ok $ "Tag successfully edited"))
        `CMC.catches` [CMC.Handler (sqlH)]
  where sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e = logError (T.pack $ displayException e) >> return (internal "Internal error")
 
