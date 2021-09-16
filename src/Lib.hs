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

import RequestToAction
import ActionTypes
import FromSQL

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
    let whowhat@(WhoWhat maybeToken action) = requestToAction req
    logDebug "Action type is"
    logDebug $ T.pack $ GP.defaultPretty action

    val <- executeAction whowhat
    let (Response status msg) = val
    return $ W.responseLBS status [] $ Ae.encode msg

executeAction :: MonadServer m => WhoWhat Action -> m Response
executeAction (WhoWhat y (AGetPosts x)) = getThis postDummy x

executeAction (WhoWhat y (AGetAuthors x)) =
    withAuth y . withAdmin $ getThis authorDummy x

executeAction (WhoWhat y (AGetTags x)) = getThis tagDummy x
--executeAction (WhoWhat y (ACreateTag x)) = withAuth y . withAdmin $ createTag x
executeAction (WhoWhat y (ACreateTag x)) = withAuth y . withAdmin $ createThis dummyCTag x
executeAction (WhoWhat y (AEditTag x)) =  withAuth y . withAdmin $ editTag x
executeAction (WhoWhat y (ADeleteTag x)) = withAuth y . withAdmin $ deleteTag x


executeAction (WhoWhat y (AGetCategories x)) = getThis catDummy x
executeAction (WhoWhat y (ACreateCategory x)) = withAuth y . withAdmin $ createCategory x
executeAction (WhoWhat y (AEditCategory x)) =  withAuth y . withAdmin $ editCategory x
executeAction (WhoWhat y (ADeleteCategory x)) = withAuth y . withAdmin $ deleteCategory x

executeAction (WhoWhat y (ACreateUser x)) = createUser x

executeAction (WhoWhat y (AError x)) = handleError x

editTag = undefined
deleteTag = undefined
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

ok = Response NHT.ok200 . msgValue
bad = Response NHT.status400 . msgValue
unauthorized = Response NHT.unauthorized401 . msgValue
notFound = Response NHT.status404 . msgValue

msgValue :: B.ByteString -> Ae.Value
msgValue str = Ae.object [("errmsg", Ae.String $ E.decodeUtf8 str)]

--data Permissions = 

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
   

uniqueConstraintViolated e = PS.sqlState e == "23505"
foreignKeyViolated e = PS.sqlState e == "23503"


withAuth :: (MonadServer m) => Maybe Token -> (Maybe Ty.User -> m Response) -> m Response
withAuth mtoken m = case mtoken of
    Nothing -> m Nothing
    Just token -> do
        users <- getUserByToken token
        case users of
            [] -> m Nothing
            [u] -> m (Just u) -- ok
            lst -> undefined

unauthorizedMsg = "Unauthorized, use /auth"
invalidEndpointMsg = "Invalid endpoint"

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

createTag :: (MonadServer m) => CreateTag -> m Response
createTag CreateTag{..} = do

        let str = "INSERT INTO news.tag (name) VALUES (?)"
        (execute str [_ct_tagName] >> (return $ ok "Tag successfully created"))
            `CMC.catches` [CMC.Handler sqlH]

  where name = _ct_tagName
        sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                        "Failed to create new tag, there is one with such name\n\
                       \ tag_name = " <> E.encodeUtf8 name <> 
                        "\nSqlError: " <> PS.sqlErrorMsg e 
                return $ Response NHT.status400 $ "Tag with such name already exists."
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")



createCategory :: (MonadServer m) => CreateCategory -> m Response
createCategory CreateCategory{..} = do

        let str = "INSERT INTO news.category (name, parent_category_Id) VALUES (?, ?)"
        (execute str (_cc_catName, _cc_parentCat) >> (return $ ok "Category successfully created"))
            `CMC.catches` [CMC.Handler sqlH]

  where name = _cc_catName --crecat
        sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                        "Failed to create new category, there is one with such name\n\
                        \category_name = " <> E.encodeUtf8 name <> 
                        "\nSqlError: " <> PS.sqlErrorMsg e 
                return $ Response NHT.status400 $ "Category with such name already exists."
            | foreignKeyViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to created new category, parent category id is invalid"
                return $ bad "Failed to created new category, parent category id is invalid"
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")



createUser :: (MonadServer m) => CreateUser -> m Response
createUser CreateUser{..} =
    let str = "INSERT INTO news.users (firstname, lastname, login, pass_hash) VALUES (?, ?, ?, ?)"

    in  (execute str (_cu_firstName, _cu_lastName, _cu_login, _cu_passHash) >> return  (ok "User successfully created"))
            `CMC.catches` [CMC.Handler sqlH]
  where sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                        "Failed to create new user, login is already in use\n\
                        \login = " <> E.encodeUtf8 _cu_login <> 
                        "\nSqlError: " <> PS.sqlErrorMsg e 
                return $ bad "User with such login already exists."
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")

class (PS.ToRow (Create s)) => CreateSQL s where
    type Create s :: *
    createQuery :: s -> PS.Query
    cName :: s -> B.ByteString
    cUniqueField :: s -> B.ByteString -- ???
    cForeign :: s -> B.ByteString -- ?????

createThis :: (MonadServer m, CreateSQL s) => s -> Create s -> m Response
createThis s cres = do
    let str = createQuery s
    (execute str cres) >> return (ok $ cName s <> " successfully created")
        `CMC.catches` [CMC.Handler (sqlH s)]
  where sqlH :: (MonadServer m, CreateSQL s) => s -> PS.SqlError -> m Response
        sqlH s e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to create new " <> cName s <> ", " <>
                    cUniqueField s <> " is already in use\n" <>
                    "\nSqlError: " <> PS.sqlErrorMsg e
                return $ bad $ cName s <> " with such " <> cUniqueField s <> " already exists."
            | foreignKeyViolated e = do
                let errmsg = 
                     "Failed to create new " <> cName s <> ", " <> cForeign s <> " is invalid"
                logError $ E.decodeUtf8 $ errmsg
                return $ bad errmsg
            | otherwise = logError (T.pack $ displayException e) >> undefined
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")

    --query :: (PS.ToRow q, PS.FromRow r) => PS.Query -> q -> m [r]
    --execute :: (PS.ToRow q) => PS.Query -> q -> m Int
 

newtype CTag = CTag ()
dummyCTag = CTag ()

instance CreateSQL CTag where
    type Create CTag = CreateTag
    createQuery _ = "INSERT INTO news.tag (name) VALUES (?)"
    cName _ = "tag"
    cUniqueField _ = "name"
    cForeign _ = "error"



