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
executeAction (WhoWhat y (AGetPosts x)) = getThis postDummy (WhoWhat y x)
executeAction (WhoWhat y (AGetCategories x)) = getThis catDummy (WhoWhat y x)
executeAction (WhoWhat y (AGetAuthors x)) = getThis authorDummy (WhoWhat y x)
executeAction (WhoWhat y (AGetTags x)) = getThis tagDummy (WhoWhat y x)
--executeAction (WhoWhat y (ACreateCategory x)) = createCategory (WhoWhat y x)
executeAction (WhoWhat y (ACreateCategory x)) =
    withAuth y (\u -> withAdmin u $ createCategory x)
executeAction (WhoWhat y (ACreateUser x)) = createUser (WhoWhat y x)
executeAction (WhoWhat y (AError x)) = handleError x

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

getThis :: (FromSQL s, MonadServer m) => s -> WhoWhat (Get s) -> m Response
getThis x (WhoWhat token g) = do
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


withAuth :: (MonadServer m) => Maybe Token -> (Ty.User -> m Response) -> m Response
withAuth mtoken m = case mtoken of
    Nothing -> return $ unauthorized unauthorizedMsg
    Just token -> do
        users <- getUserByToken token
        case users of
            [] -> return $ unauthorized unauthorizedMsg
            [u] -> m u -- ok
            lst -> undefined

unauthorizedMsg = "Unauthorized, use /auth"
invalidEndpointMsg = "Invalid endpoint"

withAdmin :: (MonadServer m) => Ty.User -> m Response -> m Response
withAdmin user m
  | Ty._u_admin user == Just True = m
  | otherwise = return $ notFound invalidEndpointMsg

getUserByToken :: (MonadServer m) => Token -> m [Ty.User]
getUserByToken token = do
    let str = "SELECT u.user_id, u.firstname, u.lastname, \
              \u.image, u.login, u.pass_hash, u.creation_date, u.is_admin \
              \FROM news.token t JOIN news.users u ON t.user_id = u.user_id WHERE t.token = ?"
    users <- query str [token]
    return users


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
                        "SqlError: " <> PS.sqlErrorMsg e 
                return $ Response NHT.status400 $ "Category with such name already exists."
            | foreignKeyViolated e = do
                logError $ E.decodeUtf8 $
                    "Failed to created new category, parent category id is invalid"
                return $ bad "Failed to created new category, parent category id is invalid"
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")



createUser :: (MonadServer m) => WhoWhat CreateUser -> m Response
createUser (WhoWhat token CreateUser{..}) =
    let str = "INSERT INTO news.users (firstname, lastname, login, pass_hash) VALUES (?, ?, ?, ?)"
        
    --in  logDebug "create user" >> undefined
    in  (execute str (_cu_firstName, _cu_lastName, _cu_login, _cu_passHash) >> return  (ok "User successfully created"))
            `CMC.catches` [CMC.Handler sqlH]
  where sqlH :: (MonadServer m) => PS.SqlError -> m Response
        sqlH e
            | uniqueConstraintViolated e = do
                logError $ E.decodeUtf8 $
                        "Failed to create new user, login is already in use\n\
                        \login = " <> E.encodeUtf8 _cu_login <> 
                        "SqlError: " <> PS.sqlErrorMsg e 
                return $ bad "User with such login already exists."
            | otherwise = logError (T.pack $ displayException e)
                >> return (Response NHT.internalServerError500 $ msgValue "Internal error")

   




