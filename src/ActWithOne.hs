module ActWithOne where





import qualified Network.HTTP.Types as NHT
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception

import qualified GenericPretty as GP
import GHC.Generics

import Action.RequestToAction
import Action.Types (WhoWhat (..), Token)
import Action.Common
import Database.Read
import Database.Create
import Database.Delete
import Database.Update

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Database.PostgreSQL.Simple as PS (SqlError(..))
import qualified Types as Ty
import qualified Data.Aeson as Ae
import qualified Control.Monad.Catch as CMC (catches, Handler(..), MonadCatch)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Execute.Types
import Execute.Utils


class ActWithOne s where
    success :: s -> B.ByteString
    nothingFound :: s -> B.ByteString
    tooMuchFound :: s -> B.ByteString

newtype AWOu s = AWOu s
newtype AWOd s = AWOd s

instance (UpdateSQL s) => ActWithOne (AWOu s) where
    success (AWOu s) = uName s <> " successfully edited"
    nothingFound (AWOu s) = "No " <> uName s <> " found."
    tooMuchFound (AWOu s) = "Too much " <> uName s <> "found"
 
instance (DeleteSQL s) => ActWithOne (AWOd s) where
    success (AWOd s) = dName s <> " successfully deleted"
    nothingFound (AWOd s) = "No " <> dName s <> " found."
    tooMuchFound (AWOd s) = "Too much " <> dName s <> "deleted"
    
   
   
actWithOne :: (ActWithOne s, MonadServer m) => s -> Int -> m Response
actWithOne s n
  | n == 1 = logInfo (E.decodeUtf8 $ success s) >> return (ok $ Ae.toJSON $ E.decodeUtf8 $ success s)
  | n == 0 = logWarn (E.decodeUtf8 $ nothingFound s) >> return (bad $ E.decodeUtf8 $ nothingFound s)
  | n > 1 = logError (E.decodeUtf8 $ tooMuchFound s) >> return (internal "Internal error")
  | n < 0 = logFatal "program is working incorrectly, number of affected rows in database is negative"
        >> return (internal "Internal error")


