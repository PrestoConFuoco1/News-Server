module ActWithOne where


import qualified Data.ByteString as B


import Action.Types (WhoWhat (..), Token)
import Database.Delete (DeleteSQL(..))
import Database.Update (UpdateSQL(..))

import MonadTypes (MonadServer (..), logError, logDebug, execute, query, formatQuery, logInfo, logWarn, logFatal)
import qualified Data.Aeson as Ae (toJSON)
import qualified Data.Text.Encoding as E (decodeUtf8, encodeUtf8)
import Execute.Types (Response)
import Execute.Utils (ok, bad, internal)
import qualified Exceptions as Ex

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

   

actWithOne :: (ActWithOne s, MonadServer m) => s -> Int -> m ()
actWithOne s n
  | n == 1 = logInfo (E.decodeUtf8 $ success s) -- >> return (ok $ Ae.toJSON $ E.decodeUtf8 $ success s)
  | n == 0 = logWarn (E.decodeUtf8 $ nothingFound s) 
 --           >> return (bad $ E.decodeUtf8 $ nothingFound s)
            >> Ex.invalidUpdDel (E.decodeUtf8 $ nothingFound s)
  | n > 1 = logError (E.decodeUtf8 $ tooMuchFound s) >> Ex.throwDefault
  | n < 0 = logFatal "program is working incorrectly, number of affected rows in database is negative"
        >> Ex.throwDefault


