module ExecuteUtils where


import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import ExecuteTypes


{-
ok = Response NHT.ok200 . msgValue "ok"
bad = Response NHT.status400 . errValue
unauthorized = Response NHT.unauthorized401 . errValue
notFound = Response NHT.status404 . errValue
internal = Response NHT.internalServerError500 . errValue

msgValue :: T.Text -> B.ByteString -> Ae.Value
msgValue field str = Ae.object [(field, Ae.String $ E.decodeUtf8 str)]

errValue str = msgValue "errmsg" str

-}

unauthorizedMsg = "Unauthorized, use /auth" :: T.Text

invalidEndpointMsg = "Invalid endpoint" :: T.Text
internalErrorMsg = "Internal error" :: T.Text

notAnAuthorMsg = "Not an author" :: T.Text


ok :: Ae.Value -> Response
ok res = Response NHT.ok200 val
  where val = Ae.toJSON $ Result True Nothing $ Just res

errR :: T.Text -> Ae.Value
errR t = Ae.toJSON $ Result False (Just t) Nothing

bad = Response NHT.status400 . errR

unauthorized = Response NHT.unauthorized401 . errR 

notFound = Response NHT.status404 . errR

internal = Response NHT.internalServerError500 . errR




