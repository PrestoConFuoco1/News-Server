module Execute.Utils where


import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import Execute.Types (Result(..), Response(..))




unauthorizedMsg = "Unauthorized, use /auth" :: T.Text

invalidEndpointMsg = "Invalid endpoint" :: T.Text
internalErrorMsg = "Internal error" :: T.Text

notAnAuthorMsg = "Not an author" :: T.Text

okCreated :: T.Text -> Int -> Response
okCreated msg id = Response NHT.ok200 val
  where val = Ae.toJSON $ Result True (Just msg) (Just $ Ae.toJSON id)

ok :: Ae.Value -> Response
ok res = Response NHT.ok200 val
  where val = Ae.toJSON $ Result True Nothing $ Just res

errR :: T.Text -> Ae.Value
errR t = Ae.toJSON $ Result False (Just t) Nothing

bad = Response NHT.status400 . errR

unauthorized = Response NHT.unauthorized401 . errR 

notFound = Response NHT.status404 . errR

internal = Response NHT.internalServerError500 . errR




