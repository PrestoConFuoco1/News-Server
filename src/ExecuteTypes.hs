
module ExecuteTypes where


import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.ToRow as PSR

import qualified Network.HTTP.Types as NHT
import qualified Data.Aeson as Ae
import GHC.Generics

data Response = Response {
    _r_status :: NHT.Status,
    _r_message :: Ae.Value
    }


data WithUser a = WithUser {
    _wu_userId :: Int,
    _wu_action :: a
    } deriving (Show, Generic)

data WithAuthor a = WithAuthor {
    _wa_authorId :: Int,
    _wa_action :: a
    } deriving (Show, Generic)

instance PSR.ToRow a => PSR.ToRow (WithUser a) where
    toRow (WithUser int x) = PSR.toRow (PS.Only int PS.:. x)


instance PSR.ToRow a => PSR.ToRow (WithAuthor a) where
    toRow (WithAuthor int x) = PSR.toRow (PS.Only int PS.:. x)
