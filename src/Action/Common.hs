{-# LANGUAGE
             GeneralizedNewtypeDeriving  #-}
module Action.Common (
Query, ActionError(..), CRUD(..), ActionErrorPerms(..), ask,
RoutingEnv(..), Router(..), askAdmin, askHash, routerError,
runRouter, withMaybe, errorOnNothing, renv
) where


import qualified Data.ByteString as BS
import GHC.Generics
import qualified GenericPretty as GP
import qualified Data.HashMap.Strict as HS (HashMap, fromList, lookup)
import Control.Monad.Reader
import Data.Bifunctor (first)
--import Control.Applicativ

type Query = HS.HashMap BS.ByteString BS.ByteString

data ActionError = EInvalidEndpoint
    | ERequiredFieldMissing BS.ByteString
    | EInvalidFieldValue BS.ByteString
    -- here Bool is needed because we need to know whether 
    -- the user can access this endpoint or not
    deriving (Show, Generic)

data CRUD c r u d = Create c | Read r | Update u | Delete d
    deriving (Generic, Show)
instance (GP.PrettyShow c, GP.PrettyShow r,
          GP.PrettyShow u, GP.PrettyShow d) => GP.PrettyShow (CRUD c r u d)

-- invalidEP = AError EInvalidEndpoint

data ActionErrorPerms = ActionErrorPerms {
    _ae_admin :: Bool,
    _ae_error :: ActionError
    } deriving (Show, Generic)

data RoutingEnv = RoutingEnv {
    _re_admin :: Bool,
    _re_hash :: Query
    } deriving (Show, Generic)

instance GP.PrettyShow ActionError where
    prettyShow = GP.LStr . show

instance GP.PrettyShow ActionErrorPerms where
    prettyShow = GP.LStr . show



newtype Router a = Router { unR :: ReaderT RoutingEnv (Either ActionErrorPerms) a }
    deriving (Functor, Applicative, Monad, MonadReader RoutingEnv)

askAdmin :: Router Bool
askAdmin = fmap _re_admin ask

askHash :: Router Query
askHash = fmap _re_hash ask

routerError :: ActionError -> Router a
routerError err = askAdmin >>= \admin ->
    Router $ ReaderT $ \e -> Left $ ActionErrorPerms admin err

runRouter :: RoutingEnv -> Router a -> Either ActionErrorPerms a
runRouter e rt = runReaderT (unR rt) e

withMaybe :: Router a -> Maybe a -> Router a
withMaybe r = maybe r return

errorOnNothing :: ActionError -> Maybe a -> Router a
errorOnNothing e = maybe (routerError e) return

renv b hash = RoutingEnv b hash
