{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Action.Common
    ( Router
    , Query
    , errorOnNothing
    , ActionError(..)
    , askHash
    , ActionErrorPerms(..)
    , runRouter
    , routingEnv
    , pathNotFound
    , withMaybe
    ) where

import Control.Monad.Reader (MonadReader, ReaderT(..), asks)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HS (HashMap)
import GHC.Generics
import qualified GenericPretty as GP

type Query = HS.HashMap BS.ByteString BS.ByteString

data ActionError
    = EInvalidEndpoint
    | ERequiredFieldMissing BS.ByteString
    | EInvalidFieldValue BS.ByteString
  deriving (Show, Generic, Eq)
  deriving GP.PrettyShow via GP.Showable ActionError

pathNotFound :: ActionErrorPerms
pathNotFound =
    ActionErrorPerms {aeAdmin = False, aeError = EInvalidEndpoint}

data ActionErrorPerms =
    ActionErrorPerms
        { aeAdmin :: Bool
        , aeError :: ActionError
        }
  deriving (Show, Generic, Eq)
  deriving GP.PrettyShow via GP.Showable ActionErrorPerms

data RoutingEnv =
    RoutingEnv
        { reAdmin :: Bool
        , reHash :: Query
        }
  deriving (Show, Generic)

newtype Router a =
    Router
        { unR :: ReaderT RoutingEnv (Either ActionErrorPerms) a
        }
  deriving (Functor, Applicative, Monad, MonadReader RoutingEnv)

askAdmin :: Router Bool
askAdmin = asks reAdmin

askHash :: Router Query
askHash = asks reHash

routerError :: ActionError -> Router a
routerError err =
    askAdmin >>= \admin ->
        Router $ ReaderT $ \_ -> Left $ ActionErrorPerms admin err

runRouter :: RoutingEnv -> Router a -> Either ActionErrorPerms a
runRouter e rt = runReaderT (unR rt) e

withMaybe :: Router a -> Maybe a -> Router a
withMaybe r = maybe r pure

errorOnNothing :: ActionError -> Maybe a -> Router a
errorOnNothing e = maybe (routerError e) pure

routingEnv :: Bool -> Query -> RoutingEnv
routingEnv = RoutingEnv
