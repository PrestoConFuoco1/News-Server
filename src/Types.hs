{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Types
    ( module Types
    , module TypesInternal
    ) where

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple.ToField as PST
import GHC.Generics
import qualified GenericPretty as GP
import Types.APIErrors as TypesInternal
import Types.APIResult as TypesInternal
import Types.Authors as TypesInternal
import Types.Category as TypesInternal
import Types.Common as TypesInternal
import Types.Draft as TypesInternal
import Types.Entity as TypesInternal
import Types.Posts as TypesInternal
import Types.Tags as TypesInternal
import Types.Users as TypesInternal

data WithUser a =
    WithUser
        { _wu_userId :: User
        , _wu_action :: a
        }
  deriving (Show, Generic)

data WithAuthor a =
    WithAuthor
        { _wa_authorId :: Int
        , _wa_action :: a
        }
  deriving (Show, Generic)

newtype Token =
    Token
        { _t_token :: T.Text
        }
  deriving (Show, Eq, PST.ToField)
  deriving GP.PrettyShow via GP.Showable Token

data WhoWhat a =
    WhoWhat
        { _ww_token :: Maybe Token
        , _ww_action :: a
        }
  deriving (Show, Eq, Generic)

