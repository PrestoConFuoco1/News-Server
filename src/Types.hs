{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Types
    ( module Types
    , module TypesInternal
    ) where

import qualified Data.Text as Text
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
        { wuUserId :: User
        , wuAction :: a
        }
  deriving (Show, Generic)

data WithAuthor a =
    WithAuthor
        { waAuthorId :: Int
        , waAction :: a
        }
  deriving (Show, Generic)

newtype Token =
    Token
        { tToken :: Text.Text
        }
  deriving (Show, Eq, PST.ToField)
  deriving GP.PrettyShow via GP.Showable Token

data WhoWhat a =
    WhoWhat
        { wwToken :: Maybe Token
        , wwAction :: a
        }
  deriving (Show, Eq, Generic)

