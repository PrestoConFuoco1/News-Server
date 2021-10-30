module Types.APIErrors where

import qualified Control.Monad.Catch as CMC
import Data.Text

data ForeignViolation =
    ForeignViolation Text Text
  deriving (Show)

data UniqueViolation =
    UniqueViolation Text Text
  deriving (Show)

data ModifyError
    = MAlreadyInUse UniqueViolation
    | MInvalidForeign ForeignViolation
    | MNoAction
  deriving (Show)

data DeleteError =
    DNoAction
  deriving (Show)

newtype TagsError =
    TagsAttachError ForeignViolation
  deriving (Show)

data DraftModifyError
    = DModifyError ModifyError
    | DTagsError TagsError
  deriving (Show)

instance CMC.Exception DraftModifyError
