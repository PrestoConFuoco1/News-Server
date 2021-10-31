module Types.APIErrors
    ( module Types.APIErrors
    ) where

import qualified Control.Monad.Catch as CMC
import Data.Text

data ForeignViolation =
    ForeignViolation
        { _fv_field :: Text
        , _fv_value :: Text
        }
  deriving (Show)

data UniqueViolation =
    UniqueViolation
        { _uv_field :: Text
        , _uv_value :: Text
        }
  deriving (Show)

data ConstraintViolation =
    ConstraintViolation
        { _cv_field :: Text
        , _cv_value :: Text
        , _cv_description :: Text
        }
  deriving (Show)

data ModifyError
    = MAlreadyInUse UniqueViolation
    | MInvalidForeign ForeignViolation
    | MConstraintViolated ConstraintViolation
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
