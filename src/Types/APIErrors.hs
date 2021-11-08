module Types.APIErrors
    ( module Types.APIErrors
    ) where

import qualified Control.Monad.Catch as CMC
import Data.Text

data ForeignViolation =
    ForeignViolation
        { fvField :: Text
        , fvValue :: Text
        }
  deriving (Show)

data UniqueViolation =
    UniqueViolation
        { uvField :: Text
        , uvValue :: Text
        }
  deriving (Show)

data ConstraintViolation =
    ConstraintViolation
        { cvField :: Text
        , cvValue :: Text
        , cvDescription :: Text
        }
  deriving (Show)

data ModifyError
    = MAlreadyInUse UniqueViolation
    | MInvalidForeign ForeignViolation
    | MConstraintViolated ConstraintViolation
    | MNoAction
  deriving (Show)

data DeleteError =
    DeleteNoAction
  deriving (Show)

newtype TagsError =
    TagsAttachError ForeignViolation
  deriving (Show)

data DraftModifyError
    = DraftModifyError ModifyError
    | DraftTagsError TagsError
  deriving (Show)

instance CMC.Exception DraftModifyError
