module Types.APIErrors where

import Data.Text
import qualified Control.Monad.Catch as CMC

data ForeignViolation = ForeignViolation Text Text
    deriving (Show)
data UniqueViolation = UniqueViolation Text Text
    deriving (Show)

data ModifyError =
      MAlreadyInUse UniqueViolation
    | MInvalidForeign ForeignViolation
    | MNoAction
    deriving (Show)

data DeleteError =
      DNoAction
    deriving (Show)

data TagsError = TagsAttachError ForeignViolation
    deriving (Show)

data DraftModifyError = DModifyError ModifyError
                      | DTagsError TagsError
    deriving (Show)

instance CMC.Exception DraftModifyError