module Types.APIErrors where

import Data.Text


data ModifyError =
      AlreadyInUse Text
    | InvalidForeign Text
    | NotFound


