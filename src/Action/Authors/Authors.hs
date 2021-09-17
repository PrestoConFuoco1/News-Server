module Action.Authors.Authors where

import Action.Common
import qualified Data.Text as T
import Action.Authors.Types


requestToActionAuthors :: [T.Text] -> Query -> Either ActionError ActionAuthors
requestToActionAuthors path hash = case path of
  (x:xs)
    | x == "get" -> Right $ Read GetAuthors
  [] -> Left EInvalidEndpoint


