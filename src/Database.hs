{-
module Database(
CreateSQL(..),
UpdateSQL(..),
Read(..),
DeleteSQL(..),
pageingClause,
updateParams,
authorDummy,
dummyCAuthor,
dummyUAuthor,
dummyDAuthor,
tagDummy,
dummyCTag,
dummyUTag,
dummyDTag,
catDummy,
dummyCCat,
dummyUCat,
dummyDCat,
dummyCUser,
dummyDUser,
commentDummy,
dummyCComment,
dummyDComment,
userTokenDummy,
postDummy,
draftDummy,
dummyDDraft,
draftCreateDummy,
draftEditDummy,
draftRawDummy,
draftEditPublishDummy,
dummyUPost,
dummyCPost
) where
-}
module Database (module D) where

import Database.Read as D
import Database.Create as D
import Database.Delete as D
import Database.Update as D
import Database.HasTags as D
import Prelude hiding (Read)





