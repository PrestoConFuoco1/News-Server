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
dummyDComment

) where


import Database.Read
import Database.Create
import Database.Delete
import Database.Update
import Prelude hiding (Read)





