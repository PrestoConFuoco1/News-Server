module MonadNews where

import MonadTypes
import Execute.Database
import Types
import Database


class (Monad m, MonadLog m) => MonadNews m where
    getAuthors :: Paginated GetAuthors -> m [Author]
    createAuthor :: CreateAuthor -> m Int
    editAuthor :: EditAuthor -> m Int
    deleteAuthor :: DeleteAuthor -> m [Int]

    getTags :: Paginated GetTags -> m [Tag]
    createTag :: CreateTag -> m Int
    editTag :: EditTag -> m Int
    deleteTag :: DeleteTag -> m [Int]

    getCategories :: Paginated GetCategories -> m [Category]
    createCategory :: CreateCategory -> m Int
    editCategory :: EditCategory -> m Int
    deleteCategory :: DeleteCategory -> m [Int]

    createUser :: CreateUser -> m Int
    deleteUser :: DeleteUser -> m [Int]

    getComments :: Paginated GetComments -> m [Comment]
    createComment :: WithUser CreateComment -> m Int
    deleteComment :: WithUser DeleteComment -> m [Int]

instance MonadNews ServerIO where
    getComments = getThisPaginated' commentDummy
    createComment = createThis' dummyCComment
    deleteComment = deleteThis' dummyDComment

    getAuthors = getThisPaginated' authorDummy
    createAuthor = createThis' dummyCAuthor
    editAuthor = editThis' dummyUAuthor
    deleteAuthor = deleteThis' dummyDAuthor

    getTags = getThisPaginated' tagDummy
    createTag = createThis' dummyCTag
    editTag = editThis' dummyUTag
    deleteTag = deleteThis' dummyDTag

    getCategories = getThisPaginated' catDummy
    createCategory = createThis' dummyCCat
    editCategory = editThis' dummyUCat
    deleteCategory = deleteThis' dummyDCat

    createUser = createThis' dummyCUser
    deleteUser = deleteThis' dummyDUser

{-

getThis' :: (Read s, MonadServer m) => s -> Get s -> m [MType s]
getThis' x g = do
    let (qu, pars) = selectQuery x g
    debugStr <- formatQuery qu pars
    logDebug $ T.pack $ show debugStr
    --withTimePrint $
    res <- query qu pars
    logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res



getThisPaginated' :: (Read s, MonadServer m) => s -> Paginated (Get s) -> m [MType s]
getThisPaginated' x (Paginated page size g) = do
    let (qu, pars) = selectQuery x g
        (qupag, parspag) = pageingClause page size
        qu' = qu <> qupag
        totalpars = pars ++ parspag
    debugStr <- formatQuery qu' totalpars
    logDebug $ T.pack $ show debugStr
    --withTimePrint $
    res <- query qu' totalpars
    logInfo $ "Fetched " <> showText (length res) <> " entities"
    return res

editThis' :: (MonadServer m, UpdateSQL s) => s -> Upd s -> m Int
editThis' s u = case updateParams s u of
  Nothing -> Ex.invalidUpdDel "No data to edit found, required at least one parameter"
  Just (q, vals) -> do
    let str = updateQuery s q
        params = vals ++ identifParams s u
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ids <- fmap (map PSTy.fromOnly) $ query str params
    id <- validateUnique (Ex.throwUpdNotFound $ uName s) ids
    logInfo $ "Updated " <> uName s <> " with id = " <> showText id
    return id


deleteThis' :: (MonadServer m, DeleteSQL s) => s -> Del s -> m [Int]
deleteThis' s del = do
    let (str, params) = deleteQuery s del
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    --withExceptionHandlers (Ex.defaultHandlers "deleteThis") $ do
    ids <- fmap (map PSTy.fromOnly) $ query str params
    case ids of
        [] -> logInfo $ "No " <> dName s <> " deleted"
        _  -> logInfo $ "Deleted " <> dName s <> " with id = " <> showText ids
    return ids



createThis' :: (MonadServer m, CreateSQL s) => s -> Create s -> m Int
createThis' w cres = do
    let (str, params) = createQuery w cres
    debugStr <- formatQuery str params
    logDebug $ T.pack $ show debugStr

    ints <- fmap (map PSTy.fromOnly) $ query str params
    int <- validateUnique (Ex.throwBadInsert (cName w)) ints
    logInfo $ "Created " <> cName w <> " with id = " <> showText int
    return int
-}

