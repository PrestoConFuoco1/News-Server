module DatabaseHandler where




import qualified Database.PostgreSQL.Simple as PS



--query :: (PS.ToRow q, PS.FromRow r) => PS.Query -> q -> m [r]
data Handle = Handle {
    conn :: PS.Connection
    }
