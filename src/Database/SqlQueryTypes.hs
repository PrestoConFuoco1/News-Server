{-# LANGUAGE ExistentialQuantification,
             GeneralizedNewtypeDeriving,
             BangPatterns #-}
module SqlQueryTypes where

import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.ToField as PSF
import qualified Database.PostgreSQL.Simple.Types as PSTy
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.String as S
import SqlValue

data Where =   Or !Where !Where
            | And !Where !Where
            | Unit !WhereUnit
            | WTrue
    deriving (Show, Eq)

data WhereUnit  = CompareNum !CmpOperator !Field !Int
                | CompareDate !CmpOperator !Field !Time.Day
                | CompareArr !ArrOperator !Arr !Field
                | ILike Field !T.Text
    deriving (Show, Eq)

data SqlQuery = SqlQuery {
    _sql_query :: !PS.Query,
    _sql_params :: ![SqlValue]
    }

func :: PS.Query -> Where -> Where -> SqlQuery
func op a b = 
    let sqa = whereToQuery a
        sqb = whereToQuery b
    in  SqlQuery
            (mconcat ["(", _sql_query sqa, op, _sql_query sqb, ")"])
            (_sql_params sqa ++ _sql_params sqb)
 
whereToQuery :: Where -> SqlQuery
whereToQuery (Or a b) = func ") OR (" a b
whereToQuery (And a b) = func ") AND (" a b
whereToQuery (Unit unit) = whereUnitToQuery unit
whereToQuery WTrue = SqlQuery "true" []
{-
-}
whereUnitToQuery :: WhereUnit -> SqlQuery
whereUnitToQuery (CompareNum op field int) =
    SqlQuery (mconcat [unField field, " ", cmpOperator op, " ? "]) [SqlValue int]

whereUnitToQuery (CompareDate op field day) = 
    SqlQuery (mconcat [unField field, " ", cmpOperator op, " ? "]) [SqlValue day]

whereUnitToQuery (CompareArr op arr field) =
    SqlQuery (mconcat [" ? ", arrOperator op, " ", unField field]) [SqlValue arr]

whereUnitToQuery (ILike field text) = 
    SqlQuery (mconcat [unField field, " ILIKE ?"]) [SqlValue text]


                 
data CmpOperator = L | G | LEq | GEq | Eq
    deriving (Show, Eq)
data ArrOperator = Overlap | IsContainedBy | Contains
    deriving (Show, Eq)

cmpOperator :: CmpOperator -> PS.Query
cmpOperator L = "<"
cmpOperator G = ">"
cmpOperator LEq = ">="
cmpOperator GEq = "<="
cmpOperator Eq = "="

arrOperator :: ArrOperator -> PS.Query
arrOperator Overlap = "&&"
arrOperator IsContainedBy = "<@"
arrOperator Contains = "@>"


newtype Field = Field { unField :: PS.Query }
    deriving (Show, Eq, S.IsString)
data Param = Num Int | Text T.Text | Array Arr
    deriving (Show, Eq)
data Arr = ArrInt [Int] | ArrText [T.Text]
    deriving (Show, Eq)

instance PSF.ToField Arr where
    toField (ArrInt ints) = PSF.toField $ PSTy.PGArray ints
    toField (ArrText texts) = PSF.toField $ PSTy.PGArray texts
