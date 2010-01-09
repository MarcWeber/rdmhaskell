module Database.InMem.Names where
import Database.InMem.Types
import Database.InMem.Types
import Data.Char
import Language.Haskell.TH


-- prefix primary index if there is one 
-- fst place so that toList outputs rows in primary index order
-- TODO move this functions somewhere else
allIndexes :: Table -> [ Index ]
allIndexes tbl =
  let prim NoPrimary = []
      prim (PrimaryUniq list _) = [foldl (\a n -> Index n [a]) (IndexUnique (head list)) (tail list)]
  in prim (primary tbl) ++ indexes tbl 

dbTableField :: DB -> Table -> String 
dbTableField db tbl = "tbl_" ++ lName (tblName tbl)

lName  (c:cs) =  toLower c : cs 
uName (c:cs) = toUpper c : cs

dbStateField db state = "state"++(uName $ dbName $ db)++(uName state)
dbStateFunctionName db state = "withState"++state++"From"++(dbName db)

-- table helper functions
-- names which will be used
rowADTName, tableADTName :: Table -> String
rowADTName tbl = uName $ tblName tbl ++ "Row" -- cd -> CdRow 
tableADTName tbl = uName $ tblName tbl ++ "Table" -- cd -> CdTable 
idxName tbl idx = case [ (lName $ tblName tbl) ++ "_idx_" ++ (show nr) | (idx', nr) <- zip (allIndexes tbl) [1..], idx == idx'] of -- first index idx_1, second idx_2 etc.. 
                       [] -> error $ "error getting index name for idx " ++ (show idx) ++ " of table " ++ (tblName tbl) -- should not occur 
                       x:_ -> x
tableRowsFields tbl = (lName $ tblName tbl) ++ "_rows"
dbADTName db = uName $ dbName db ++ "DB"

tblStateField db tbl state = "state"++(uName (tblName tbl))++(uName state)
tableStateFunctionName db tbl state = "withState"++state++"From"++(tblName tbl)
tableStatsInserts tbl = tblName tbl ++ "Insertions"
tableStatsDeletions tbl = tblName tbl ++ "Deletions"
tableStatsUpdates tbl = tblName tbl ++ "Updates"

-- manipulating tables 
insertTableName tableName = (lName tableName) ++ "Insert"
toListName tbl = (lName $ tblName $ tbl) ++ "ToList"

lookupName tbl = mkName $ (lName $ tblName $ tbl) ++ "Lookup"
