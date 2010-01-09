{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls -XCPP -XScopedTypeVariables #-}
module Database.InMem.TH where
import System.Directory

import Control.Exception
import qualified Data.Binary as B
import Data.Function
import Data.Typeable
import Database.InMem.Util
import Database.InMem.Types
import Database.InMem.THSplice
import System.IO
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Writer
import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import qualified Data.Set as Set -- maybe use sequence or the newer one (haskelcafe) instead? 
-- import qualified Data.Sequence as S
import qualified Data.List as L
import Control.Monad.State
import Data.Char
import Data.Maybe

#ifdef HAPPS_STATE_SUPPORTa
import qualified HAppS.Data.Serialize as S
import HAppS.Data.SerializeTH
#endif

-- TODO derive equal by primary key 
-- TODO derive equal wihtout primary keys
-- TODO do not only provide <tablename>Functions but also class isntances for convinience 
-- TODO add statistics manipulation functions to no primary key tables (and refactor) 
-- TODO renaming  row -> tuple
--                 table -> relation ?

{-
from a DB description (see TestCase.hs) these functions are created automatically:

foreach table / relation:

  data <tableName>Row = <tableName>Row ...
    for each field: up<Column name> :: (\ColType -> ColType) -> Row -> Row

  instance PrimaryKey <tableName>Row where pk = ...

  -- accessing/ updating database 
  <tableName>Lookup :: PrimaryKey -> StateT DB m Tuple
  <tableName>HasPK :: PrimaryKey -> StateT DB m Bool
  <tableName>HasNotPK :: PrimaryKey -> StateT DB m Bool
  <tableName>UpdateByPK :: (Row -> Row) -> PK -> StateT DB m m2
  (7): <tableName>DeleteByPK ::
  <tableName>Insert :: Row -> State DB (m <primary key>) (3) 
                    or Row -> MonadReaderT STM DB (m <primary key>)

  <tableName>ToList :: DB -> [ Row ]

  -- if TVars are used StateT DB m ret changes to (MonadIO m) => ReaderT DB m ret
  -- 


  instance PrimaryKey :: 

m2 = typically beeing True/False, Maybe [Error], ...

-}

{- 
(2): rowtype
(1): table type
    If there is neither a primary key nor an index Data.Set will be used
    else a tuple (idx1, idx2, ...) is craeted using Data.Map
        Map key row is used for uniq keys
        Map key (Set row) is used for non uniq keys (This may change in the future)
        multiple indexes result in
        Map key (Map key2 row, Map key3 (Set row))
        and such etc..
(3): insert row
  row -> StateT DB m ()
(4): toList: all rows as list in primary key order
(5): table state fucntions, for each given state  function
    withState<state name>From<Table>:: (State st b) -> State DB b
    is created. So you can increment a counter by withStateCounterFrom<Table> $ modify (+1)
(6): defaultTable: initializes an empty table with initial state
(7): 
(8): lookup by primary key
    pk -> DB -> m Row
-}


-- creates a function inserting a row 
-- it finally is a lambda func :: container -> row -> Either String container
-- this all could be done without lookup first when not returning a monadic type but throwing an exception. 
-- This could only be catched within IO, correct? So the only solution would be using unsafePerformIO.. bad within STM
insertIntoIndex :: Index -> ExpQ
insertIntoIndex (IndexUnique t) = 
    [| (\map row ->
        let k = $(varE $ mkName t) row
        in case M.insertLookupWithKey (\_ _ _ -> undefined) k row map of
              (Nothing, m) -> Right m
              (Just o, _) -> Left ("key '" ++ (show k) ++ "' already present")) |]
insertIntoIndex (Index       t []) = 
    [| (\map row ->
        let k = $(varE $ mkName t) row
        in Right $ M.insertWith (Set.union) k (Set.singleton row) map ) |]
insertIntoIndex (Index       t [id]) =
    [| (\map row ->
        let k = $(varE $ mkName t) row
            insertSub = $(insertIntoIndex id)
        in  case M.lookup k map of
              Just m -> do submap <- insertSub m row 
                           Right $ M.insert k submap map -- insert = replace here
              Nothing -> case insertSub M.empty row of
                            Left _ -> error "internal error, should never occur because inserting into empty submap"
                            Right sm -> Right $ M.insert k sm map ) |]
insertIntoIndex (Index       t ids) = error "TODO (multi subindex)"


-- creates function removing a row 
-- func :: container -> row -> Either String container
-- warning:! the row passed must have exactly the same key values as the row in
-- the maps. Otherwise the row might be deleted from some maps but not all.
-- Thus the row passed here should be taken from the table before.
deleteFromIndex :: Index -> ExpQ
deleteFromIndex (IndexUnique t) = 
    [| (\map row ->
        let k = $(varE $ mkName t) row
        in  assert (k `M.member` map)  $ M.delete k map ) |]
deleteFromIndex (Index       t []) = 
    [| (\map row ->
        let k = $(varE $ mkName t) row
        in  assert (M.member k map) $ M.updateWithKey (\k set -> let s = assert (Set.member row set) (Set.delete row set) in if Set.null s then Nothing else Just s) k map ) |]
deleteFromIndex (Index       t [id]) =
    [| (\map row ->
        let k = $(varE $ mkName t) row
        in  assert (M.member k map) $ M.updateWithKey (\k map -> let m = $(deleteFromIndex id) map row in if M.null m then Nothing else Just m) k map ) |]
deleteFromIndex (Index       t ids) = error "TODO (multi subindex)"

lName  (c:cs) =  toLower c : cs 
uName (c:cs) = toUpper c : cs
lookupE k l m = case L.lookup k l of
  Nothing -> error $ m k
  Just e -> e
-- db helper functions 
tableFieldName :: DB -> Table -> String 
tableFieldName db tbl = "tbl_" ++ lName (tblName tbl)

dbStateField db state = "state"++(uName $ dbName $ db)++(uName state)
dbStateFunctionName db state = "withState"++(uName state)++"From"++(uName $ dbName db)

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
tableStateFunctionName db tbl state = "withState"++(uName state)++"From"++(uName $ tblName tbl)
tableStatsInserts tbl = lName (tblName tbl) ++ "Insertions"
tableStatsDeletions tbl = lName (tblName tbl) ++ "Deletions"
tableStatsUpdates tbl = lName (tblName tbl) ++ "Updates"

-- manipulating tables 
insertTableName tableName = (lName tableName) ++ "Insert"
updateByPKName tableName = (lName tableName) ++ "UpdateByPK"
toListName tableName = (lName tableName) ++ "ToList"

-- prefix primary index if there is one 
-- fst place so that toList outputs rows in primary index order
allIndexes :: Table -> [ Index ]
allIndexes tbl =
  let prim NoPrimary = []
      prim (PrimaryUniq list _) = [foldl (\a n -> Index n [a]) (IndexUnique (head list)) (tail list)]
  in prim (primary tbl) ++ indexes tbl 



createDB :: DB -> Q [Dec]
createDB db@(DB dbName tables dbStates statistics useTVars) = do
  dbDecs <- dbDeclarations db
  tableDecs <- tablesDeclarations db
  -- print created declarations for debugging and information 
  runIO $ let text = unlines $  [ "-- ===================================================="
                               , "-- declarations db " ++ dbName ++ ":"
                               , "-- ====================================================" ]
                               ++ (map pprint dbDecs)
                               ++ (concat $ zipWith (\decs tbl ->  ("-- ===== table  " ++ (tblName tbl) ++ "  =============================") : map pprint decs 
                                                    ) tableDecs tables )
              dir = "auto-genereted-code"
          in do putStrLn text >> hFlush stdout
                -- also write generated code to extra file for convinience (and for tag programs :-) 
                createDirectoryIfMissing False dir
                writeFile (dir ++ "/" ++ dbName ++ ".hs") text
  return $ dbDecs ++ (concat tableDecs)

-- ========== db functions =========================================== 
dbDeclarations db@(DB dbName tables dbStates statistics useTVars) = do
    dbStates' <- liftSndTrd dbStates
    let stateFields = map (\(s, t, _) -> ( mkName $ "state_" ++ s, t)) dbStates'
    let tables' :: [( Name, Type)]
        tables' =  [ ( mkName $ tableFieldName db tbl, (ConT . mkName . tableADTName) tbl ) | tbl <- tables ]
        dbName' = mkName $ dbADTName db
        -- adds TVar <type> if TVars should be used 
        -- wrapTVar :: Bool -> ( String, Type ) -> ( String, Type )
        -- wrapTVar useTVars (n, t) = (n, if useTVars then AppT (ConT ''TVar) t else t)
    -- db data type 
    dbADT <- adt dbName' $ {-map (wrapTVar useTVars) $ -} tables' ++ stateFields

    -- db initialization funciton 
    defaultDB <- let dbs = map (\table -> liftM ((,) (mkName $ tableFieldName db table)) (varE $ mkName $ "default" ++ (tableADTName table) ) ) tables
                     states = map (\(n,_,e) -> return (mkName n,e)) dbStates'
                 in funD (mkName $ "default" ++ (uName dbName)) 
                         [clause [] (normalB $ recConE dbName' $ dbs ++ states ) []]
    -- modify db state 
    dbStateFunctions <- do 
         [f,db', s',r] <- mapM newName ["f", "db", "s'","r" ]
         mapM ((\(n, t, _) -> 
                let fieldName = mkName $ dbStateField db n
                in  funD (mkName $ dbStateFunctionName db n)
                  [clause [varP f] (
                       --  
                      normalB $ doE [ -- db' <- get
                            bindS (varP db') (varE 'get)
                           -- let (r, db') = runSate f $ stateField db'
                          , letS [valD (tupP [varP r, varP s']) (normalB [| runState $(varE f) ($(varE $ fieldName) $(varE db')) |]) [] ]
                          , --  put $ db'{state = s}
                            noBindS [| put $(recUpdE (varE db') ([ return (fieldName, VarE s')]))|]
                          , -- return r 
                            noBindS [| return $(varE r) |]
                          ])
                          []
                  ]) ) dbStates'

#ifdef HAPPS_STATE_SUPPORTa

    let dbSerialize = do
          let stateNames = map (\(a,_,_) -> mkName a) dbStates'
          let tableNames = map fst $ tables'
          let (names :: [ Name ] ) = (stateNames ++ tableNames)
          temps <- mapM (const $ newName "tmp") names
          let (toBeSerialized :: [ExpQ]) = map varE names
          a <- newName "a"
          instanceD (cxt []) (appT (conT $ ''S.Serialize) (conT dbName'))
                          [ funD  (mkName "getCopy") [clause [] (normalB $ appE (varE 'S.contain) $
                            -- new table adt, fill in rows 
                            doE $ [ bindS (varP tmp) (varE 'S.safeGet) | (_, tmp) <- zip names temps ]
                                  -- assign states, return table (TODO: use constructor instead)
                                  ++ [ noBindS $ appE (varE 'return) $ appEn (conE $ dbName') (map varE temps) ]
                              ) []]
                          , funD  (mkName "putCopy") [clause [varP a] (normalB $ 
                            appE (varE 'S.contain) $ doE $ map (\ts -> noBindS $ appE (varE 'S.safePut) (appE ts (varE a))) toBeSerialized
                            ) []]
                          ]
    verTable <- instanceD (cxt []) (appT (conT $ ''S.Version) (conT $ dbName')) []
    dbSer <- dbSerialize
#endif

    -- dbTHQuery :: String -> DecQ
    -- dbTHQuery queryStr = error "todo" 
    return $ dbADT : defaultDB 
#ifdef HAPPS_STATE_SUPPORTa
              : dbSer : verTable
#endif
              : dbStateFunctions




-- ========== table functions ======================================== 
tablesDeclarations :: DB -> Q [[Dec]]
tablesDeclarations db = mapM (tableDecs db) (tables db)

tableDecs :: DB -> Table -> Q [Dec]
tableDecs db (tbl@(Table n cs p idxs chks tgs sts)) = do
  -- bindings used in many functions 
  let rowADTName = mkName $ Database.InMem.TH.rowADTName tbl
  let dbADTName = mkName $ Database.InMem.TH.dbADTName db
  let allIdxs = allIndexes tbl
  let rowsFieldName = mkName $ tableRowsFields tbl
  let idxName = Database.InMem.TH.idxName tbl
  let stateFieldName = mkName . tblStateField db tbl
  let statFields = map ($ tbl) [tableStatsInserts, tableStatsDeletions, tableStatsUpdates] 
  let tblFieldName = mkName $ tableFieldName db tbl
  let tblADTName = mkName $ tableADTName tbl

  let statsInsertsFieldName = tableStatsInserts tbl
  let statsDeletetionsFieldName = tableStatsDeletions tbl
  let statsUpdatesFieldName = tableStatsUpdates tbl

  let statistics' = statistics db
  let useTVars' = useTVars db

  -- function names table interaction
  let insertFName = mkName $ insertTableName n
  let updateFName = mkName $ updateByPKName n
  let toListFName = mkName $ toListName n
  let lookupFName = mkName $ (lName n) ++ "Lookup"
  let deleteFName = mkName $ (lName n) ++ "deleteByPK"

  let defaultTableFName = mkName $ "default" ++ (uName $ tableADTName tbl) -- internal use only 
  let pkType = case p of
            NoPrimary -> conT '()
            (PrimaryUniq pks _) -> 
                    let types = map (\k -> fromJust $ L.lookup k cs) pks
                    in tupH (appn appT (tupleT (length pks))) types
  states' <- liftSndTrd sts
  columns' <- liftSnd cs

  -- functions creating declarations: 
  let tableADTDec = do
        let lt :: String ->Type 
            lt t = lookupE t columns'  -- lookup index type 
               (\e -> "column named " ++ (show e) ++ " found in index definition not found in table")
        let idxType (IndexUnique t)       = AppT (AppT (ConT ''M.Map) (lt t)) (ConT rowADTName) -- Map k Row 
            idxType (Index       t idxs)  = AppT (AppT (ConT ''M.Map) (lt t)) (sub idxs) -- Map k (Set / Map subkey (..) / (Map subkey, Map subkey .. )
              where sub [] = AppT (ConT ''Set.Set) (ConT  rowADTName) -- no subindex : Set Row
                    sub [si] = idxType si -- one subindex 
                    sub _ = error "TODO 3" -- will be tuple of Maps 
        let rows [] = [ ( rowsFieldName,  AppT (ConT ''Set.Set) (ConT rowADTName) ) ] -- no index: Set Row
            rows idxs = [ (mkName $ idxName idx, idxType idx ) | idx <- idxs ]
        let statesDecs = [ ( stateFieldName s, t) | (s, t, _) <- states']
        let stats = [ (mkName n, ConT ''Integer) | n <- statFields ]
        adt tblADTName $ (rows allIdxs ) ++ statesDecs ++ stats


  -- used in tableInsertRow and serialization only
  -- table -> row -> table 
  let insertRowInternal = if (null allIdxs) 
                            then do [t, row] <- mapM newName ["t", "row"] -- no indexes, just insert into the set
                                    -- (\table row -> table{ rows = S.insert row (rows table) ) 
                                    lamE [varP t, varP row ] (recUpdE (varE t) ([liftM ((,) rowsFieldName) [| Set.insert $(varE row) $(appE (varE $ rowsFieldName) (varE t ) ) |]]))
                            else do [table, row] <- mapM newName ["table","row"]
                                    let idxN = map idxName allIdxs
                                    let t_idxN = map ("t_" ++ ) idxN
                                    let nInsert = mkName $ tableStatsInserts tbl
                                    incInsertC <- [| 1 + ($(varE nInsert)) $(varE $ table) |]
                                    let updateStats = if statistics' then [(nInsert, incInsertC)] else []
                                    lamE [varP table, varP row] 
                                             (doE $ -- t_idx_XX <- <insertIntoMap> (idx_XX table) row
                                                  ( zipWith3 (\n t idx -> do f <- insertIntoIndex idx
                                                                             -- table { <index> = <f> (<index> table) row }
                                                                             return $ BindS (VarP $ mkName t) $ AppE (AppE f (AppE (VarE $ mkName n) (VarE table))) (VarE row)
                                                        ) idxN t_idxN allIdxs
                                                  )
                                                  -- return $ table {idx_XX = t_idx_XX , ... }
                                                  ++ [( return $ NoBindS $ AppE (VarE 'return) $ RecUpdE (VarE table) $ (updateStats ++) $ zipWith (\u t -> (mkName u, VarE $ mkName t)) idxN t_idxN)])

  -- row -> State DB (Either String <primary key>)
  let tableInsertRow = 
        --  TODO insert triggers
          if useTVars'
                then error "TODO 1"
                else  do row <- newName "row"
                         fbody <- do  row' <- newName "row'"
                                      [t,msg,t2,db2,map,db] <- mapM newName ["t","msg", "t2", "db2","map","db"]
                                      doE [ bindS (varP row') (
                                              case p of -- set new primary key value 
                                                  (PrimaryUniq _ (SetPK f)) 
                                                    -> [| liftM (setPK $(varE row)) $(f) |]  -- let 
                                                  (PrimaryUniq _ (MaxPKPlusOne start)) 
                                                    -> [| do  pmap <- gets $ $(varE $ mkName $ idxName (head allIdxs)) . $(varE tblFieldName)
                                                              ( return . setPK $(varE row)) ( if M.null pmap then fromIntegral $(litE $ integerL start) 
                                                                                         else (1+) . fst . M.findMax $ pmap ) |]
                                                  (PrimaryUniq [p,psnd] (SetSubPKPlusOne start))
                                                    -> do  r <- newName "r"
                                                           doE [ bindS (varP r) (appE (varE 'gets) 
                                                                                      (appEn (varE '(.))
                                                                                            [ varE $ mkName $ idxName (head allIdxs)
                                                                                            , varE tblFieldName]))
                                                               , noBindS $ caseE (appEn (varE 'M.lookup) [appE (varE $ mkName p) (varE row), varE r])
                                                                              [ match (conP (mkName "Nothing") []) (normalB [| return $(recUpdE' row (mkName psnd) (litE $ integerL start) ) |]) []
                                                                              , match (conP (mkName "Just") [varP map]) (normalB [| return $ 
                                                                                      $(recUpdE (varE row) [liftM  ((,) (mkName psnd)) [|(+1) $ fst $ M.findMax $(varE map) |]] ) |] ) []
                                                                              ] 
                                                               ]
                                                        {- [| (\row -> do tbl <- gets $(mkNameE "tbl_cds")
                                                                    case M.lookup ($(mkNameE "cd") row) ($(mkNameE "tracks_idx_1") tbl) of
                                                                      Nothing -> row { trackId = 1 }
                                                                      Just m ->  row { trackId = (+1) $ fst $ findMax m } ) |] )
                                                                      -}
                                                  _
                                                    -> appE (varE 'return) (varE row)
                                            )

                                          , bindS (varP db) (varE 'get)
                                          , letS [ valD (varP t) (normalB ( appE (varE tblFieldName) (varE db) ) ) [] ]
                                          , {- case (<insertRowInternal t> of
                                                Left msg -> return $ fail msg
                                                Right table -> do modify (\db -> db{<table> = table } )
                                                                  triggers
                                                                  return primary key
                                           -}
                                            if (null allIdxs)
                                              then noBindS [| -- modify (\db -> db{ rows = insertRowInternal table row } ) -- no indexes, nothing can go wrong 
                                                            do modify $ $(lamE [varP  db] (recUpdE (varE db) ([ liftM ((,) tblFieldName) (appEn insertRowInternal [varE t, varE row']) ])) )
                                                                -- TODO run triggers 
                                                               return $ return ()
                                            
                                                           |]
                                              else noBindS $ caseE (appEn insertRowInternal [varE t, varE row'] )
                                                      [ match (conP (mkName "Left") [varP msg]) (normalB [| put $(varE db) >> (return . fail) $(varE msg) |]) [] -- in case of failure set old state 
                                                      , match (conP (mkName "Right") [varP t2])
                                                            (normalB [| 
                                                              do modify $ $(lamE [varP  db2] (recUpdE (varE db2) ([ return (tblFieldName, VarE t2) ]) ))
                                                                  -- TODO run triggers 
                                                                 return $ return $  pk $(varE row') -- return primary key 
                                                            |]) []
                                              ]
                                          ]
                         -- <insertTableName> row = <fbody> 
                         return $ (FunD (insertFName) [Clause [VarP row] (NormalB fbody) []])
  let tableUpdateRow = 
        if (not $ hasPrimary p) then return []
        else liftM (:[]) $ do
                [newrow, oldrow] <- mapM newName ["newrow", "oldrow"]
                let -- insertRowInternal is lambda of type  table -> row -> Either String table
                  insertRowInternal = if (null allIdxs) 
                                    then do [t, row] <- mapM newName ["t", "row"] -- no indexes, just insert into the set
                                            -- (\table row -> table{ rows = S.insert row (rows table) ) 
                                            lamE [varP t, varP row ] (recUpdE (varE t) ([liftM ((,) rowsFieldName) 
                                                [| let set = $(appE (varE $ rowsFieldName) (varE t ) )
                                                   in Set.insert $(varE newrow) $ 
                                                        assert (Set.member $(varE oldrow) set) $ Set.delete $(varE oldrow) set |]]))
                                    else do [table, row] <- mapM newName ["table","row"]
                                            let idxN = map idxName allIdxs
                                            let t_idxN = map ("t_" ++ ) idxN
                                            let nUpdate = mkName $ tableStatsUpdates tbl
                                            incUpdatesC <- [| 1 + ($(varE nUpdate)) $(varE $ table) |]
                                            let updateStats = if statistics' then [(nUpdate, incUpdatesC)] else []
                                            lamE [varP table, varP row] 
                                                     (doE $ -- t_idx_XX <- <insertIntoMap> (idx_XX table) row
                                                          ( zipWith3 (\n t idx -> do f <- insertIntoIndex idx
                                                                                     -- table { <index> = <f> (<index> table) row }
                                                                                     bindS (varP $ mkName t) $ 
                                                                                       appEn (insertIntoIndex idx) 
                                                                                             [ appEn (deleteFromIndex idx) [ appE (varE $ mkName n) (varE table), (varE oldrow) ]
                                                                                             , varE row ] ) idxN t_idxN allIdxs

                                                          )
                                                          -- return $ table {idx_XX = t_idx_XX , ... }
                                                          ++ [ return $ NoBindS $ AppE (VarE 'return) $ 
                                                                RecUpdE (VarE table) $ (updateStats ++) $ zipWith (\u t -> (mkName u, VarE $ mkName $ t)) idxN t_idxN ])
                --  TODO insert triggers
                  -- row -> State DB (Either String <primary key>)
                if useTVars'
                        then error "TODO 1"
                        else  do pk <- newName "pk"
                                 fbody <- do  [t,msg,t2,db, rowm] <- mapM newName ["t","msg", "t2", "db", "rowm"]
                                              doE [ -- t <- gets <table> 
                                                    bindS (varP t) [| gets $(varE tblFieldName) |]
                                                  , bindS (varP rowm) $ [| gets $(appE (varE $ lookupFName) (varE pk)) |]
                                                  , noBindS $ caseE (varE $ rowm) 
                                                                [ match (conP (mkName "Left") [varP msg]) (normalB [| fail $ "could not update non existing primary key " ++(show $(varE pk) ) ++ " from table " ++ $(litE $ stringL n) ++ " " ++  $(varE msg) |]) []
                                                                , match (conP (mkName "Right") [varP oldrow]) ( normalB $
                                                                      if (null allIdxs)
                                                                        then [| -- modify (\db -> db{ rows = insertRowInternal table row } ) -- no indexes, nothing can go wrong 
                                                                                      do modify $ $(lamE [varP  db] (recUpdE (varE db) ([ liftM ((,) tblFieldName) (appEn insertRowInternal [varE t,varE newrow]) ])) )
                                                                                          -- TODO run triggers 
                                                                                         return $ return ()
                                                                      
                                                                                     |]
                                                                        else caseE (appEn insertRowInternal [varE t, varE newrow] ) 
                                                                                [ match (conP (mkName "Left") [varP msg]) (normalB [| return $ fail $ $(varE msg) |]) []
                                                                                , match (conP (mkName "Right") [varP t2])
                                                                                      (normalB [| 
                                                                                        do modify $ $(lamE [varP  db] (recUpdE (varE db) ([ return (tblFieldName, VarE t2) ]) ))
                                                                                            -- TODO run triggers 
                                                                                           return $ return ()
                                                                                      |]) []

                                                                                ] ) []
                                                              ]
                                                  ]
                                 -- <insertTableName> row = <fbody> 
                                 return $ (FunD (updateFName) [Clause [VarP newrow, VarP pk] (NormalB fbody) []])


  -- only used in tableToList and serialization (internal use) 
  let  rowsFromIndex (IndexUnique _) = [| M.elems |]
       rowsFromIndex (Index _ [])    = [| (concatMap Set.toList) . M.elems |]
       rowsFromIndex (Index _ [idx]) = [| concatMap $(rowsFromIndex idx) . M.elems |]
       rowsFromIndex (Index _ (idx:_)) = error "TODO multi subindexes"

       -- if there are multi indexes this could be optimized - don't care 
                                  -- S.toList (rows table)
       rowsFromTable' [] = [| Set.toList . $(varE rowsFieldName) |]
       rowsFromTable' (idx:_) = [| $(rowsFromIndex idx) . $(varE . mkName $ idxName idx) |]
       rowsFromTable = rowsFromTable' allIdxs

  let tableToList = do 
              [db] <- mapM newName [ "db" ]
              funD (toListFName)
                      [clause [varP db] 
                              (normalB (appE rowsFromTable (appE (varE $ tblFieldName) (varE db))  ) ) 
                              []
                      ]

  let tableStateFunctions = do 
        [f,db',table', s',r] <- mapM newName ["f","db", "table", "s'","r" ]
        mapM ((\(n, t, _) -> 
            let fieldName = stateFieldName n
            in  funD (mkName $ tableStateFunctionName db tbl n)
              [clause [varP f] (
                   --  
                  normalB $ doE [ 
                         -- db <- get
                        bindS (varP db') (varE 'get)
                      , 
                        -- let table <- <tableName> db
                        --     (r, s') = runSate f $ stateField db'
                        --
                        letS [ valD (varP table') (normalB (appE (varE $ tblFieldName) (varE db') )) []
                             , valD (tupP [varP r, varP s']) (normalB [| runState $(varE f) ($(varE $ fieldName) $(varE table')) |]) [] ]
                      , --  put $ db'{tableName =  table{ state = s'} }
                        noBindS [| put $(recUpdE' db' tblFieldName (recUpdE' table' fieldName (varE s')) )|]
                      , -- return r 
                        noBindS [| return $(varE r) |]
                      ])
                      []
              ]) ) states'

  let tableDefaultTable = 
          let indexes [] = [ return (rowsFieldName, VarE 'Set.empty) ]
              indexes idxs = map (\i -> return (mkName $ idxName i, VarE 'M.empty)) idxs
              states = map (\(n,_,e) -> return (stateFieldName n,e) ) states'
              stats = [ return (mkName n, LitE $ IntegerL 0) | n <- statFields ]
          in funD (defaultTableFName) 
                        [clause [] (normalB $ recConE tblADTName $ (indexes allIdxs) ++ states ++ stats ) []]

  let tableLookupRow = 
       let lr NoPrimary = return []
           lr (PrimaryUniq fields _) = do
              db:ks <- mapM newName $ "db":fields
              let fromMap [k] = [| M.lookup $(varE k) |]
                  fromMap (k:ks) = [| (\m -> M.lookup $(varE k) m  >>= $(fromMap ks)) |]
              liftM (:[]) $ funD lookupFName
                              [ clause ( tupH tupP (map varP $ ks) : [varP db])
                              (normalB $ appE (fromMap ks) ( (appE (varE $ mkName $ idxName (head allIdxs))) (appE (varE tblFieldName) (varE db))))
                              [] ]
       in lr p

  let tableDeleteRow = 
        let
            -- insertRowInternal is lambda of type  table -> row -> Either String table
            removeFromTable = if (null allIdxs) 
                  then do [t, row] <- mapM newName ["t", "row"] -- no indexes, just remove from set
                          -- (\table row -> table{ rows = Set.delete row (rows table) ) 
                          lamE [varP t, varP row ] (recUpdE (varE t) ([liftM ((,) rowsFieldName) [| Set.delete $(varE row) $(appE (varE $ rowsFieldName) (varE t ) ) |]]))
                  else do [table, row] <- mapM newName ["table","row"]
                          let idxN = map idxName allIdxs
                          let t_idxN = map ("t_" ++ ) idxN
                          let nDelete = mkName $ tableStatsDeletions tbl
                          incDeleteC <- [| 1 + ($(varE nDelete)) $(varE $ table) |]
                          let updateStats = if statistics' then [return (nDelete, incDeleteC)] else []
                          lamE [varP table, varP row] $
                                   letE -- t_idx_XX <- <insertIntoMap> (idx_XX table) row
                                        ( zipWith3 (\n t idx -> valD (varP $ mkName t) 
                                                                     (normalB $ appEn (deleteFromIndex idx) [ appE ( mkNameE n) (varE table), varE row]) [] )
                                                   idxN t_idxN allIdxs )
                                        ( recUpdE (varE table) $ (updateStats ++) $ zipWith (\u t -> return (mkName u, VarE $ mkName t)) idxN t_idxN)
          in   --  TODO insert triggers
              -- row -> State DB (Either String <primary key>)
              if (not $ hasPrimary p) then return []
              else liftM (:[]) $ if useTVars' 
                    then error "TODO 1"
                    else  do pk <- newName "pk"
                             fbody <- do  [row, msg, db', t, t2, rowm] <- mapM newName ["row","msg","db", "t", "t2", "rowm"]
                                          doE [ bindS (varP t) [| gets $(varE tblFieldName) |]
                                              , bindS (varP rowm) $ [| gets $(appE (varE $ lookupFName) (varE pk)) |]
                                              , noBindS $ caseE (varE $ rowm) 
                                                            [ match (conP (mkName "Left") [varP msg]) (normalB [| error $ "could not delete non existing primary key " ++(show $(varE pk) ) ++ " from table " ++ $(litE $ stringL n) ++ " " ++  $(varE msg) |]) []
                                                            , match (conP (mkName "Right") [varP row]) ( normalB $
                                                                if (null allIdxs)
                                                                    then [| -- modify (\db -> db{ rows = insertRowInternal table row } ) -- no indexes, nothing can go wrong 
                                                                                  do modify $ $(lamE [varP db'] (recUpdE (varE db') ([ liftM ((,) tblFieldName) 
                                                                                                  [| let set = $(appE (varE $ rowsFieldName) (varE t))
                                                                                                         r = $(varE row)
                                                                                                     in assert (Set.member r set) $ Set.delete r set |] ])))
                                                                                      -- TODO run triggers 
                                                                                     return ()
                                                                                 |]
                                                                    else  -- TODO remove caseE, not needed here 
                                                                          caseE (appEn removeFromTable [varE t, varE row ])
                                                                            [ -- match (conP (mkName "Left") [varP msg]) (normalB [| return $ fail $ $(varE msg) |]) []
                                                                              match (varP t2)
                                                                                  (normalB [| 
                                                                                    do modify $ $(lamE [varP db'] (recUpdE (varE db') ([ return (tblFieldName, VarE t2) ]) ))
                                                                                        -- TODO run triggers 
                                                                                       return ()
                                                                                  |]) []
                                                                            ]
                                                                      ) []
                                                            ]
                                                ]
                             -- <insertTableName> row = <fbody> 
                             funD deleteFName [clause [varP pk] (normalB (return fbody)) []]
  let tablePKIntsance = case p of
            NoPrimary -> return []
            (PrimaryUniq pks _) -> do 
              row <- newName "row"
              let f n i = funD (mkName n) [clause [] (normalB (varE i)) []]
              liftM  (:[]) $ instanceD (cxt []) (appTn (conT ''TableWithPrimaryKey) (pkType : map conT [rowADTName, tblADTName, dbADTName] ))
                                -- TODO what about statistics?
                                [ funD  (mkName "pk")    [clause [varP row] 
                                                                 (normalB (tupH tupE (map (\k -> (appE (varE $ mkName $ k) (varE row))) pks))) []]
                                , funD  (mkName "setPK") [clause [varP row, tupH tupP (map (varP . mkName) pks)]
                                                                 (normalB ( recUpdE (varE row) [ return (mkName f, VarE $ mkName f) | f <- pks])) []]
                                , f "insert" insertFName
                                , f "updateByPK" updateFName
                                , f "lookup" lookupFName
                                , f "deleteByPK" deleteFName
                                ]
                        
                        
#ifdef HAPPS_STATE_SUPPORTa

  -- TODO what about statistics?
  let tblSerialize = do
          let stateNames = map (stateFieldName . (\(a,_,_) -> a)) states'
          temps <- mapM (const $ newName "tmp") stateNames
          let (toBeSerialized :: [ExpQ]) = rowsFromTable : map varE stateNames
          [a,r,t,t2,table] <- mapM newName ["a","r","t","t2","table"]
          serTable <- instanceD (cxt []) (appT (conT $ ''S.Serialize) (conT tblADTName))
                          [ funD  (mkName "getCopy") [clause [] (normalB $ appE (varE 'S.contain) $
                            -- new table adt, fill in rows 
                            doE $ [ bindS (varP table)  [| liftM (foldr (\r t -> case $(insertRowInternal) t r of
                                                                                       Left msg -> error ("couldn't insert row with id " ++ (show $ pk r) ++", message " ++ msg)
                                                                                       Right t -> t ) $(varE defaultTableFName) ) S.safeGet |]
                                  ] 
                                  -- read states 
                                  ++ [ bindS (varP tmp) (varE 'S.safeGet) | (_, tmp) <- zip stateNames temps ]
                                  -- assign states, return table (TODO: use constructor instead)
                                  ++ [ noBindS $ appE (varE 'return) $ recUpdE (varE table)
                                        [ liftM ((,) n) (varE tmp) | (n, tmp) <- zip stateNames temps ]
                                     ]
                              ) []]
                          , funD  (mkName "putCopy") [clause [varP a] (normalB $ 
                            appE (varE 'S.contain) $ doE $ map (\ts -> noBindS $ appE (varE 'S.safePut) (appE ts (varE a))) toBeSerialized
                            ) []]
                          ]
          verTable <- instanceD (cxt []) (appT (conT $ ''S.Version) (conT $ tblADTName)) []
          -- can't use deriveSerialize from HAppS-Data here because the data is not yet defined This could be changed by using two splicing regions,
          -- one to define the data type and the second to define the this instance
          temps <- mapM (const $ newName "tmp") columns'
          let (toBeSerialized' :: [ExpQ]) = map (varE . mkName . fst) columns'
          serRow <-  instanceD (cxt []) (appT (conT $  ''S.Serialize) (conT rowADTName))
                          [ funD  (mkName "getCopy") [clause [] (normalB $ appE (varE 'S.contain) $
                            -- new table adt, fill in rows 
                            doE $  [ bindS (varP tmp) (varE 'S.safeGet) | (_, tmp) <- zip ((map fst) columns') temps ]
                                   ++ [ noBindS $ appE (varE 'return) $ appEn (conE rowADTName) $ map varE temps ]
                              ) []]
                          , funD  (mkName "putCopy") [clause [varP a] (normalB $ 
                            appE (varE 'S.contain) $ doE $ map (\ts -> noBindS $ appE (varE 'S.safePut) (appE ts (varE a))) toBeSerialized'
                            ) []]
                          ]
          verRow <- instanceD (cxt []) (appT (conT $ ''S.Version) (conT rowADTName)) []
          return $ [ serTable, verTable, serRow, verRow ]
#endif
  states' <- liftSndTrd sts
  rowADT <- adt rowADTName =<< liftM (map (\(a,b) -> (mkName a, b))) (liftSnd cs)
  tableADT <- tableADTDec
  insertRow <- tableInsertRow
  toList <- tableToList
  stateFucntions <- tableStateFunctions
  lookupRow <- tableLookupRow
  deleteRow <- tableDeleteRow
  defaultTable <- tableDefaultTable
  updateRow <- tableUpdateRow
  pkInstance <- tablePKIntsance
  insertRowSig <- do
    [m] <- mapM newName ["m"]
    sigD insertFName (forallT [m] 
                     (cxt [appT (conT ''Monad) (varT m)])
                     (appT (appT arrowT (conT rowADTName)) (appT (appT (conT ''State) (conT dbADTName)) (appT (varT m) pkType))))
#ifdef HAPPS_STATE_SUPPORTa
  tblSer <- tblSerialize
#endif
  return $ [ rowADT, tableADT, insertRow, insertRowSig, toList, defaultTable] ++ stateFucntions ++ lookupRow ++ deleteRow ++ updateRow ++ pkInstance
#ifdef HAPPS_STATE_SUPPORTa
              ++ tblSer
#endif



-- some helper functions 
strictness = IsStrict
adt n ts = return $ DataD [] n [] [RecC (n) [ (n, strictness, t) | (n,t) <- ts ]] [''Show, ''Ord, ''Eq, ''Typeable] -- data n = n { ts ... } 
appEn :: ExpQ -> [ ExpQ ] -> ExpQ
appEn t l = appn appE t l
appTn t l = appn appT t l
appn app f = appn' f . reverse
  where appn' f [] = f
        appn' f (a:as) = app (appn' f as) a

-- helper function: only applies tuple functions if there is more than one element 
tupH _ [a] = a
tupH f as = f as
liftSnd :: [(a, Q b)]  -> Q [(a,b)]
liftSnd = mapM (\(a,b) -> b >>= \b' -> return  (a,b'))
liftSndTrd :: [(a, Q b, Q c)]  -> Q [(a,b,c)]
liftSndTrd = mapM (\(a,b,c) -> liftM2 ((,,) a) b c)
