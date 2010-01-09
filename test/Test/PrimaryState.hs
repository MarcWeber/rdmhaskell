{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls -XMultiParamTypeClasses -XScopedTypeVariables -XDeriveDataTypeable 
 -XNoMonomorphismRestriction
 -XFlexibleInstances #-}
module Test.PrimaryState where

-- import Debug.Trace
import Control.Monad.Error
import Database.InMem.Types
import Control.Monad.State
import TestCaseSpliceIn
import Language.Haskell.TH
import Control.Monad.Error
import Database.InMem.TH
import Data.Maybe
import Test.HUnit
import qualified HAppS.Data.Serialize as S

trace a b = b


$( do let tablePS = defaultTable {
           tblName = "tablePS"
           , columns = [ ("tpsId", conT ''Int) 
                       , ("text", conT ''String) 
                       ]
           , primary = PrimaryUniq [ "tpsId" ] 
                      (SetPK [| $(varE $ mkName $ "withStateNextTpsIdFromTablePS") $ do {c <- get; put (c+1); return c} |])
           , tblStates = [ ( "nextTpsId", [t| Int |], [| 0 |] ) ]
      }
      let db = defaultDB {
              dbName = "PrimaryState"
            , tables = [ tablePS ]
            , statistics = True }
        in createDB db
        )


isLeft (Left _) = True
isLeft _        = False

testPrimaryState =
 let nextIdFromState = (stateTablePSNextTpsId . tbl_tablePS)
     new = TablePSRow 5000 "dummy"
     count = 10
     rows = [ TablePSRow (i +400) | i <- [1..count] ]
     db = defaultPrimaryState
     (ps, db') = withStateNextTpsIdFromTablePS (modify (\a ->  a - 5)) `runState` db
     -- TODO test updaste failure non existing pk 
     ((r1 :: Either String Int), db'') = tablePSInsert new `runState` db'
     ((r2 :: Either String Int), db''') = tablePSInsert (TablePSRow 5001 "dummy2") `runState` db''
     ((r3 :: Either String ()), dbup) = tablePSUpdateByPK (TablePSRow (-5) "dummy3") (-5)  `runState` db'''
     ((r4 :: Either String ()), dbup2) = tablePSUpdateByPK (TablePSRow (-4) "dummy4") (-4) `runState` dbup
     (r5 :: (), dbdel) = tablePSdeleteByPK (-4) `runState` dbup2
     -- (r6 :: (), dbdel2) = tablePSUpdateByPK (-4) `runState` dbdel -- error 
     (r7 :: (), dbdel2') = tablePSdeleteByPK (-5) `runState` dbdel

     -- ((r :: [Either String Int]), db2) = (mapM (\r -> do r <- trace ("adding " ++ (show r)) $ tablePSInsert r
                                                         -- d <- get
                                                         -- trace (show d) $ return ()
                                                         -- return  r
                                               -- ) rows) `runState` db'
     -- ((rDup :: Either String Int), db3) = trace (show $ tablePSToList db2) $ tablePSInsert (head rows) `runState` db2
  in "PrimaryState" ~: TestList [ "withState" ~: -5 ~=? nextIdFromState db'
                                , "first row key" ~: -5 ~=?  ( tpsId . head . tablePSToList) db'' 
                                , "next key" ~: -4 ~=?  nextIdFromState db''
                                , "res1, res2" ~: [r1,r2] ~?= map Right [-5,-4]
                                , "lookup" ~: [(TablePSRow (-5) "dummy"), ( TablePSRow (-4) "dummy2")] ~=? [ fromJust $ tablePSLookup i db'''  | i <- [ -5, -4]]
                                , "update result" ~: [r3,r4] ~?= [Right (), Right ()]
                                , "lookup after update" ~: [TablePSRow (-5) "dummy3",TablePSRow (-4) "dummy4"] ~?= [ fromJust $ tablePSLookup i dbup2 | i <- [ -5, -4]]
                                -- , "deleting non existing failure" ~? isLeft r6 
                                , "empty ?" ~: [] ~?= tablePSToList dbdel2'
                                -- , "primary keys set ?" ~: r ~?= map Right [-5..(count -5-1)] -- check indexes 
                                -- , rDup ~?= Right (-5+ count +1) 
                                ]
