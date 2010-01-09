{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls -XMultiParamTypeClasses -XPatternSignatures -XDeriveDataTypeable 
 -XNoMonomorphismRestriction
 -XFlexibleInstances #-}
module Test.NoPrimary where
import Data.Set
import Data.Map
import Control.Monad.State
import Database.InMem.Types
import Control.Monad.State
import TestCaseSpliceIn
import Language.Haskell.TH
import Control.Monad.Error
import Database.InMem.TH
import Test.HUnit hiding (State)
import qualified HAppS.Data.Serialize as S

$(testTableA 1 False False)


-- not that much to test here .. 
testNoPrimary = 
 let rows = [ Table1Row i $ (show i) ++ "text" | i <- [1..10] ]
     db = defaultTestcase1
     (r, db2) = (mapM table1Insert rows) `runState` db
  in "testNoPrimary" ~: TestList [ all ( == Just ()) r ~? "insertions ok?"
                                 , (all (`elem` (table1ToList db2)) rows)  ~? "at least one row is missing" ]
