{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls -XMultiParamTypeClasses -XScopedTypeVariables -XDeriveDataTypeable 
 -XNoMonomorphismRestriction
 -XFlexibleInstances #-}
module Test.PrimarySimple where
-- import Debug.Trace
import Data.Binary as B
import qualified HAppS.Data.Serialize as S
import Control.Exception
import Control.Monad.Error
import Control.Monad
import Data.Map as DM
import Data.List as DL
import Data.Typeable
import Control.Monad.State as ST
import Control.Monad.State.Class
import qualified Data.Set
import qualified Data.Maybe
import Data.Either
import Database.InMem.Types
import Control.Monad.State
import TestCaseSpliceIn
import Language.Haskell.TH
import Control.Monad.Error
import Database.InMem.TH
import Test.HUnit hiding ( assert)


trace a b = b

$(testTableA 2 True True )

serder :: (S.Serialize a) => a -> a
serder = fst . S.deserialize . S.serialize

testPrimarySimple =
 let max = 10
     rows = [ Table2Row (i +400) $ (show i) ++ "text" | i <- [1..max] ]
     db = defaultTestcase2
     ((r :: [Either String Int]), db2) = (mapM (\r -> do r <- trace ("adding " ++ (show r)) $ table2Insert r
                                                         d <- ST.get
                                                         trace (show d) $ return ()
                                                         return  r
                                               ) rows) `runState` db
     ((rDup :: Either String Int), db3) = trace (show $ table2ToList db2) $ table2Insert (head rows) `runState` db2
  in "PrimarySimple" ~: TestList [ "primary keys not set ?" ~: r ~?= Prelude.map (Right . (+400)) [1..max]
                                 , (all (\(Table2Row _ t) -> any ((== t) . text2) rows ) $ table2ToList db2)~? "at least one row is missing" 
                                 , rDup ~?= Left "key '401' already present"
                                 , "serialization failed" ~: db2 ~?= serder db2
                                 ]
