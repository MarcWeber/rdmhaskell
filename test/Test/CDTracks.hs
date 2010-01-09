{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls -XMultiParamTypeClasses -XScopedTypeVariables -XDeriveDataTypeable 
 -XNoMonomorphismRestriction
 -XFlexibleInstances #-}
module Test.CDTracks where
import Database.InMem.Util
import qualified HAppS.Data.Serialize as S
import Data.Typeable
import Control.Exception
import Database.InMem.Types
import Control.Monad.State
import TestCaseSpliceIn
import Language.Haskell.TH
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Class
import Database.InMem.TH
import qualified Data.Map as M
import Data.Maybe
import Data.Either
import Data.Set
import Test.HUnit hiding ( assert)

-- this will test relational features (foreign key constraints etc) in the future (TODO) 


$(let cds = defaultTable {
               tblName = "cds"
               , columns = [ ("cdId", conT ''Int) , ("title", conT ''Int) ]
               , primary = PrimaryUniq [ "cdId" ] (MaxPKPlusOne 10)
               , indexes = [ Index "title" [] ]
               , tblStates = [ ( "nextCdId", [t| Int |], [| 0 |] ) ]
           }

      tracks = let
            a="a"
            -- updateNumRows n = [| \n -> cdUpdateByPK (\r -> r { num_tracks = (num_tracks r) + $(n) } ) |]
            in defaultTable {
            tblName = "tracks"
            , columns = [ ("trackId", conT ''Int )
                      , ("name", conT ''String)
                      , ("cd", conT ''Int) -- foreign key 
                      ]
            , primary = PrimaryUniq [ "cd", "trackId" ] 
                                    -- is there a nice way than using that many mkNameE 
                                    -- KeepPk
                                    (SetSubPKPlusOne 5)
            , indexes = [ Index "cd" [ IndexUnique "trackId" ] ] --the id is uniq foreach cd 
            -- checks = [ foreignConstraint "cd" "cds" "id" ]
            -- triggers =  [ InsertUpdate  (Just ["cd"]) [| cdUpdateByPK ( updateNum_tracks (+1) ) . pk |]
                          -- DeleteUpdate  (Just ["cd"]) [| cdUpdateByPK ( updateNum_tracks (-1) ) . pk |] 
                          -- ]
                }
      db = defaultDB {
             dbName = "my"
           , tables = [ cds, tracks]
           , statistics = True
           }
 in createDB db)

testCDTracks = TestCase $ error "TODO"


