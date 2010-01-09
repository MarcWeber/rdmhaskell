{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls #-}
module TestCase where
import Database.InMem.Types
import Control.Monad.State
import TestCaseSpliceIn
import Language.Haskell.TH
import Control.Monad.Error
import Database.InMem.TH

-- ========== testcase 2 ============================================= 
{-
table Test

id | String

with primary key and additional index
-}
$(testTableA 2 True True)

-- ========== testcase 3 ============================================= 
{-
table CDs
id | title | num_tracks

table tracks
id | name | cd

num_tracks = count of tracks belonging to the cd (is updated by triggers automatically)

-}

$(let cds = defaultTable {
                tblName = "cds"
                , columns = [ ("cdId", conT ''Int) , ("title", conT ''Int) ]
                , primary = PrimaryUniq [ "cdId" ] [| 0 |]
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
             , primary = PrimaryUniq [ "cd", "trackId" ] [| 0 |]
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
