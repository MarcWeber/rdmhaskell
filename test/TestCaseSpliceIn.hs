{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls #-}
module TestCaseSpliceIn where
import Database.InMem.Types
import Control.Monad.State
import Language.Haskell.TH
import Control.Monad.Error
import Database.InMem.TH

-- functions whose result will be spliced in in TestCase.hs 

testTableA :: Int  -> Bool -> Bool -> Q [Dec]
testTableA nr primary index = 
  let addNr = (++ (show nr))
      addPrimary = if primary 
          then (\t -> t { primary = PrimaryUniq [ addNr "id" ] KeepPk
                       , tblStates = [ ( addNr "id", conT ''Int, [| 0 |] ) ] })
          else id
      addIndex = if index 
          then (\t -> t{ indexes = [ Index (addNr "text") [] ] })
          else id
      table = addPrimary $ addIndex $  defaultTable { 
              tblName = addNr "Table" 
            , columns = [ ( addNr "id", conT ''Int), (addNr "text", conT ''String) ] }
      db = defaultDB {
              dbName = addNr "testcase"
            , tables = [ table ]
            , statistics = True }
  in createDB db
