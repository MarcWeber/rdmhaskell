{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls -XMultiParamTypeClasses -XFunctionalDependencies #-}
module Database.InMem.Types where
import Language.Haskell.TH
import Data.Maybe
import Control.Monad.State

-- indexes such as Map (String,Int) Rec are not supported, you have to use
-- Map String (Map Int Rec) -- why? 
-- They only make sense on uniq constraints or when filtering using ==
data Index  =
  IndexUnique String
  | Index String [Index] -- you can create multi indexes 
  deriving (Eq, Show)

data SetPK = KeepPk
        -- | IncrementPKStartAt Integer -- new pk will be the hightest existing pk number +1 (FIXME: use state and never assign the same id twice? ) 
        | MaxPKPlusOne Integer -- new pk will be the hightest existing pk number +1 
                                -- (you should consider using IncrementPKStartAt instead which will never assign the same number a second time )
        | SetPK ExpQ       -- State DB <primary key> 
        | SetSubPKPlusOne Integer -- the second primary key field will be made uniq (see CD Tracks example)
        | SetPKCustom ExpQ -- arbitrary expression. :: row -> State DB row

data Primary = 
  NoPrimary
  | PrimaryUniq [ String ] -- columns 
                ( SetPK ) -- StateT DB m PKType , returns next primary key, optional
  -- no support for non uniq primary keys yet 
hasPrimary NoPrimary = False
hasPrimary _ = True

type TriggerOnColumnsChanged = Maybe [ String ] -- Nothing = anytime
data Trigger = 
        Delete Type -- :: old -> StateT DB m ()
      | Insert Type -- :: new -> StateT DB m () 
      | DeleteUpdate TriggerOnColumnsChanged Type -- :: old        -> StateT DB m ()
      | InsertUpdate TriggerOnColumnsChanged Type -- ::        new -> StateT DB m () 
      | Update       TriggerOnColumnsChanged Type -- :: old -> new -> StateT DB m ()
  deriving (Eq)

data Table = Table {
      tblName :: String
    , columns :: [ ( String, TypeQ ) ]
    , primary :: Primary -- which columns form the primary key 
    , indexes :: [ Index ] -- the primary index will be added for you automatically 
                          -- allIndexes returns all true indexes (with primary key if given) 
    , checks :: [ TypeQ ] -- row -> ReadRT DB m ()
                         -- will be transformed to triggers with no action but failing
    , triggers :: [ Trigger ]
    , tblStates :: [ ( String, TypeQ, ExpQ ) ] -- states only depending on actions on this table
                                           -- name, Type, initialization function (is undefined by default)
  }

data DB = DB {
    dbName :: String
    , tables :: [ Table ]
    , dbStates :: [ ( String, TypeQ, ExpQ ) ] -- states (if you need the same sequence as primary id generator for two tables )
    , statistics :: Bool -- count statistics (insertions, deletions, updates TODO) (would be nice to have: estimated size in memory .. ?) 
    , useTVars :: Bool -- True will result in data DB = DB { table1 :: TVar Table1, state1 :: TVar state1, ... }
  }

-- instances for convinience 
class TableWithPrimaryKey pk row table db | table -> db, row -> pk, row -> table, table -> row where 
  -- get or set the primary key of a row 
  pk :: row -> pk
  setPK :: row -> pk -> row

  -- manipulate db 
  insert :: (Monad m) => row -> State db (m pk)
  updateByPK :: (Monad m) => row -> pk -> State db (m ())
  deleteByPK :: pk -> State db ()
  lookup :: (Monad m) => pk -> db -> m row

-- ========== defaults provided for convinience ===================== 
defaultDB = DB {
    dbName =  error "please set a database name"
    , tables = error "please set database tables/relations"
    , dbStates = []
    , statistics = True
    , useTVars = False
  }

defaultTable = Table {
  tblName = error "please set a table/relation name"
  , columns = error "please set table columns / tuple field types of relation"
  , primary = NoPrimary
  , indexes = []
  , checks = []
  , triggers = []
  , tblStates =  []
  }
