{-# OPTIONS_GHC -XTemplateHaskell -XEmptyDataDecls #-}
module Database.InMem.THSplice where
-- TODO tidy up imports 
import Database.InMem.Util
import Database.InMem.Types
import System.IO
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.Writer
import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.Sequence as S
import Data.Char
import Data.Maybe

-- all stuff which can't be defined in TH.hs because of splicing restrictions 

