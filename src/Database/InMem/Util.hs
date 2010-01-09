module Database.InMem.Util where
import Control.Monad
import Language.Haskell.TH
import Control.Monad.Writer
import Data.Char

whenM a b =
  a >>= \p -> when p b

mkNameP = VarP . mkName
mkNameE = varE . mkName

recUpdE' :: Name -> Name -> ExpQ -> ExpQ
recUpdE' v f n = do n' <- n 
                    recUpdE (varE v) ([ return (f, n')])
