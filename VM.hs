module VM 
       (
         Inst(..)
       , Op(..)
       , Env
       , execute
       , defaultVM
       , emptyEnv
       ) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Char (toUpper)
import qualified Data.Map as M
import Expr

data Op = Constant Expr
        | Lookup String

data Inst = Inst { getOp   :: Op
                 , nextInst  :: Inst
                 }
          | Halt

data Env = Env { runEnv    :: M.Map String Expr
               , parentEnv :: Maybe Env
               }

data VM = VM { accumulator :: Expr
             , environment :: Env
             }

defaultVM :: VM
defaultVM = VM SNull emptyEnv

emptyEnv = Env M.empty Nothing

lookup :: String -> Env -> Maybe Expr
lookup key env = let e = runEnv env
                     p = parentEnv env
                 in M.lookup key e `mplus` case p of
                   Just e' -> lookup key e'
                   Nothing -> Nothing

execute :: VM -> Inst -> Expr
execute vm Halt = accumulator vm
execute vm inst = case getOp inst of
  Constant k -> execute vm { accumulator = k } (nextInst inst)
  
  Lookup s -> case lookup s (environment vm) of
    Just v  -> execute vm { accumulator = v } (nextInst inst)
    Nothing -> SException ("Unbound variable: " ++ s)

  _ -> SException "Unknown runtime error"
