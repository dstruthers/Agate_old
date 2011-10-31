module VM 
       (
         Inst(..)
       , Op(..)
       , Env
       , execute
       , initialVM
       , initialEnv
       ) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Char (toUpper)
import qualified Data.Map as M
import Expr

data Op = Assign String
        | Constant Expr
        | Lookup String
        | Test { consequent  :: Inst
               , alternative :: Inst
               }

data Inst = Inst { getOp    :: Op
                 , nextInst :: Inst
                 }
          | Halt

data Env = Env { runEnv    :: M.Map String Expr
               , parentEnv :: Maybe Env
               }

data VM = VM { accumulator :: Expr
             , environment :: Env
             }

initialEnv = Env M.empty Nothing
initialVM = VM SNull initialEnv

setVar :: VM -> String -> Expr -> VM
setVar vm varName val = let envMap = runEnv (environment vm)
                            parent = parentEnv (environment vm)
                            newEnv = Env (M.insert varName val envMap) parent
                        in vm { environment = newEnv }

lookup :: String -> Env -> Maybe Expr
lookup key env = let e = runEnv env
                     p = parentEnv env
                 in M.lookup key e `mplus` case p of
                   Just e' -> lookup key e'
                   Nothing -> Nothing

compile :: Expr -> Inst
compile (SSymbol s) = Inst (Lookup s) Halt
compile k = Inst (Constant k) Halt

execute :: VM -> Inst -> Expr
execute vm Halt = accumulator vm
execute vm inst = case getOp inst of
  Assign varName -> let vm' = setVar vm varName (accumulator vm)
                    in execute vm' (nextInst inst)
  
  Constant k -> execute vm { accumulator = k } (nextInst inst)
  
  Lookup s -> case lookup s (environment vm) of
    Just v  -> execute vm { accumulator = v } (nextInst inst)
    Nothing -> SException ("Unbound variable: " ++ s)
    
  Test c a -> if isTrue (accumulator vm)
              then execute vm c
              else execute vm a