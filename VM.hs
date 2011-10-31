module VM 
       (
         Inst(..)
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

data Inst = Assign String
          | Constant Expr
          | Lookup String
          | Test { consequent  :: Compiled
                 , alternative :: Compiled
                 }
type Compiled = [Inst]

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

compile :: Expr -> Compiled
compile (SSymbol s) = [Lookup s]
compile k = [Constant k]

execute :: VM -> Compiled -> Expr
execute vm [] = accumulator vm
execute vm (inst:nextInst) = case inst of
  Assign varName -> let vm' = setVar vm varName (accumulator vm)
                    in execute vm' nextInst
  
  Constant k -> execute vm { accumulator = k } nextInst
  
  Lookup s -> case lookup s (environment vm) of
    Just v  -> execute vm { accumulator = v } nextInst
    Nothing -> SException ("Unbound variable: " ++ s)
    
  Test consequent alternative -> if isTrue (accumulator vm)
                                 then execute vm consequent
                                 else execute vm alternative
