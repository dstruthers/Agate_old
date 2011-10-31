module VM 
       (
         Inst(..)
       , Env(..)
       , VM(..)
       , compile
       , execute
       , initialVM
       , initialEnv
       , setVar
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
compile expr
  | isNull expr = [Constant SNull]
  | isList expr == False = [Constant expr]
  | otherwise = case car expr of
    SSymbol s -> case (map toUpper s) of 
      "DEFINE" -> compileDefine expr
      "IF" -> compileIf expr      
      "QUOTE" -> [Constant (arg 1 expr)]
      _ -> compilationError "unknown error"

compileDefine expr = case (arg 1 expr) of
  SSymbol sym -> let value = compile (arg 2 expr)
                 in value ++ [Assign sym]
                    
  _           -> compilationError "Argument 1 must be a symbol"
                          
compileIf expr = let test = compile (arg 1 expr)
                     consequent = compile (arg 2 expr)
                     alternative = if len expr > 3
                                   then compile (arg 3 expr)
                                   else [Constant SNull]
                 in test ++ [Test consequent alternative]

compilationError msg = [Constant (SException ("Compilation error: " ++ msg))]

arg :: Int -> Expr -> Expr    
arg 0 (SPair x _) = x
arg n (SPair _ y) = arg (n - 1) y
arg _ _ = SException "Premature end of list, or object not a list"
    
execute :: VM -> Compiled -> (Expr, VM)
execute vm [] = (accumulator vm, vm)
execute vm (inst:nextInst) = case inst of
  Assign varName -> let varName' = map toUpper varName
                        vm' = setVar vm varName' (accumulator vm)
                    in execute vm' nextInst
  
  Constant k -> execute vm { accumulator = k } nextInst
  
  Lookup s -> let s' = map toUpper s
              in case lookup s' (environment vm) of
                Just v  -> execute vm { accumulator = v } nextInst
                Nothing -> (SException ("Unbound variable: " ++ s'), vm)
    
  Test consequent alternative -> if isTrue (accumulator vm)
                                 then execute vm consequent
                                 else execute vm alternative
