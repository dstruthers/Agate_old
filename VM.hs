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
import qualified Data.Map as Map
import Expr

data VM = VM { accumulator :: Expr
             , environment :: Env
             , arguments   :: [Expr]
             }

initialEnv = Env Map.empty Nothing
initialVM = VM Null initialEnv []

setVar :: VM -> String -> Expr -> VM
setVar vm varName val = let envMap = runEnv (environment vm)
                            parent = parentEnv (environment vm)
                            newEnv = Env (Map.insert varName val envMap) parent
                        in vm { environment = newEnv }


lookup :: String -> Env -> Maybe Expr
lookup key env = let e = runEnv env
                     p = parentEnv env
                 in Map.lookup key e `mplus` case p of
                   Just e' -> lookup key e'
                   Nothing -> Nothing

compile :: VM -> Expr -> Compiled
compile _ (Symbol s) = [Lookup s]
compile vm expr
  | isNull expr = [Constant Null]
  | isList expr == False = [Constant expr]
  | otherwise = case car expr of
    Symbol s -> case (map toUpper s) of 
      "DEFINE" -> compileDefine vm expr
      "IF" -> compileIf vm expr
      "LAMBDA" -> compileLambda vm expr
      "QUOTE" -> [Constant (arg 1 expr)]
      _ -> compilationError "unknown error"

compileDefine vm expr = case (arg 1 expr) of
  Symbol sym -> let value = compile vm (arg 2 expr)
                in value ++ [Assign sym]
                    
  _          -> compilationError "Argument 1 must be a symbol"
                          
compileIf vm expr = let test = compile vm (arg 1 expr)
                        consequent = compile vm (arg 2 expr)
                        alternative = if len expr > 3
                                      then compile vm (arg 3 expr)
                                      else [Constant Null]
                    in test ++ [Test consequent alternative]

compileLambda vm expr =
  if len expr == 3
  then let params = paramNames (arg 1 expr)
           env    = environment vm
           body   = compile vm (arg 2 expr)
       in case params of 
         Just p  -> [Constant (Procedure p env body)]
         Nothing -> compilationError "Argument 1 to Lambda must be a list of symbols"
  else compilationError "Lambda requires exactly 2 arguments"
    where paramNames Null = Just []
          paramNames (Pair (Symbol p) ps) = liftM2 (:) (Just p) (paramNames ps)
          paramNames _ = Nothing

compilationError msg = [Constant (Exception ("Compilation error: " ++ msg))]

arg :: Int -> Expr -> Expr    
arg 0 (Pair x _) = x
arg n (Pair _ y) = arg (n - 1) y
arg _ _ = Exception "Premature end of list, or object not a list"
    
execute :: VM -> Compiled -> (Expr, VM)
execute vm [] = (accumulator vm, vm)
execute vm (inst:nextInst) = case inst of
  Argument -> let argStack = arguments vm
                  newArgs  = argStack ++ [accumulator vm]
                  vm'      = vm { arguments = newArgs }
              in execute vm' nextInst
                 
  Assign varName -> let varName' = map toUpper varName
                        vm' = setVar vm varName' (accumulator vm)
                    in execute vm' nextInst
  
  Constant k -> execute vm { accumulator = k } nextInst
  
  Lookup s -> let s' = map toUpper s
              in case lookup s' (environment vm) of
                Just v  -> execute vm { accumulator = v } nextInst
                Nothing -> (Exception ("Unbound variable: " ++ s'), vm)
    
  Test consequent alternative -> if isTrue (accumulator vm)
                                 then execute vm consequent
                                 else execute vm alternative
