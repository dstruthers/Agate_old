module VM 
       ( compile
       , execute
       , initialVM
       ) where

import Prelude hiding (lookup)
import Control.Monad
import Data.Char (toUpper)
import qualified Data.Map as Map
import Types
import Expr

initialEnv = Env initialMap Nothing
  where forms = [("DEFINE", SpecialForm compileDefine)
                ,("IF", SpecialForm compileIf)
                ,("LAMBDA", SpecialForm compileLambda)
                ,("QUOTE", SpecialForm compileQuote)
                ]
        initialMap   = foldr addToMap Map.empty forms
        addToMap (sym, binding) m = Map.insert sym binding m
        
initialVM = VM initialAcc initialEnv initialArgs initialStack
  where initialAcc = Null
        initialArgs = []
        initialStack = Nothing

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
  | isList expr == False = [Constant expr]
  | otherwise = case car expr of
    Symbol s -> case lookup (map toUpper s) (environment vm) of
      Just (SpecialForm compiler) -> compiler vm expr
      _ -> compileApply vm expr
    _ -> compileApply vm expr

compileApply vm expr =
  let fn = compile vm (car expr)
  in case head fn of
    Constant (SpecialForm compiler) -> compiler vm expr
    _ -> compileArgs vm (cdr expr) ++ fn ++ [Apply]
    where compileArgs vm Null = []
          compileArgs vm (Pair arg rest) = compile vm arg ++ [Argument] ++ compileArgs vm rest

compileDefine vm expr
  | len expr /= 3 = compilationError "DEFINE requires exactly 2 arguments"
  | otherwise = case (arg 1 expr) of
    Symbol sym -> let value = compile vm (arg 2 expr)
                  in value ++ [Assign sym]
                    
    _          -> compilationError "Argument 1 must be a symbol"
                          
compileIf vm expr
  | len expr < 3 || len expr > 4 = compilationError "IF requires 2 or 3 arguments"
  | otherwise = let test = compile vm (arg 1 expr)
                    consequent = compile vm (arg 2 expr)
                    alternative = if len expr > 3
                                  then compile vm (arg 3 expr)
                                  else [Constant Null]
                in test ++ [Test consequent alternative]

compileLambda vm expr
  | len expr /= 3 = compilationError "LAMBDA requires exactly 2 arguments"
  | otherwise = let params = paramNames (arg 1 expr)
                    env    = environment vm
                    body   = compile vm (arg 2 expr)
                in case params of 
                  Just p  -> [Constant (Procedure p env body)]
                  Nothing -> compilationError "Argument 1 to LAMBDA must be a list of symbols"

    where paramNames Null = Just []
          paramNames (Pair (Symbol p) ps) = liftM2 (:) (Just p) (paramNames ps)
          paramNames _ = Nothing

compileQuote vm expr
  | len expr > 2 = compilationError "QUOTE requires exactly 1 argument"
  | otherwise    = [Constant (arg 1 expr)]

compilationError msg = [Constant (Exception ("Compilation error: " ++ msg))]

arg :: Int -> Expr -> Expr    
arg 0 (Pair x _) = x
arg n (Pair _ y) = arg (n - 1) y
arg _ _ = Exception "Premature end of list, or object not a list"
    
execute :: VM -> Compiled -> (Expr, VM)
execute vm [] = (accumulator vm, vm)
execute vm (inst:nextInst) = case inst of
  Apply -> execute vm { accumulator = head (arguments vm) } nextInst
  
  Argument -> let argStack = arguments vm
                  newArgs  = argStack ++ [accumulator vm]
                  vm'      = vm { arguments = newArgs }
              in execute vm' nextInst
                 
  Assign varName -> let varName' = map toUpper varName
                        vm' = setVar vm varName' (accumulator vm)
                    in execute vm' nextInst
  
  Constant k -> execute vm { accumulator = k } nextInst
  
  Frame ret -> execute vm { stack = Just (ret, vm) } nextInst

  Lookup s -> let s' = map toUpper s
              in case lookup s' (environment vm) of
                Just v  -> execute vm { accumulator = v } nextInst
                Nothing -> (Exception ("Unbound variable: " ++ s'), vm)
    
  Return -> case stack vm of
                 Just (next, newState) ->
                   execute vm { environment = environment newState
                              , arguments   = arguments newState
                              , stack       = stack newState            
                              } next
                 Nothing -> (Exception "Return from empty call stack", vm)

  Test consequent alternative -> if isTrue (accumulator vm)
                                 then execute vm consequent
                                 else execute vm alternative
