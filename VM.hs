module VM
       ( compile
       , execute
       , initialVM
       ) where

import Prelude hiding (lookup)
import Control.Monad (liftM2, mplus)
import Data.Char (toUpper)
import qualified Data.Map as Map
import Types
import Parse

emptyEnv = Env Map.empty Nothing

initialEnv :: Env
initialEnv = Env initialMap Nothing
  where forms = [("DEFINE", SpecialForm compileDefine)
                ,("IF", SpecialForm compileIf)
                ,("LAMBDA", SpecialForm compileLambda)
                ,("QUOTE", SpecialForm compileQuote)

                ,("CAR", Procedure ["X"] emptyEnv (Car Return))
                ,("CDR", Procedure ["X"] emptyEnv (Cdr Return))
                ,("CONS", Procedure ["X", "Y"] emptyEnv (Cons Return))
                ]
        initialMap = foldr addToMap Map.empty forms
        addToMap (sym, binding) m = Map.insert sym binding m

newEnv :: Env -> Env
newEnv e = Env Map.empty (Just e)

bindEnv :: Env -> String -> Expr -> Env
bindEnv e k v = Env (Map.insert k v (getMap e)) (parentEnv e)

initialVM = VM op acc env args stack
  where op    = Exit
        acc   = Null
        env   = initialEnv
        args  = []
        stack = Nothing

lookup :: String -> Env -> Maybe Expr
lookup key env = let m = getMap env
                     p = parentEnv env
                 in Map.lookup key m `mplus` case p of
                   Just m' -> lookup key m'
                   Nothing -> Nothing

bindVar :: VM -> String -> Expr -> VM
bindVar vm sym val = let map    = getMap (environment vm)
                         parent = parentEnv (environment vm)
                         env    = Env (Map.insert sym val map) parent
                     in vm { environment = env }

compile :: Compiler
compile vm expr next
  | isList expr = case car expr of
    Symbol s -> case lookup (map toUpper s) (environment vm) of
      Just (SpecialForm compiler) -> compiler vm expr next
      _ -> compileApply vm expr next
    _ -> compileApply vm expr next
  
  | otherwise = case expr of 
    Symbol s  -> Lookup s next
    otherwise -> Constant expr next

compileApply vm expr next =
  case car expr of
    Exception _ -> compilationError "Ill-formed expression"
    _ ->
      let fn = compile vm (car expr) next
      in case fn of
        Constant (SpecialForm compiler) n -> compiler vm expr n
        Constant (Procedure p e b) n -> 
          let compiled = compileArgs vm (cdr expr) (Constant (Procedure p e b) (Apply n))
          in case next of
            Return -> compiled
            _      -> Frame next compiled
        
        Lookup sym n ->
          let compiled = compileArgs vm (cdr expr) (Lookup sym (Apply n))
          in case next of
            Return -> compiled
            _      -> Frame next compiled
    
    where compileArgs _ Null n' = n'
          compileArgs vm (Pair arg rest) next =
            compile vm arg (Argument (compileArgs vm rest next))

compileDefine vm expr next
  | len expr /= 3 = compilationError "DEFINE requires exactly 2 arguments"
  | otherwise = 
    case (arg 1 expr) of
      Symbol sym -> compile vm (arg 2 expr) (Assign sym next)
      _          -> compilationError "Argument 1 must be a symbol"

compileIf vm expr next
  | len expr < 3 || len expr > 4 = compilationError "IF requires 2 or 3 arguments"
  | otherwise =
    let consequence = compile vm (arg 2 expr) next
        alternative = if len expr > 3
                      then compile vm (arg 3 expr) next
                      else Constant Null next
    in compile vm (arg 1 expr) (Test consequence alternative)

compileLambda vm expr next
  | len expr /= 3 = compilationError "LAMBDA requires exactly 2 arguments"
  | otherwise =
    let params = paramNames (arg 1 expr)
        env    = environment vm
        body   = compile vm (arg 2 expr) Return
    in case params of
      Just p -> Constant (Procedure p env body) next
      _      -> compilationError "Argument 1 to LAMBDA must be a list of symbols"
      
      where paramNames Null = Just []
            paramNames (Pair (Symbol p) ps) = liftM2 (:) (Just p) (paramNames ps)
            paramNames _ = Nothing

compileQuote vm expr next
  | len expr > 2 = compilationError "QUOTE requires exactly 1 argument"
  | otherwise    = Constant (arg 1 expr) next

compilationError msg = Constant (Exception ("Compilation error: " ++ msg)) Exit

arg :: Int -> Expr -> Expr
arg 0 (Pair x _) = x
arg n (Pair _ y) = arg (n - 1) y
arg _ _ = Exception "Premature end of list, or object not a list"

execute :: VM -> (Expr, VM)
execute vm = case nextOp vm of
  Apply next ->
    case accumulator vm of
      Procedure params env body ->
        let env' = mkEnvWithArgs (newEnv (environment vm))
                                 params
                                 (arguments vm)
                                           
            mkEnvWithArgs e []     _  = e
            mkEnvWithArgs e (p:ps) as =
                mkEnvWithArgs (bindEnv e (map toUpper p) (head as)) ps (tail as)
                
        in execute vm { nextOp = body, environment = env' }
      
      _ -> (Exception (show (accumulator vm) ++ ": not applicable"), vm)
  
  Argument next ->
    let args  = arguments vm
        args' = args ++ [accumulator vm]
        vm'   = vm { nextOp = next, arguments = args' }
    in execute vm'

  Assign sym next ->
    let sym' = (map toUpper sym)
        vm'  = bindVar vm sym' (accumulator vm)
    in execute vm' { nextOp = next }

  Car next ->
    case head (arguments vm) of
      Pair p1 _ -> execute vm { nextOp = next, accumulator = p1 }
      _         -> (Exception "Pair expected", vm)
      
  Cdr next ->
    case head (arguments vm) of
      Pair _ p2 -> execute vm { nextOp = next, accumulator = p2 }
      _         -> (Exception "Pair expected", vm)
      
  Cons next ->
    let p1 = head (arguments vm)
        p2 = arguments vm !! 1
    in execute vm { nextOp = next, accumulator = Pair p1 p2 }
  
  Constant k next ->
    execute vm { nextOp = next, accumulator = k }
  
  Exit ->
    (accumulator vm, vm)  
  
  Frame ret next ->
    let vm' = vm { nextOp = next
                 , arguments = []
                 , stack = Just vm { nextOp = ret }
                 }
    in execute vm'
  
  Lookup sym next ->
    let s = map toUpper sym
    in case lookup s (environment vm) of
      Just v  -> execute vm { nextOp = next, accumulator = v }
      Nothing -> (Exception ("Unbound variable: " ++ s), vm)
  
  Return ->
    case stack vm of
      Just stackVM ->
        let vm' = vm { nextOp = nextOp stackVM
                     , environment = environment stackVM
                     , arguments = arguments stackVM
                     , stack = stack stackVM
                     }
        in execute vm'
      _ -> (Exception "Return from empty stack", vm)
  
  Test consequence alternative -> 
    if isTrue (accumulator vm)
    then execute vm { nextOp = consequence }
    else execute vm { nextOp = alternative }
