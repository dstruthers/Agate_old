module Main (repl) where

import Types
import qualified Parse
import VM
import System.IO
import Text.ParserCombinators.Parsec

data ReplOp = Compile String

main = repl

repl = runRepl initialVM
  where runRepl vm = do
          source <- prompt "Agate> "
          let (result, vm') = replParse vm source
          putStrLn $ show result
          runRepl vm'
        
prompt p = putStr p >> hFlush stdout >> getLine
eval vm src = execute vm { nextOp = compile vm (Parse.parse src) Exit }

replParse vm input = case parse parseCmdOrExpr "" input of
  Right result -> case result of
    Compile src -> (String (show (compile vm (Parse.parse src) Exit)), vm)
  
  Left _  -> execute vm { nextOp = compile vm (Parse.parse input) Exit }

parseCmdOrExpr = try parseCompile

parseCompile = do skipMany space
                  string ":compile "
                  src <- many anyChar
                  return $ Compile src