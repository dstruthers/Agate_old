module Main (repl) where

import Expr
import VM
import System.IO

main = repl

repl = runRepl initialVM
  where runRepl vm = do
          source <- prompt "scheme> "
          let (result, vm') = eval vm source
          putStrLn $ show result
          runRepl vm'
        
prompt p = putStr p >> hFlush stdout >> getLine
eval vm src = execute vm $ compile vm $ parse src

