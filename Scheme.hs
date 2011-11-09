module Main (repl) where

import Types
import Parse
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
eval vm src = execute vm { nextOp = compile vm (parse src) Exit }
--eval vm src = (String $ show $ compile vm (parse src) Exit, vm)

