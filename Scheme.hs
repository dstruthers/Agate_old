module Scheme
       (
         Expr(..)
       , parse
       , fromList
       , module VM
       ) where

import Expr
import VM

repl = runRepl initialVM
  where runRepl vm = do
          putStr "scheme> "
          source <- getLine
          let parsed = parse source
          let compiled = compile parsed
          let (result, vm') = execute initialVM compiled
          putStrLn $ show result
          runRepl vm'