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
          let (result, vm') = execute vm $ compile $ parse source
          putStrLn $ show result
          runRepl vm'

main = repl
