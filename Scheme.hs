module Main (repl) where

import Expr
import VM
import System.IO

repl = runRepl initialVM
  where runRepl vm = do
          putStr "scheme> "
          hFlush stdout
          source <- getLine
          let (result, vm') = execute vm $ compile $ parse source
          putStrLn $ show result
          runRepl vm'

main = repl
