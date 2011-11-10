module Parse 
       ( Parse.parse
       ) where

import Data.Char (toUpper)
import Text.ParserCombinators.Parsec
import Types

parseNumber = do sign <- option "" (string "-")
                 number <- many1 digit
                 decimal <- option "0" (string "." >> many1 digit)
                 return $ Number (read (sign ++ number ++ "." ++ decimal))

parseString = do char '"'
                 s <- many (noneOf "\"")
                 char '"'
                 return $ String s

parseSymbol = do f <- firstAllowed
                 r <- many (firstAllowed <|> digit)
                 return $ case (map toUpper (f:r)) of 
                   "#T" -> Bool True
                   "#F" -> Bool False
                   _    -> Symbol (f:r)
  where firstAllowed = oneOf "+-*/=!@#$%^&_<>?" <|> letter

parseAnyExpr = try parseSymbolWithNumber 
               <|> try parseNumber
               <|> try parseSymbol
               <|> try parseString
               <|> try parseList
               
-- This parser finds symbols like the incrementer 1+
parseSymbolWithNumber = do d <- many1 digit
                           o <- many1 (letter <|> digit <|> oneOf "+-*/=!@#$%^&_<>?")
                           return $ Symbol (d ++ o)

parseList = do char '('
               skipMany space
               x <- parseAnyExpr `sepEndBy` (many1 space)
               char ')'
               return $ fromList x

parseExpr = do skipMany space
               x <- parseAnyExpr
               skipMany space
               eof
               return x

parse :: String -> Expr
parse input = case (Text.ParserCombinators.Parsec.parse parseExpr "" input) of
  Right x -> x
  Left e -> Exception (show e)
