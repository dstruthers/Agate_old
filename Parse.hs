module Parse 
       ( Parse.parse
       ) where

import Data.Char (toUpper)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Types

parseBool = do char '#'
               v <- oneOf "TFtf"
               return $ if toUpper v == 'T' then Bool True else Bool False
               
parseNumber = do sign <- option "" (string "-")
                 number <- many1 digit
                 decimal <- option "0" (string "." >> many1 digit)
                 return $ Number (read (sign ++ number ++ "." ++ decimal))

parseString = do char '"'
                 s <- many (noneOf "\"")
                 char '"'
                 return $ String s

parseSymbol = do f <- firstAllowed
                 r <- many (firstAllowed <|> digit <|> oneOf "!?")
                 return $ Symbol (f:r)
  where firstAllowed = oneOf "+-*/" <|> letter

parseAnyExpr = try parseBool
               <|> try parseNumber
               <|> try parseSymbol
               <|> try parseString
               <|> try parseList

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
