module Expr 
       (
         Expr(..)
       , fromList
       , isPair
       , isList
       , isNull
       , isTrue
       , isFalse
       , car
       , cdr
       , len
       , Expr.parse
       ) where

import Data.Char (toUpper)
import Text.ParserCombinators.Parsec

data Expr = Symbol String
          | Number Double
          | String String
          | Bool Bool
          | Pair Expr Expr
          | Null
          | Exception String

isPair :: Expr -> Bool
isPair (Pair _ _) = True
isPair _          = False

isList :: Expr -> Bool
isList Null        = True
isList (Pair _ p2) = isList p2
isList _           = False

isNull :: Expr -> Bool
isNull Null = True
isNull _    = False

isTrue :: Expr -> Bool
isTrue = not . isFalse

isFalse :: Expr -> Bool
isFalse (Bool False) = True
isFalse _            = False

fromList :: [Expr] -> Expr
fromList = foldr Pair Null

car (Pair x _) = x
cdr (Pair _ y) = y

len :: Expr -> Int
len Null        = 0
len (Pair x xs) = 1 + len xs

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

parseExprAux = try parseBool
               <|> try parseNumber
               <|> try parseSymbol
               <|> try parseString
               <|> try parseList

parseList = do char '('
               skipMany space
               x <- parseExprAux `sepEndBy` (many1 space)
               char ')'
               return $ fromList x

parseExpr = do skipMany space
               x <- parseExprAux
               skipMany space
               eof
               return x

parse :: String -> Expr
parse input = case (Text.ParserCombinators.Parsec.parse parseExpr "" input) of
  Right x -> x
  Left e -> Exception (show e)

instance Show Expr where
  show (Symbol s)   = map toUpper s
  show (Number n)   = show n
  show (String s)   = show s
  show (Bool True)  = "#t"
  show (Bool False) = "#f"
  show p@(Pair _ _) = "(" ++ showPair p ++ ")"
    where showPair (Pair p1 p2)
            | isPair p2 = show p1 ++ " " ++ showPair p2
            | isNull p2 = show p1
            | otherwise = show p1 ++ " . " ++ show p2
  show Null = "()"
  show (Exception s) = "**Exception: " ++ s

instance Eq Expr where
  (Symbol a) == (Symbol b) = (map toUpper a) == (map toUpper b)
  (Number a) == (Number b) = a == b
  (String a) == (String b) = a == b
  (Bool a) == (Bool b) = a == b
  (Pair a b) == (Pair c d) = a == c && b == d
  Null == Null = True
  (Exception a) == (Exception b) = a == b
  _ == _ = False