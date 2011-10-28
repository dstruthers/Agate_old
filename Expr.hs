module Expr where

import Data.Char (toUpper)

data Expr = SSymbol String
          | SNumber Double
          | SString String
          | SBool Bool
          | SPair Expr Expr
          | SNull
          | SException String

isList :: Expr -> Bool
isList (SPair _ _) = True
isList _           = False

isNull :: Expr -> Bool
isNull SNull = True
isNull _     = False

isTrue :: Expr -> Bool
isTrue = not . isFalse

isFalse :: Expr -> Bool
isFalse (SBool False) = True
isFalse _             = False

fromList :: [Expr] -> Expr
fromList [] = SNull
fromList (x:xs) = SPair x (fromList xs)

instance Show Expr where
  show (SSymbol s)   = map toUpper s
  show (SNumber n)   = show n
  show (SString s)   = show s
  show (SBool True)  = "#t"
  show (SBool False) = "#f"
  show p@(SPair _ _) = "(" ++ showPair p ++ ")"
    where showPair (SPair p1 p2)
            | isList p2 = show p1 ++ " " ++ showPair p2
            | isNull p2 = show p1
            | otherwise = show p1 ++ " . " ++ show p2
  show SNull = "()"
  show (SException s) = "**Exception: " ++ s