module Types where 

import qualified Data.Map as Map
import Data.Char (toUpper)

data Op = Apply Op
        | Argument Op
        | Assign String Op
        | Car Op
        | Cdr Op
        | Cons Op
        | Constant Expr Op
        | Equal Op
        | Exit
        | Frame Op Op
        | Lookup String Op
        | Return
        | Test { consequence  :: Op
               , alternative :: Op
               }
        deriving (Eq, Show)

data Env = Env { getMap    :: Map.Map String Expr
               , parentEnv :: Maybe Env
               } deriving (Eq, Show)
           
data Expr = Symbol String
          | Number Double
          | String String
          | Bool Bool
          | Pair Expr Expr
          | Null
          | Procedure { params :: [String]
                      , env :: Env
                      , body :: Op
                      }
          | SpecialForm Compiler
          | Exception String

data VM = VM { nextOp      :: Op
             , accumulator :: Expr
             , environment :: Env
             , arguments   :: [Expr]
             , stack       :: Maybe VM
             } deriving (Show)

type Compiler = VM -> Expr -> Op -> Op

instance Eq Expr where
  (Symbol a) == (Symbol b) = (map toUpper a) == (map toUpper b)
  (Number a) == (Number b) = a == b
  (String a) == (String b) = a == b
  (Bool a) == (Bool b) = a == b
  (Pair a b) == (Pair c d) = a == c && b == d
  Null == Null = True
  (Procedure p1 e1 b1) == (Procedure p2 e2 b2) =
    p1 == p2 && e1 == e2 && b1 == b2
  (Exception a) == (Exception b) = a == b
  _ == _ = False

instance Show Expr where
  show (Symbol s)   = map toUpper s
  show (Number n)   = format n
    where format n = let n' = round n
                     in if (toRational n') == toRational n
                        then show n'
                        else show n
  show (String s)   = show s
  show (Bool True)  = "#t"
  show (Bool False) = "#f"
  show p@(Pair _ _) = "(" ++ showPair p ++ ")"
    where showPair (Pair p1 p2)
            | isPair p2 = show p1 ++ " " ++ showPair p2
            | isNull p2 = show p1
            | otherwise = show p1 ++ " . " ++ show p2
  show (Procedure _ _ b) = "#<compiled procedure -- " ++ show b ++ ">"
  show (SpecialForm _) = "#<special form>"
  show Null = "()"
  show (Exception s) = "**Exception: " ++ s

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

toList :: Expr -> [Expr]
toList (Pair x xs) = x : toList xs

car (Pair x _) = x
car _ = Exception "Pair expected"

cdr (Pair _ y) = y
cdr _ = Exception "Pair expected"

len :: Expr -> Int
len Null        = 0
len (Pair x xs) = 1 + len xs

{-next :: Op -> Op
next (Apply o) = o
next (Argument o) = o
next (Assign _ o) = o
next (Constant _ o) = o
next (Frame _ o) = o
next (Lookup _ o) = o
-}
