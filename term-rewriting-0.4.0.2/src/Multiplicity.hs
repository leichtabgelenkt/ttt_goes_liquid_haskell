module Multiplicity where
import Data.Rewriting.Term
import Data.Rewriting.Term.Type
import Data.Rewriting.Term.Ops as TermOps
import Data.Rewriting.Term.Pretty
import Data.Rewriting.Context
import Data.Rewriting.CriticalPair
import Data.Rewriting.Pos
import Data.Rewriting.Rules
import Data.Rewriting.Rule
import Data.Rewriting.Substitution
import Data.Rewriting.Rule.Type
import Data.Rewriting.Rules.Rewrite
import Data.Rewriting.Problem
import Data.List
import Data.SBV.Internals

type Projection = [(Char, SInteger)]

-- True if x is imbedded in y
subgroup :: Term Char Char -> Term Char Char -> Bool
subgroup x y
  | x == y = True
  | isVar y = False
  | otherwise = or [subgroup x v | v <- handBackArgumentsFromTerm y]

-- True if x is properly imbedded in y
properSubgroup :: Term Char Char -> Term Char Char -> Bool
properSubgroup x y
  | x == y = False
  | isVar y = False
  | otherwise = or [subgroup x v | v <- handBackArgumentsFromTerm y]

isNotProjecting :: Term Char Char -> Projection -> Bool
isNotProjecting (Fun c _) p = Data.List.any (\(symbol, list) -> symbol == c && Data.List.null list) p
isNotProjecting (Var c) p = error "isNotProjecting on variable should never be called"

handBackArgumentsFromTerm :: Term Char Char -> [Term Char Char]
handBackArgumentsFromTerm (Var _) = []
handBackArgumentsFromTerm (Fun _ args) = args

maybeToInt :: Maybe Int -> Int
maybeToInt Nothing = -1
maybeToInt (Just x) = x

isProjectingToArgument :: Term Char Char -> Term Char Char -> Projection -> Bool
isProjectingToArgument x ( Fun c vars ) p = maybeToInt (elemIndex x (handBackArgumentsFromTerm s)) + 1 `Data.List.elem` snd (Data.List.head (Data.List.filter (\(symbol, list) -> symbol == c) p))
  where s = Fun c vars
 

-- Multiplicity of a term in another termisVar
multiplicity :: Int -> Term Char Char -> Term Char Char -> Projection -> Int
multiplicity w s t p
  | s == t && not (isVar s) = if isNotProjecting t p then w else 0
  | s == t && isVar s = w
  | subgroup t s && not (isVar s) = sum [multiplicity w x t p | x <- handBackArgumentsFromTerm s, isProjectingToArgument x s p]
  | otherwise = 0 -- case4 finished
