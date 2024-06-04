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
import Data.SBV
import Data.SBV.List
import Data.List
import Data.SBV.Internals

-- everything starting with s* is meant to use proper form to be used in an SMT-Solver
-- therefore the recursively should only use other s* - functions
-- furthermore the use Projection instead of Multiprojection to differentiate 
-- (later will Projection also use a SInteger instead of a list) 


type Multiprojection = [(Char, [Integer])]

type Projection = [(Char, SInteger)]

-- True if x is imbedded in y
subgroup :: Term Char Char -> Term Char Char -> Bool
subgroup x y
  | x == y = True
  | isVar y = False
  | otherwise = or [subgroup x v | v <- handBackArgumentsFromTerm y]

sSubgroup :: Term Char Char -> Term Char Char -> Bool
sSubgroup = subgroup

-- True if x is properly imbedded in y
properSubgroup :: Term Char Char -> Term Char Char -> Bool
properSubgroup x y
  | x == y = False
  | isVar y = False
  | otherwise = or [subgroup x v | v <- handBackArgumentsFromTerm y]

sProperSubgroup :: Term Char Char -> Term Char Char -> Bool
sProperSubgroup = properSubgroup

isNotProjecting :: Term Char Char -> Multiprojection -> Bool
isNotProjecting (Fun c _) p = Data.List.any (\(symbol, list) -> symbol == c && Data.List.null list) p
isNotProjecting (Var c) p = error "isNotProjecting on variable should never be called"

sIsNotProjecting :: Term Char Char -> Projection -> SBool
sIsNotProjecting (Fun c _) p = sAny (\(symbol, idx) -> (literal symbol) .== (literal c) .&& idx .== -1) p
sIsNotProjecting (Var c) p = error "isNotProjecting on variable should never be called"

handBackArgumentsFromTerm :: Term Char Char -> [Term Char Char]
handBackArgumentsFromTerm (Var _) = []
handBackArgumentsFromTerm (Fun _ args) = args

sHandBackArgumentsFromTerm :: Term Char Char -> [Term Char Char]
sHandBackArgumentsFromTerm = handBackArgumentsFromTerm

maybeToInt :: Maybe Int -> Integer
maybeToInt Nothing = -1
maybeToInt (Just x) = fromIntegral x

sMaybeToInt :: Maybe Int -> SInteger
sMaybeToInt x = literal (maybeToInt x)

isProjectingToArgument :: Term Char Char -> Term Char Char -> Multiprojection -> Bool
isProjectingToArgument x ( Fun c vars ) p = maybeToInt (elemIndex x (handBackArgumentsFromTerm s)) + 1 `Data.List.elem` snd (Data.List.head (Data.List.filter (\(symbol, list) -> symbol == c) p))
  where s = Fun c vars
 
sIsProjectingToArgument :: Term Char Char -> Term Char Char -> Projection -> SBool
sIsProjectingToArgument x (Fun c vars) p =
  let s = Fun c vars
      index = sFromIntegral $ sMaybeToInt (elemIndex x (sHandBackArgumentsFromTerm s)) + 1
  in sAny (\(symbol, idx) -> literal (symbol == c) .&& index .== idx) p

-- Multiplicity of a term in another term
multiplicity :: Integer -> Term Char Char -> Term Char Char -> Multiprojection -> Integer
multiplicity w s t p
  | s == t && not (isVar s) = if isNotProjecting t p then w else 0
  | s == t && isVar s = w
  | subgroup t s && not (isVar s) = sum [multiplicity w x t p | x <- handBackArgumentsFromTerm s, isProjectingToArgument x s p]
  | otherwise = 0

findProjectingIndex :: Projection -> Term Char Char -> SInteger
findProjectingIndex [] _ = literal (-1)
findProjectingIndex ((k, v):xs) s@(Fun c _) = if k == c then v else findProjectingIndex xs s

sMultiplicity :: SInteger -> Term Char Char -> Term Char Char -> Projection -> SInteger
sMultiplicity w s t p
  | s == t && not (isVar s) = ite (sIsNotProjecting t p) w (literal 0)
  | s == t && isVar s = w
  | sSubgroup t s && not (isVar s) = help (sHandBackArgumentsFromTerm s) (findProjectingIndex p s) 0--(\x -> sMultiplicity w x t p) $ (sHandBackArgumentsFromTerm s) !! fromIntegral (findIndexTest (sHandBackArgumentsFromTerm s) s p 0)
  | otherwise = literal 0
  where help [] _ _ = literal 0
        help (x:xs) i y = ite (i .== literal y) (sMultiplicity w x t p) (help xs i (y+1))


findIndexTest :: [Term Char Char] -> Term Char Char -> Projection -> Integer -> Integer
findIndexTest (x:xs) s p y = ite (sIsProjectingToArgument x s p) (y) (findIndexTest xs s p (y+1))
findIndexTest [] _ _ y = y