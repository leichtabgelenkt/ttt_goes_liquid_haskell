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


instance (Show f, Show v, Show v') => Show (Reduct f v v') where
  show (Reduct result pos rule subst) =
    "Reduct { result = " ++ show result
    ++ ", pos = " ++ show pos
    ++ ", rule = " ++ show rule
    ++ ", subst = " ++ show subst
    ++ " }"

splitString :: String -> [String]
splitString []     = []
splitString (x:xs) = [x] : splitString xs

collectFunctionsInRule :: Rule Char Char -> [String]
collectFunctionsInRule rule = splitString (TermOps.funs (lhs rule) ++ TermOps.funs (rhs rule))


collectFunctionsInRuleList :: [Rule Char Char] -> [String]
collectFunctionsInRuleList rules = nub (splitString (concatMap (\rule -> TermOps.funs (lhs rule) ++ TermOps.funs (rhs rule)) rules))

getAllFamilies :: [Rule Char Char] -> [[String]]
getAllFamilies rules = permutations(collectFunctionsInRuleList rules)

outermostSymbol :: Term Char Char -> [Char]
outermostSymbol (Var _) = error "Variable has no outermost symbol"
outermostSymbol (Fun f _) = [f]

compareOutermostSymbols :: Term Char Char -> Term Char Char -> [String]
compareOutermostSymbols term1 term2 = splitString (outermostSymbol term1 ++ outermostSymbol term2)

findOrderings :: [String] -> [[String]] -> [[String]]
findOrderings _ [] = []
findOrderings [x, y] allOrderings =
  filter (\ordering -> isBefore ordering x y) allOrderings
  where
    isBefore :: Eq a => [a] -> a -> a -> Bool
    isBefore ordering a b =
      case (a `elemIndex` ordering, b `elemIndex` ordering) of
        (Just indexA, Just indexB) -> indexA < indexB
        _ -> False

refine :: [[String]] -> Term Char Char -> [Term Char Char] -> [[String]]
refine rules t1 [] = []
refine rules t1 t2 = findOrderings(compareOutermostSymbols t1 (head t2)) (rules)

sat :: [[a]] -> Bool
sat [] = False
sat _ = True

rest :: [Rule Char Char] -> Term Char Char -> [Term Char Char]
rest rules term = p ([term], (getAllFamilies rules)) rules 

p :: ([Term Char Char], [[String]]) -> [Rule Char Char] -> [Term Char Char]
p (terms, families) rules = [head terms] ++ (help2 (head terms) families rules rules)

getNewReduct :: Term Char Char -> Rule Char Char -> [Term Char Char]
getNewReduct term rule =
  case fullRewrite [rule] term of
    [] -> [] -- Rule doesn't apply, return Nothing
    (reduct:_) -> [result reduct]

help2 :: Term Char Char -> [[String]] -> [Rule Char Char] ->  [Rule Char Char] -> [Term Char Char]
help2 term families rules rSet
 | rSet == [] = []
 | null newReduct = help2 term families rules (tail rSet)
 | not (sat newFamilies) = help2 term families rules (tail rSet)
 | otherwise = newReduct ++ (help2 term newFamilies rules (tail rSet)) ++ (help2 (head newReduct) newFamilies rules rules)
 where newReduct = getNewReduct term (head rSet)
       newFamilies = refine families term newReduct


-- Define a rule
rule1 :: Rule Char Char
rule1 = Rule
  { lhs = Fun 'f' [Var 'x', Var 'y']
  , rhs = Fun 'g' []
  }

rule2 :: Rule Char Char
rule2 = Rule
  { lhs = Fun 'f' [Var 'a', Var 'y']
  , rhs = Fun 'h' []
  }

rule3 :: Rule Char Char
rule3 = Rule
  { lhs = Fun 'h' []
  , rhs = Fun 'z' []
  }

rule4 :: Rule Char Char
rule4 = Rule
  { lhs = Fun 'z' []
  , rhs = Fun 'f' []
  }

rule5 :: Rule Char Char
rule5 = Rule
  { lhs = Fun 'z' []
  , rhs = Fun 'e' []
  }

-- List of rules
rSet :: [Rule Char Char]
rSet = [rule1, rule2, rule3, rule4, rule5]

-- Corrected sample term
sampleTerm :: Term Char Char
sampleTerm = Fun 'f' [Var 'x', Var 'y']

sampleTerm2 :: Term Char Char
sampleTerm2 = Fun 'g' [Var 'x', Var 'y']

sampleTerm3 :: Term Char Char
sampleTerm3 = Fun 'h' [Var 'x', Var 'y']

-- Apply full rewrite
resultTerms :: [Reduct Char Char Char]
resultTerms = fullRewrite rSet sampleTerm


