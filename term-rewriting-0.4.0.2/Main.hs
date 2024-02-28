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

-- Splits a String
splitString :: String -> [String]
splitString []     = []
splitString (x:xs) = [x] : splitString xs

-- Gets all function symbols in a set of rules
collectFunctionsInRuleList :: [Rule Char Char] -> [String]
collectFunctionsInRuleList rules = nub (splitString (concatMap (\rule -> TermOps.funs (lhs rule) ++ TermOps.funs (rhs rule)) rules))

-- Gets all possible hirachies of function symbols in a set of rules
getAllFamilies :: [Rule Char Char] -> [[String]]
getAllFamilies rules = permutations (collectFunctionsInRuleList rules)

-- Finds the outermost function symbol in a term 
outermostSymbol :: Term Char Char -> [Char]
outermostSymbol (Var _) = error "Variable has no outermost symbol"
outermostSymbol (Fun f _) = [f]

-- Returns a list of the combined outermost symbols of two terms
compareOutermostSymbols :: Term Char Char -> Term Char Char -> [String]
compareOutermostSymbols term1 term2 = splitString (outermostSymbol term1 ++ outermostSymbol term2)

-- Removes all orderings of a family where the ordering is not the same as in the first argument, which is a sub family
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

-- Makes a sub function order out of two terms and then refines the family of orderings 
refine :: [[String]] -> Term Char Char -> [Term Char Char] -> [[String]]
refine rules t1 [] = []
refine rules t1 t2 = findOrderings (compareOutermostSymbols t1 (head t2)) rules

-- Just checks if a list is empty or not
sat :: [[a]] -> Bool
sat [] = False
sat _ = True

-- Start of the REST algorithm
rest :: [Rule Char Char] -> Term Char Char -> [Term Char Char]
rest rules term = p ([term], (getAllFamilies rules)) rules 

-- Adds the head element to the result and then hands the work down
p :: ([Term Char Char], [[String]]) -> [Rule Char Char] -> [Term Char Char]
p (terms, families) rules = [head terms] ++ (help2 (head terms) families rules rules)

-- Checks if a rules is applicable on a certain term. If so, then it returns the reduct, otherwise it returns an empty list
getNewReduct :: Term Char Char -> Rule Char Char -> [Term Char Char]
getNewReduct term rule =
  case fullRewrite [rule] term of
    [] -> [] -- Rule doesn't apply, return Nothing
    (reduct:_) -> [result reduct]

-- Here is where most of the work for the REST algorithm happens. This function takes a term, a family of function orderings, the whole list of rewrite rules and the list of rewrite rules which have not been tested yet.
-- If all rules have been tested then it returns an empty list. If the next rules isn't applicable on the term, then a recursive call happens with the tail of the rule list. If the rule is applicable, but the arising subordering
-- of functions isn't satisfiable for the given family, then the result will be ignored and process will be continued with the next rule. However, if everything works out, then the reduct gets into the result list and we get two new
-- function calls: The first one on the old term with the tail of the rules and the second one on the new term with all rules
help2 :: Term Char Char -> [[String]] -> [Rule Char Char] ->  [Rule Char Char] -> [Term Char Char]
help2 term families rules rSet
 | rSet == [] = []
 | null newReduct = help2 term families rules (tail rSet)
 | not (sat newFamilies) = help2 term families rules (tail rSet)
 | otherwise = newReduct ++ (help2 term newFamilies rules (tail rSet)) ++ (help2 (head newReduct) newFamilies rules rules)
 where lhs = getLHS (head rSet)
       rhs = getRHS (head rSet)
       newReduct = getNewReduct term (head rSet)
       newFamilies = refine families lhs [rhs]

getLHS :: Rule Char Char -> Term Char Char
getLHS (Rule lhs _) = lhs

getRHS :: Rule Char Char -> Term Char Char
getRHS (Rule _ rhs) = rhs

-- Finds the outermost function symbol in the left-hand side of a rule
outermostSymbolRule :: Rule Char Char -> [Char]
outermostSymbolRule (Rule lhs _) = outermostSymbol lhs

-- Returns list of all defined symbols in a set of rules
definedSymbols :: [Rule Char Char] -> [[Char]]
definedSymbols rules = nub (Prelude.map outermostSymbolRule rules)

dependencyPairs :: [Rule Char Char] -> [Rule Char Char]
dependencyPairs [] = []
dependencyPairs ((Rule lhs rhs) : xs) = undefined

changeSymbol :: Rule Char Char -> [Char] -> Rule Char Char
changeSymbol (Rule (Fun a b) rhs@(Fun c d)) ys = if a `elem` ys then (Rule (Fun '1' b) rhs) else (Rule (Fun a b) rhs)

changeSymbolRHS ::  Term Char Char -> [Char] -> Maybe (Term Char Char)
changeSymbolRHS (Var _) ys = Nothing
changeSymbolRHS (Fun c d) ys = if c `elem` ys then Just (Fun '2' d) else changeSymbolRHS (head d) ys
-- Define a rule
rule1 :: Rule Char Char
rule1 = Rule
  { lhs = Fun 'f' [Var 'x', Var 'y']
  , rhs = Fun 'g' [Var 'x', Var 'y']
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
  , rhs = Fun 'f' [Var 'x', Var 'y']
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

sampleTerm4 :: Term Char Char
sampleTerm4 = Fun 'f' [Fun 'g' [Var 'x', Var 'y'], Var 'y']

-- Apply full rewrite
resultTerms :: [Reduct Char Char Char]
resultTerms = fullRewrite rSet sampleTerm4


