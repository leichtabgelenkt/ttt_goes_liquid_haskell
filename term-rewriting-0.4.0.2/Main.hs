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
-- outermostSymbol (Var v) = [v]
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
definedSymbols :: [Rule Char Char] -> [(Char, Char)]
definedSymbols rules = temp 1 (Prelude.map head (nub (Prelude.map outermostSymbolRule rules)))
 where temp x [] = []
       temp x y = (head y, head (show x)) : temp (x + 1) (tail y) 


-- This function takes the same list of rules twice as input and returns the list of dependency pairs
dependencyPairs :: [Rule Char Char] ->[Rule Char Char] -> [Rule Char Char]
dependencyPairs [] _ = []
dependencyPairs ((Rule lhs rhs) : xs) rules = case drhs of
  [Nothing] -> dependencyPairs xs rules
  x ->  composeRules dlhs x ++ dependencyPairs xs rules
  where dSymbols = definedSymbols rules
        dlhs = changeSymbol lhs dSymbols
        drhs = changeSymbolRHS rhs dSymbols

-- Takes a term as a lhs of a rule and a list of Maybe terms as input and constructs a list of rules out of the lhs and all rhs terms which are not Nothing in the list
composeRules :: Term Char Char -> [Maybe (Term Char Char)] -> [Rule Char Char]
composeRules x [] = []
composeRules x (y:ys) = case y of
  Just z -> (Rule x z) : composeRules x ys
  Nothing -> composeRules x ys

-- Takes a term and the list of defined functions and returns a term where the function symbol of the term is changed with the corresponding dependency symbol.
-- This function is only used for lhs of a dependecy pair, so the function symbol of the term will always be inside the list of defined symbols
changeSymbol :: Term Char Char -> [(Char, Char)] -> Term Char Char
changeSymbol (Fun a b) ys = case lookup a ys of
  Just x -> Fun x b
  _ -> Fun a b

-- This function takes a term (which represents a rhs of a dependency pair) and a list of defined symbols. It checks if the outermost function symbol matches one of the defined symbols.
-- If it does, it gets changed with the corresponding dependency symbol, if it doesn't we do nothing. In both cases we continue with the sub functions.
changeSymbolRHS ::  Term Char Char -> [(Char, Char)] -> [Maybe (Term Char Char)]
changeSymbolRHS (Var _) ys = [Nothing]
changeSymbolRHS (Fun c d) ys = case lookup c ys of
  Just x -> Just (Fun x d) : nub (concat [changeSymbolRHS t ys | t <- d])
  Nothing -> nub (concat [changeSymbolRHS t ys | t <- d])

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

rule6 :: Rule Char Char
rule6 = Rule
  { lhs = Fun 'f' [Var 'a', Var 'y']
  , rhs = Fun 'h' [Fun 'z' []]
  }

-- Example 1 of Rest

-- n is intersection
-- u is union
-- s means s0
-- t means s1

distribUnion :: Rule Char Char
distribUnion = Rule 
  { 
    lhs = Fun 'n' [ Fun 'u' [Fun 's' [], Fun 't' []] , Fun 's' [] ], 
    rhs = Fun 'u' [ Fun 'n' [Fun 's' [], Fun 's' []] , Fun 'n' [Fun 't' [], Fun 's' []]]
  }

idemInter :: Rule Char Char
idemInter = Rule
  {
    lhs = Fun 'n' [ Fun 's' [], Fun 's' []],
    rhs = Fun 's' []
  }

-- only applicable when s and t are disjoint
disjointnessAss :: Rule Char Char
disjointnessAss = Rule
  {
    lhs = Fun 'n' [ Fun 't' [], Fun 's' []],
    rhs = Fun 'e' []
  }

emptyUnion :: Rule Char Char
emptyUnion = Rule
  {
    lhs = Fun 'u' [Fun 's' [], Fun 'e' []],
    rhs = Fun 's' []
  }

example1RuleSet :: [Rule Char Char]
example1RuleSet = [distribUnion, idemInter, disjointnessAss, emptyUnion]

example1StartTerm :: Term Char Char
example1StartTerm = Fun 'n' [Fun 'u' [Fun 's' [], Fun 't' []], Fun 's' []]

example1ResultTerms :: [Term Char Char]
example1ResultTerms = rest example1RuleSet example1StartTerm
-- =  [Fun 'n' [Fun 'u' [Var 's',Var 't'],Var 's'],Fun 'u' [Fun 'n' [Var 's',Var 's'],Fun 'n' [Var 't',Var 's']]]


-- Example 2 of Rest

distribInter :: Rule Char Char
distribInter = Rule
  { 
    lhs = Fun 'u' [ Fun 'n' [Var 's', Var 't'] , Var 's' ], 
    rhs = Fun 'n' [ Fun 'u' [Var 's', Var 's'] , Fun 'u' [Var 't', Var 's']]
  }

idemUnion :: Rule Char Char
idemUnion = Rule
  {
    lhs = Fun 'u' [ Var 's', Var 's'],
    rhs = Var 's'
  }

commutUnion :: Rule Char Char
commutUnion = Rule
  {
    lhs = Fun 'u' [Var 's', Var 't'],
    rhs = Fun 'u' [Var 't', Var 's']
  }

-- only applicable when t is a subset of s
subsetAss :: Rule Char Char
subsetAss = Rule
  {
    lhs = Fun 'u' [Var 's', Var 't'],
    rhs = Var 's'
  }

-- indemInter allready in Example 1

example2RuleSet :: [Rule Char Char]
example2RuleSet = [distribInter, idemUnion, commutUnion, subsetAss, idemInter]

example2StartTerm :: Term Char Char
example2StartTerm = Fun 'u' [Fun 'n' [Var 's', Var 't'], Var 's']

example2ResultTerms :: [Term Char Char]
example2ResultTerms = rest example2RuleSet example2StartTerm

-- no solution so far because of the commutativity

-- Example 3 of Rest
-- currently not possible because of associativity and commutativity


-- other examples -- 

-- List of rules
rSet :: [Rule Char Char]
rSet = [rule1, rule2, rule3, rule4, rule5, rule6]

-- Corrected sample term
sampleTerm :: Term Char Char
sampleTerm = Fun 'f' [Var 'x', Var 'y']

sampleTerm2 :: Term Char Char
sampleTerm2 = Fun 'g' [Var 'x', Var 'y']

sampleTerm3 :: Term Char Char
sampleTerm3 = Fun 'h' [Var 'x', Var 'y']

sampleTerm4 :: Term Char Char
sampleTerm4 = Fun 'f' [Fun 'g' [Var 'x', Fun 'h' [Fun 'f' [Var 'x', Var 'y'], Var 'b']], Fun 'h' [Var 'x', Var 'y']]

-- Apply full rewrite
resultTerms :: [Reduct Char Char Char]
resultTerms = fullRewrite rSet sampleTerm4


