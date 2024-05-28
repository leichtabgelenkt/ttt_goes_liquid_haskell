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
import Data.Function (on, const)
import Data.Graph
import Data.Graph.SCC
import Data.SBV
import Data.SBV.List
import Data.SBV.Internals
import GHC.Exts (fromList)
import GHC.Cmm (CmmNode(res))
import Data.SBV (constrain, sInteger, allSat)
import GHC.Prelude (Show(show))
import Data.Bool (Bool(True, False))
import Data.Char

aaa = sFalse
bbb = sNot aaa

instance (Show f, Show v, Show v') => Show (Reduct f v v') where
  show (Reduct result pos rule subst) =
    "Reduct { result = " Data.List.++ show result
    Data.List.++ ", pos = " Data.List.++ show pos
    Data.List.++ ", rule = " Data.List.++ show rule
    Data.List.++ ", subst = " Data.List.++ show subst
    Data.List.++ " }"


newtype RewriteSequence = RewriteSequence [(Term Char Char, Term Char Char)]

instance Show RewriteSequence where
  show s@(RewriteSequence a) = show $ Prelude.map listToStringWithArrows $ nub $ removeSublists $ removeEmptySublists $ Prelude.map nub $ showRewriteSequence s (fst (Data.List.head a))

listToStringWithArrows :: [Term Char Char] -> String
listToStringWithArrows [] = ""
listToStringWithArrows [x] = show x
listToStringWithArrows (x:xs) = show x Data.List.++ " -> " Data.List.++ listToStringWithArrows xs


showRewriteSequence :: RewriteSequence -> Term Char Char -> [[Term Char Char]]
showRewriteSequence (RewriteSequence []) x = [[]]
showRewriteSequence (RewriteSequence ((start, end):xs)) x = if start == x then [start : end : t | t <- splitAndRemove $ getL end xs] Data.List.++ showRewriteSequence (RewriteSequence xs) x else showRewriteSequence (RewriteSequence xs) x -- Data.List.++ showRewriteSequence (RewriteSequence xs)
--[([start, end] Data.List.++ getL end xs)]

getL :: Term Char Char -> [(Term Char Char, Term Char Char)] -> [Term Char Char]
getL _ [] = [Fun '$' []]
getL endTerm ((startTerm, nextEndTerm) : remainingPairs)
  | endTerm == startTerm = [startTerm, nextEndTerm] Data.List.++ getL nextEndTerm remainingPairs Data.List.++ getL endTerm remainingPairs
  | otherwise = getL endTerm remainingPairs

splitAndRemove :: [Term Char Char] -> [[Term Char Char]]
splitAndRemove [] = []
splitAndRemove xs = Data.List.filter (not . Data.List.null) $ case break (== Fun '$' []) xs of
  (before, []) -> [before]
  (before, _ : after) -> before : splitAndRemove after

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist sublist list = sublist `Data.List.elem` (Data.List.tail $ Data.List.init $ subsequences list)

removeEmptySublists :: [[Term a b]] -> [[Term a b]]
removeEmptySublists = Data.List.filter (not . Data.List.null)

removeSublists :: Eq a => [[a]] -> [[a]]
removeSublists lists = Data.List.filter (\x -> Data.List.all (not . (`isSublist` x)) (delete x lists)) lists

mysequence = RewriteSequence [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]

startElement = fst $ Data.List.head [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]

test2 = getL (Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []]) [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]
test = showRewriteSequence mysequence startElement
test3 = splitAndRemove test2
test4 = nub $ removeSublists $ removeEmptySublists $ Prelude.map nub test


-- Splits a String
splitString :: String -> [String]
splitString []     = []
splitString (x:xs) = [x] : splitString xs

-- Gets all function symbols in a set of rules
collectFunctionsInRuleList :: [Rule Char Char] -> [String]
collectFunctionsInRuleList rules = nub (splitString (Data.List.concatMap (\rule -> TermOps.funs (lhs rule) Data.List.++ TermOps.funs (rhs rule)) rules))

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
compareOutermostSymbols term1 term2 = splitString (outermostSymbol term1 Data.List.++ outermostSymbol term2)

-- Removes all orderings of a family where the ordering is not the same as in the first argument, which is a sub family
findOrderings :: [String] -> [[String]] -> [[String]]
findOrderings _ [] = []
findOrderings [x, y] allOrderings =
  Data.List.filter (\ordering -> isBefore ordering x y) allOrderings
  where
    isBefore :: Eq a => [a] -> a -> a -> Bool
    isBefore ordering a b =
      case (a `elemIndex` ordering, b `elemIndex` ordering) of
        (Just indexA, Just indexB) -> indexA < indexB
        _ -> False

-- Makes a sub function order out of two terms and then refines the family of orderings 
refine :: [[String]] -> Term Char Char -> [Term Char Char] -> [[String]]
refine rules t1 [] = []
refine rules t1 t2 = findOrderings (compareOutermostSymbols t1 (Data.List.head t2)) rules

-- Just checks if a list is empty or not
mySat :: [[a]] -> Bool
mySat [] = False
mySat _ = True

-- Start of the REST algorithm
rest :: [Rule Char Char] -> Term Char Char -> RewriteSequence
rest rules term = p ([term], (getAllFamilies rules)) rules

-- Adds the Data.List.head element to the result and then hands the work down
p :: ([Term Char Char], [[String]]) -> [Rule Char Char] -> RewriteSequence
p (terms, families) rules = RewriteSequence (help2 (Data.List.head terms) families rules rules)

-- Checks if a rules is applicable on a certain term. If so, then it returns the reduct, otherwise it returns an empty list
getNewReduct :: Term Char Char -> Rule Char Char -> [Term Char Char]
getNewReduct term rule =
  case fullRewrite [rule] term of
    [] -> [] -- Rule doesn't apply, return Nothing
    (reduct:_) -> [result reduct]

-- Here is where most of the work for the REST algorithm happens. This function takes a term, a family of function orderings, the whole list of rewrite rules and the list of rewrite rules which have not been tested yet.
-- If all rules have been tested then it returns an empty list. If the next rules isn't applicable on the term, then a recursive call happens with the Data.List.tail of the rule list. If the rule is applicable, but the arising subordering
-- of functions isn't mySatisfiable for the given family, then the result will be ignored and process will be continued with the next rule. However, if everything works out, then the reduct gets into the result list and we get two new
-- function calls: The first one on the old term with the Data.List.tail of the rules and the second one on the new term with all rules
help2 :: Term Char Char -> [[String]] -> [Rule Char Char] ->  [Rule Char Char] -> [(Term Char Char, Term Char Char)]
help2 term families rules rSet
 | rSet == [] = []
 | Data.List.null newReduct = help2 term families rules (Data.List.tail rSet)
 | not (mySat newFamilies) = help2 term families rules (Data.List.tail rSet)
 | otherwise = [(term, Data.List.head newReduct)] Data.List.++ (help2 term newFamilies rules (Data.List.tail rSet)) Data.List.++ (help2 (Data.List.head newReduct) newFamilies rules rules)
 where lhs = getLHS (Data.List.head rSet)
       rhs = getRHS (Data.List.head rSet)
       newReduct = getNewReduct term (Data.List.head rSet)
       newFamilies = refine families lhs [rhs]

getLHS :: Rule Char Char -> Term Char Char
getLHS (Rule lhs _) = lhs

getRHS :: Rule Char Char -> Term Char Char
getRHS (Rule _ rhs) = rhs

-- Finds the outermost function symbol in the left-hand side of a rule
outermostSymbolRule :: Rule Char Char -> [Char]
outermostSymbolRule (Rule lhs _) = outermostSymbol lhs

outermostSymbolRuleRight :: Rule Char Char -> [Char]
outermostSymbolRuleRight (Rule _ rhs) = outermostSymbol rhs

-- Returns list of all defined symbols in a set of rules
definedSymbols :: [Rule Char Char] -> [(Char, Char)]
definedSymbols rules = temp 1 (Prelude.map Data.List.head (nub (Prelude.map outermostSymbolRule rules)))
 where temp x [] = []
       temp x y = (Data.List.head y, Data.List.head (show x)) : temp (x + 1) (Data.List.tail y)


-- This function takes the same list of rules twice as input and returns the list of dependency pairs
dependencyPairs :: [Rule Char Char] -> [Rule Char Char] -> [Rule Char Char]
dependencyPairs [] _ = []
dependencyPairs ((Rule lhs rhs) : xs) rules = case drhs of
  [Nothing] -> dependencyPairs xs rules
  x ->  composeRules dlhs x Data.List.++ dependencyPairs xs rules
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
  Just x -> Just (Fun x d) : nub (Data.List.concat [changeSymbolRHS t ys | t <- d])
  Nothing -> nub (Data.List.concat [changeSymbolRHS t ys | t <- d])

-- Given rules, this function returns a list of triple like (Numeration of node, outermost symbol of left side, outermost symbol of right side)
sccPrepare :: [Rule Char Char] -> Int -> [(Int, String, String)]
sccPrepare [] _ = []
sccPrepare (x:xs) y = (y, outermostSymbolRule x, outermostSymbolRuleRight x) : sccPrepare xs (y+1)

-- Compute vertices of dependency graph
getVertices :: [(Int, String, String)] -> [(Int, String, String)] -> [(Int, Int)]
getVertices [] _ = []
getVertices (x:xs) y = help x y Data.List.++ getVertices xs y
 where help _ [] = []
       help s@(i, _, r) ((i2, l, _):xs) = if r == l then (i, i2) : help s xs else help s xs

-- Returns the min-node and max-node of the vertices 
getMinMax :: [(Int, String, String)] -> (Int, Int) -> (Int, Int)
getMinMax [] (x,y) = (x,y)
getMinMax ((i, _, _):xs) (x,y)
 | x == 0 && y == 0 = getMinMax xs (i,i)
 | i < x = getMinMax xs (i,y)
 | i > y = getMinMax xs (x,i)
 | otherwise = getMinMax xs (x,y)

-- Returns the SCCs from a set of Dependency Rules 
getSccFromDependencyPairs :: [Rule Char Char] -> [SCC Vertex]
getSccFromDependencyPairs x = sccList graph
 where prepare = sccPrepare x 1
       vertices = getVertices prepare prepare
       graph = buildG (getMinMax prepare (0,0)) vertices

findRule :: [Rule Char Char] -> (Int, String, String) -> Rule Char Char
findRule s p@(_, a, b) = if (outermostSymbolRule (Data.List.head s) == a) && (outermostSymbolRuleRight (Data.List.head s) == b) then (Data.List.head s) else findRule (Data.List.tail s) p

findSccNode :: [Rule Char Char] -> [(Int, String, String)] -> [Int] -> [Rule Char Char]
findSccNode _ _ [] = []
findSccNode rules prepare (y:ys) = findRule rules (getIndex prepare y) : findSccNode (delete (findRule rules (getIndex prepare y)) rules) prepare ys
 where getIndex ((i,a,b):zs) y = if i == y then (i,a,b) else getIndex zs y

-- mulitset just as a list
type Multiset a = [a]

type Projection = [(Char, [Int])]

-- testterme
term1 = Fun 'f' [Var 'x', Var 'y']
term12 = Fun 'f' [Var 'x', Var 'z']
term2 = Fun 'g' [Fun 'h' [Var 'x'], Fun 'f' [Var 'x', Var 'y']]
term3 = Fun 'h' [Fun 'f' [Var 'z', Var 'x']]

projection :: Projection
projection = [('f',[1]),('g',[]),('h',[1])]

subgroup :: Term Char Char -> Term Char Char -> Bool
subgroup x y
  | x == y = True
  | isVar y = False
  | otherwise = or [subgroup x v | v <- handBackArgumentsFromTerm y]

isNotProjecting :: Term Char Char -> Projection -> Bool
isNotProjecting (Fun c _) p = Data.List.any (\(symbol, list) -> symbol == c && Data.List.null list) p
isNotProjecting (Var c) p = Data.List.any (\(symbol, list) -> symbol == c && Data.List.null list) p

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

-- Implement the example from the term rewriting lecture, slides 13x1, slide 37-39
r1 :: Rule Char Char
r1 = Rule
  { lhs = Fun 'l' [Var 'n', Fun 'o' []]
  , rhs = Fun 'o' []
  }

r2 :: Rule Char Char
r2 = Rule
  { lhs = Fun 'l' [Var 'n', Fun ':' [Var 'm', Var 'x']]
  , rhs = Fun 'i' [Fun '<' [Var 'm', Var 'n'], Var 'n', Fun ':' [Var 'm', Var 'x']]
  }

r3 :: Rule Char Char
r3 = Rule
  { lhs = Fun 'h' [Var 'n', Fun 'o' []]
  , rhs = Fun 'o' []
  }

r4 :: Rule Char Char
r4 = Rule
  { lhs = Fun 'h' [Var 'n', Fun ':' [Var 'm', Var 'x']]
  , rhs = Fun 'j' [Fun '<' [Var 'm', Var 'n'], Var 'n', Fun ':' [Var 'm', Var 'x']]
  }

r5 :: Rule Char Char
r5 = Rule
  { lhs = Fun '+' [Fun 'o' [], Var 'y']
  , rhs = Var 'y'
  }

r6 :: Rule Char Char
r6 = Rule
  { lhs = Fun '+' [Fun ':' [Var 'n', Var 'x'], Var 'y']
  , rhs = Fun ':' [Var 'n', Fun '+' [Var 'x', Var 'y']]
  }

r7 :: Rule Char Char
r7 = Rule
  { lhs = Fun 'q' [Fun 'o' []]
  , rhs = Fun 'o' []
  }

r8 :: Rule Char Char
r8 = Rule
  { lhs = Fun 'q' [Fun ':' [Var 'n', Var 'x']]
  , rhs = Fun '+' [Fun 'q' [Fun 'l' [Var 'n', Var 'x']], Fun ':' [Var 'n', Fun 'q' [Fun 'h' [Var 'n', Var 'x']]]]
  }

r9 :: Rule Char Char
r9 = Rule
  { lhs = Fun 'i' [Fun 'F' [], Var 'n', Fun ':' [Var 'm', Var 'x']]
  , rhs = Fun 'l' [Var 'n', Var 'x']
  }

r10 :: Rule Char Char
r10 = Rule
  { lhs = Fun 'i' [Fun 'T' [], Var 'n', Fun ':' [Var 'm', Var 'x']]
  , rhs = Fun ':' [Var 'm', Fun 'l' [Var 'n', Var 'x']]
  }

r11 :: Rule Char Char
r11 = Rule
  { lhs = Fun 'j' [Fun 'F' [], Var 'n', Fun ':' [Var 'm', Var 'x']]
  , rhs = Fun ':' [Var 'm', Fun 'h' [Var 'n', Var 'x']]
  }

r12 :: Rule Char Char
r12 = Rule
  { lhs = Fun 'j' [Fun 'T' [], Var 'n', Fun ':' [Var 'm', Var 'x']]
  , rhs = Fun 'h' [Var 'n', Var 'x']
  }

r13 :: Rule Char Char
r13 = Rule
  { lhs = Fun '<' [Fun '0' [], Var 'y']
  , rhs = Fun 'T' []
  }

r14 :: Rule Char Char
r14 = Rule
  { lhs = Fun '<' [Fun 's' [Var 'x'], Fun '0' []]
  , rhs = Fun 'F' []
  }

r15 :: Rule Char Char
r15 = Rule
  { lhs = Fun '<' [Fun 's' [Var 'x'], Fun 's' [Var 'y']]
  , rhs = Fun '<' [Var 'x', Var 'y']
  }


examplerules = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]
drules = dependencyPairs examplerules examplerules
resultExample = getSccFromDependencyPairs drules
see = sccPrepare drules 1
index = [drules Data.List.!! 7, drules Data.List.!! 9]
sccTest = getSccFromDependencyPairs (dependencyPairs rulesTest rulesTest)
findScc = findSccNode drules (sccPrepare drules 1) [7,9]

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


rule7 :: Rule Char Char
rule7 = Rule
  { lhs = Fun '+' [Fun '0' [], Var 'y']
  , rhs = Var 'y'
  }

rule8 :: Rule Char Char
rule8 = Rule
  { lhs = Fun '*' [Fun '0' [], Var 'y']
  , rhs = Fun '0' []
  }

rule9 :: Rule Char Char
rule9 = Rule
  { lhs = Fun '+' [Fun 's' [Var 'x'], Var 'y']
  , rhs = Fun 's' [Fun '+' [Var 'x', Var 'y']]
  }

rule10 :: Rule Char Char
rule10 = Rule
  { lhs = Fun '*' [Fun 's' [Var 'x'], Var 'y']
  , rhs = Fun '+' [Fun '*' [Var 'x', Var 'y'], Var 'y']
  }


rulesTest = [rule7, rule8, rule9, rule10]
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

example1ResultTerms :: RewriteSequence
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

example2ResultTerms :: RewriteSequence
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



projectionPhilipp :: Term Char Char -> SInteger -> SInteger
projectionPhilipp (Fun _ xs) x = test `elemAt` x
  where help (Var b : zs) = read [b] : help zs
        help [] = []
        test = literal (help xs)


projectionPhilipp2 :: Term Char Char -> SInteger -> SInteger
projectionPhilipp2 (Fun _ xs) x = test `elemAt` x
  where help (Var b : zs) = charValue b : help zs
        help [] = []
        test = literal (help xs)

charValue :: Char -> Integer
charValue c = toInteger (ord c)

getNumChildren :: Term Char Char -> Integer
getNumChildren (Fun _ xs) = toInteger $ Data.List.length xs

charToNumber :: SInteger -> SInteger
charToNumber x = ite (x .== literal (toInteger (ord 'b'))) (literal 1) (literal 0)

s = allSat $ do
  let sss = Fun 'f' [Var 'b', Var 'a', Var 'b']
  let t = Fun 'f' [Var 'c', Var 'a', Var 'a']
  a <- sInteger "a"
  constrain $ charToNumber (projectionPhilipp2 sss a) .> charToNumber (projectionPhilipp2 t a)
  constrain $ a .>= 0
  constrain $ a .< literal 3

main :: IO ()
main = do
  result <- s
  print result