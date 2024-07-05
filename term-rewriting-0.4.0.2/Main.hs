{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
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
import Rules
import Rest
import Multiplicity
import DependencyPairs
import MySCCGraph
import Control.Monad.Trans.RWS.Lazy (get)
import Control.Monad.IO.Class (MonadIO(liftIO))

instance (Show f, Show v, Show v') => Show (Reduct f v v') where
  show (Reduct result pos rule subst) =
    "Reduct { result = " Data.List.++ show result
    Data.List.++ ", pos = " Data.List.++ show pos
    Data.List.++ ", rule = " Data.List.++ show rule
    Data.List.++ ", subst = " Data.List.++ show subst
    Data.List.++ " }"



instance Show RewriteSequence where
  show s@(RewriteSequence a) = show $ Prelude.map listToStringWithArrows $ nub $ removeSublists $ removeEmptySublists $ Prelude.map nub $ showRewriteSequence s (fst (Data.List.head a))

listToStringWithArrows :: [Term Char Char] -> String
listToStringWithArrows [] = ""
listToStringWithArrows [x] = show x
listToStringWithArrows (x:xs) = show x Data.List.++ " -> " Data.List.++ listToStringWithArrows xs

newtype PrintRule = PrintRule (Rule Char Char)

instance Show PrintRule where
  show (PrintRule (Rule lhs rhs)) = show lhs Data.List.++ " -> " Data.List.++ show rhs Data.List.++ "\n"

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
isSublist sublist list = sublist `Data.List.elem` Data.List.tail (Data.List.init $ subsequences list)

removeEmptySublists :: [[Term a b]] -> [[Term a b]]
removeEmptySublists = Data.List.filter (not . Data.List.null)

removeSublists :: Eq a => [[a]] -> [[a]]
removeSublists lists = Data.List.filter (\x -> not (Data.List.any (`isSublist` x) (delete x lists))) lists

mysequence = RewriteSequence [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]

startElement = fst $ Data.List.head [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]

test2 = getL (Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []]) [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]
test = showRewriteSequence mysequence startElement
test3 = splitAndRemove test2
test4 = nub $ removeSublists $ removeEmptySublists $ Prelude.map nub test

-- testterme
term1 = Fun 'f' [Var 'x', Var 'y']
term12 = Fun 'f' [Var 'x', Var 'z']
term2 = Fun 'g' [Fun 'h' [Var 'x'], Fun 'f' [Var 'x', Var 'y']]
term3 = Fun 'h' [Fun 'f' [Var 'z', Var 'x']]

projection :: Projection
projection = [('f',1),('g',-1),('h',1)]


examplerules :: [Rule Char Char]
examplerules = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]
drules :: [Rule Char Char]
drules = dependencyPairs examplerules examplerules
resultExample :: [SCC Vertex]
resultExample = getSccFromDependencyPairs drules
see :: [(Int, String, String)]
see = sccPrepare drules 1
index = [drules Data.List.!! 7, drules Data.List.!! 9]
sccTest = getSccFromDependencyPairs (dependencyPairs rulesTest rulesTest)
findScc :: [Rule Char Char]
findScc = findSccNode drules (sccPrepare drules 1) [7,9]


printExampleRules = Data.List.map PrintRule examplerules
printDrules = Data.List.map PrintRule drules
printScc = Data.List.map PrintRule findScc


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

sampleTerm5 :: Term Char Char
sampleTerm5 = Fun 'h' [Fun 'f' [Var 'x', Var 'y'], Fun 'g' [Var 'x', Var 'y']]

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

proj :: Projection
proj = [('f',1),('g',-1),('h',2)]

sub :: Term Char Char -> Term Char Char -> [Term Char Char]
sub s t = Data.List.nub $ help s Data.List.++ help t
 where help f@(Fun _ xs) = f : concatMap help xs
       help v@(Var _) = [v]

upper :: Term Char Char -> Term Char Char -> Term Char Char -> Projection -> SBool
upper u s t p = sAnd [ite (literal (u `properSubgroup` v)) ((sMultiplicity 1 s v p) .== (sMultiplicity 1 t v p)) (sTrue) | v <- sub s t]

geq :: Term Char Char -> Term Char Char -> Projection -> SBool
geq s t p = sAnd [ite (upper u s t p) ((sMultiplicity 1 s u p) .>= (sMultiplicity 1 t u p)) (sTrue)| u <- sub s t]

neq :: Term Char Char -> Term Char Char -> Projection -> SBool
neq s t p = sNot $ sAnd [(sMultiplicity 1 s u p) .== (sMultiplicity 1 t u p) | u <- sub s t]

geqRules :: [Rule Char Char] -> Projection -> SBool
geqRules rules p = sAnd [geq (getLHS rule) (getRHS rule) p | rule <- rules]

neqRules :: [Rule Char Char] -> Projection -> SBool
neqRules rules p = sOr [neq (getLHS rule) (getRHS rule) p | rule <- rules]

rt :: Term Char Char -> Projection -> SBool
rt t p = sNot $ sIsNotProjecting t p

rtRules :: [Rule Char Char] -> Projection -> SBool
rtRules rules p = sAnd [ite (rt lhs p) (geq lhs rhs p) (sTrue) | (Rule rhs lhs) <- rules]

isUnsatisfiable :: SMTResult -> Bool
isUnsatisfiable (Unsatisfiable _ _) = True
isUnsatisfiable _     = False

ttt3 :: [Rule Char Char] -> Term Char Char -> IO String
ttt3 rules term = do
  result <- ttt3Help rules term
  if and result
    then return "The term terminates with the given rules"
    else return "The term does not terminate with the given rules"

{-
Stand 28.6.: ttt3Help und ttt3 müssen wsl zu einer IO Funktion umgebaut werden, alle anderen Hilfsfunktionen davon werden kompiliert wurden aber noch nicht 100% getestet
Nächster Schritt: Lösung überlegen um alles auf IO umzustellen, denn ansonst kann das Ergebnis in value (SMT output) nicht vergliechen werden.
Auch wichtig wäre zu schauen ob "reachableRulesFromNodes" wirklich die richtigen Regeln enthält die man braucht
!!! reachableRulesFromNodes enthält momentan alle Regeln die man erreichen kann, allerdings brauchen wir nur die, welche sich in einem SCC befinden. Müsste noch
implementiert werden
-}


ttt3Help :: [Rule Char Char] -> Term Char Char -> IO [Bool]
ttt3Help rules@(x:xs) term
 | (fullRewrite rules term) == [] = return [True]
 | otherwise = do
    let dependencyRules = dependencyPairs rules rules
        reachableNodes = reachableNodesFromTerm rules term
        reachableRulesFromNodes = findSccNode dependencyRules (sccPrepare dependencyRules 1) reachableNodes
        projection = buildProjection rules
    -- value :: SMTResult
    value <- sat $ do
      a <- sInteger "a"
      b <- sInteger "b"
      c <- sInteger "c"
      d <- sInteger "d"
      e <- sInteger "e"
      let
        constrainList :: [SInteger]
        constrainList = [a,b,c,d,e]
      let
        newProjection :: Projection
        newProjection = putValuesIntoProjection constrainList projection
      constrain $ geqRules reachableRulesFromNodes newProjection
      constrain $ neqRules reachableRulesFromNodes newProjection
      constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (getArityOfSymbol a rules newProjection))
      constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (getArityOfSymbol b rules newProjection))
      constrain $ c .== (literal (-1)) .|| (c .> (literal 0) .&& c .< (getArityOfSymbol c rules newProjection))
      constrain $ d .== (literal (-1)) .|| (d .> (literal 0) .&& d .< (getArityOfSymbol d rules newProjection))
      constrain $ e .== (literal (-1)) .|| (e .> (literal 0) .&& e .< (getArityOfSymbol e rules newProjection))
    result <- checkTest value
    if result == False 
      then do
        return [False] 
      else do
        resultList <- mapM (ttt3Help rules) ([Data.List.head $ getNewReduct term r | r <- rules] \\ [term])
        let combined = Data.List.concat resultList
        return combined
        --return $ Data.List.concat $ mapM (ttt3Help rules) ([fullRewrite [r] term | r <- rules] \\ [term])
        --return bv
        -- return ([True] Data.List.++ (Data.List.concat $ Data.List.map (ttt3Help rules) ([fullRewrite [r] term | r <- rules] \\ [term])))

putValuesIntoProjection :: [SInteger] -> Projection -> Projection
putValuesIntoProjection (x:xs) ((a,b):ys) = ite (b .== 0) ((a,x) : putValuesIntoProjection xs ys) ((a,b) : putValuesIntoProjection (x:xs) ys)
putValuesIntoProjection _ [] = []


-- BORROWED FROM REST
-- | @checkSat' handles expr@ checks satisfiability of @expr@ in an instantiated SMT solver.
--   This is wrapped in a @push@ / @pop@, so it does not change the SMT environment
-- checkSat :: (Handle,  Handle) -> SMTExpr Bool -> IO Bool
-- checkSat (stdIn, stdOut) expr = do
--   sendCommands $ Push:askCmds expr
--   result <- hGetLine stdOut
--   sat <- case result of
--     "sat"   -> do
--       -- getModel stdIn
--       -- model <- readModel stdOut
--       -- putStrLn model
--       return True
--     "unsat" -> return False
--     other   -> error other
--   sendCommands [Pop]
--   return sat
--   where
--     sendCommands cmds = do
--       hPutStr stdIn $ T.unpack (T.intercalate "\n" (map commandString cmds)) ++ "\n"
--       hFlush stdIn

-- BORROWED FROM REST
-- | @checkSat expr@ launches Z3, to checks satisfiability of @expr@, terminating Z3
--   afterwards. Just a utility wrapper for `checkSat'`
--checkSat' :: SMTExpr Bool -> IO Bool
--checkSat' expr = do
  -- z3     <- spawnZ3
  -- result <- checkSat z3 expr
  -- -- killZ3 z3
  -- return result



checkTest :: SatResult -> IO Bool
checkTest result = do
  r2 <- bits
  if (show result) == (show r2) then return False else return True


-- when the symbol is not used there will be returned (literal 2)
getArityOfSymbol :: SInteger -> [Rule Char Char] -> Projection -> SInteger
getArityOfSymbol symbol rules projectionWithSymbols
  | functions == ' ' = literal 2
  | otherwise = getArityOfRule (getRuleOfFunctionSymbol functions rules)
    where
      findFunctionSymbolWithValue :: SInteger -> Projection -> Char
      findFunctionSymbolWithValue value [] = ' '
      findFunctionSymbolWithValue value ((a,b):xs) = ite (b .== value) a (findFunctionSymbolWithValue value xs)
      getRuleOfFunctionSymbol :: Char -> [Rule Char Char] -> Rule Char Char
      getRuleOfFunctionSymbol symbol (Rule lhs rhs : xs) = if (lhs == Fun symbol []) then (Rule lhs rhs) else (getRuleOfFunctionSymbol symbol xs)
      getArityOfRule :: Rule Char Char -> SInteger
      getArityOfRule (Rule (Fun _ arityList) _) = literal (toInteger (Data.List.length arityList))
      functions = (findFunctionSymbolWithValue symbol projectionWithSymbols)

buildProjection :: [Rule Char Char] -> Projection
buildProjection rules = (buildProjectionNormalSymbols ruleSymbols) Data.List.++ (buildProjectionDependencySmybols dependencySymbols)
  where dependencyRules = dependencyPairs rules rules
        ruleSymbols = findAllSymbols rules
        dependencySymbols = (findAllSymbols dependencyRules) \\ ruleSymbols

buildProjectionNormalSymbols :: String -> Projection
buildProjectionNormalSymbols = Data.List.map (\ x -> (x, - 1))

buildProjectionDependencySymbols :: String -> Projection
buildProjectionDependencySmybols (x:xs) = (x,0) : buildProjectionNormalSymbols xs
buildProjectionDependencySymbols [] = []

findAllSymbols :: [Rule Char Char] -> [Char]
findAllSymbols (Rule lhs rhs : xs) = Data.List.nub (findAllSymbolsFromTerm lhs Data.List.++ findAllSymbolsFromTerm rhs Data.List.++ findAllSymbols xs)
findAllSymbols [] = []

findAllSymbolsFromTerm :: Term Char Char -> [Char]
findAllSymbolsFromTerm (Fun a b) = a : (Data.List.nub $ Data.List.concat [findAllSymbolsFromTerm t | t <- b])
findAllSymbolsFromTerm (Var _) = []

checkIfSatisfiable :: [Rule Char Char] -> String
checkIfSatisfiable = undefined

s = sat $ do
  let sss = Fun 'f' [Var 'b', Var 'a', Var 'b']
  let t = Fun 'f' [Var 'c', Var 'a', Var 'a']
  a <- sInteger "a"
  constrain $ charToNumber (projectionPhilipp2 sss a) .> charToNumber (projectionPhilipp2 t a)
  constrain $ a .>= 0
  constrain $ a .< literal 3

teerm1 = Fun 'f' [Fun 'g' [Var 'x'], Var 'y']
teerm2 = Fun 'g' [Var 'x']

rule11 :: Rule Char Char
rule11 = Rule
    { lhs = Fun 'a' [Fun '0' [], Var 'y']
    , rhs = Var 'y'
    }

ssss = (Fun '1' [Fun 's' [Var 'x'], Var 'y'])
tttt = (Fun '1' [Var 'x', Var 'y'])

rule12 :: Rule Char Char
rule12 = Rule
    { lhs = Fun 'a' [Fun 's' [Var 'x'], Var 'y']
    , rhs = Fun 's' [Fun 'a' [Var 'x', Var 'y']]
    }

rule13 :: Rule Char Char
rule13 = Rule
    { lhs = Fun 'h' [Fun '0' []]
    , rhs = Fun '0' []
    }

rule14 :: Rule Char Char
rule14 = Rule
    { lhs = Fun 'h' [Fun 's' [Fun '0' []]]
    , rhs = Fun '0' []
    }

rule15 :: Rule Char Char
rule15 = Rule
    { lhs = Fun 'h' [Fun 's' [Fun 's' [Var 'x']]]
    , rhs = Fun 's' [Fun 'h' [Var 'x']]
    }

rule16 :: Rule Char Char
rule16 = Rule
    { lhs = Fun 'b' [Fun '0' []]
    , rhs = Fun 'b' [Fun 'b' [Fun '0' []]]
    }

rule17 :: Rule Char Char
rule17 = Rule
    { lhs = Fun 'b' [Fun 's' [Var 'x']]
    , rhs = Fun 's' [Fun 'b' [Fun 'h' [Fun 's' [Var 'x']]]]
    }


rule18 :: Rule Char Char
rule18 = Rule
    { lhs = Fun 'a' [Var 'x']
    , rhs = Fun 'b' [Var 'x']
    }

rule19 :: Rule Char Char
rule19 = Rule
    { lhs = Fun 'b' [Var 'x']
    , rhs = Fun 'a' [Var 'x']
    }

rppp = [('b', literal ((1)::Integer)), ('1', literal ((1)::Integer))]
rrrr = geq (Fun '1' [Fun '0' []]) (Fun '1' [Fun 'b' [Fun '0' []]]) rppp

tttBits = [rule13, rule14, rule15, rule16, rule17]


sanity = [rule18, rule19]
dependencyRulesSanity = dependencyPairs sanity sanity

dependencyRulesBits = dependencyPairs tttBits tttBits


rule20 :: Rule Char Char
rule20 = Rule
    { lhs = Fun 'f' [Var 'x']
    , rhs = Fun 'g' [Var 'x']
    }

rule21 :: Rule Char Char
rule21 = Rule
    { lhs = Fun 'g' [Var 'x']
    , rhs = Fun 'f' [Var 'x']
    }

rule21b :: Rule Char Char
rule21b = Rule
    { lhs = Fun 'g' [Var 'x']
    , rhs = Fun 'j' [Var 'x']
    }

rule22 :: Rule Char Char
rule22 = Rule
    { lhs = Fun 'i' [Fun 'k' [Var 'x']]
    , rhs = Fun 'j' [Var 'x']
    }

rule23 :: Rule Char Char
rule23 = Rule
    { lhs = Fun 'j' [Fun 's' [Var 'x']]
    , rhs = Fun 'i' [Var 'x']
    }

----- TTT Bits which should terminate -------

bitsRule1 :: Rule Char Char
bitsRule1 = Rule
    { lhs = Fun 'h' [Fun '0' []]
    , rhs = Fun '0' []
    }

bitsRule2 :: Rule Char Char
bitsRule2 = Rule
    { lhs = Fun 'h' [Fun 's' [Fun '0' []]]
    , rhs = Fun '0' []
    }

bitsRule3 :: Rule Char Char
bitsRule3 = Rule
    { lhs = Fun 'h' [Fun 's' [Fun 's' [Var 'x']]]
    , rhs = Fun 's' [Fun 'h' [Var 'x']]
    }

bitsRule4 :: Rule Char Char
bitsRule4 = Rule
    { lhs = Fun 'b' [Fun '0' []]
    , rhs = Fun '0' []
    }

bitsRule5 :: Rule Char Char
bitsRule5 = Rule
    { lhs = Fun 'b' [Fun 's' [Var 'x']]
    , rhs = Fun 's' [Fun 'b' [Fun 'h' [Fun 's' [Var 'x']]]]
    }

bitsRules = [bitsRule1, bitsRule2, bitsRule3, bitsRule4, bitsRule5]
bitsDependencyRules = dependencyPairs bitsRules bitsRules

bits = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $ geqRules bitsDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  --constrain $ neqRules bitsDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  --constrain $ rtRules mulRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 2))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))
{-
Ergebnis Philipp & Luca: Unsatisfiable
Ergebnis TTT2: Satisfiable
-}
-------------------------------------------------

----- TTT Bits which should not terminate -------

bitsWRule1 :: Rule Char Char
bitsWRule1 = Rule
    { lhs = Fun 'h' [Fun '0' []]
    , rhs = Fun '0' []
    }

bitsWRule2 :: Rule Char Char
bitsWRule2 = Rule
    { lhs = Fun 'h' [Fun 's' [Fun '0' []]]
    , rhs = Fun '0' []
    }

bitsWRule3 :: Rule Char Char
bitsWRule3 = Rule
    { lhs = Fun 'h' [Fun 's' [Fun 's' [Var 'x']]]
    , rhs = Fun 's' [Fun 'h' [Var 'x']]
    }

bitsWRule4 :: Rule Char Char
bitsWRule4 = Rule
    { lhs = Fun 'b' [Fun '0' []]
    , rhs = Fun 'b' [Fun 'b' [Fun '0' []]]
    }

bitsWRule5 :: Rule Char Char
bitsWRule5 = Rule
    { lhs = Fun 'b' [Fun 's' [Var 'x']]
    , rhs = Fun 's' [Fun 'b' [Fun 'h' [Fun 's' [Var 'x']]]]
    }

bitsWRules = [bitsWRule1, bitsWRule2, bitsWRule3, bitsWRule4, bitsWRule5]
bitsWDependencyRules = dependencyPairs bitsWRules bitsWRules

bitsW = allSat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $ geqRules bitsWDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  constrain $ neqRules bitsWDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  --constrain $ rtRules mulRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 2))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))
{-
Ergebnis Philipp & Luca: Unsatisfiable
Ergebnis TTT2: Unsatisfiable
-}
-------------------------------------------------

----- TTT add Rules termination example -------

addRule1 :: Rule Char Char
addRule1 = Rule
    { lhs = Fun 'a' [Fun '0' [], Var 'y']
    , rhs = Var 'y'
    }

addRule2 :: Rule Char Char
addRule2 = Rule
    { lhs = Fun 'a' [Fun 's' [Var 'x'], Var 'y']
    , rhs = Fun 's' [Fun 'a' [Var 'x', Var 'y']]
    }


addRules = [addRule1, addRule2]
addDependencyRules = dependencyPairs addRules addRules

add = allSat $ do
  a <- sInteger "a"
  constrain $ geqRules addDependencyRules [('1', a), ('s', -1)]
  constrain $ neqRules addDependencyRules [('1', a), ('s', -1)]
  --constrain $ rtRules mulRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 3))

{-
Ergebnis Philipp & Luca: Satisfiable
Ergebnis TTT2: Satisfiable
-}
---------------------------------------------------



----- TTT mul Rules termination example -------

mulRule1 :: Rule Char Char
mulRule1 = Rule
    { lhs = Fun 'a' [Fun '0' [], Var 'y']
    , rhs = Var 'y'
    }

mulRule2 :: Rule Char Char
mulRule2 = Rule
    { lhs = Fun 'a' [Fun 's' [Var 'x'], Var 'y']
    , rhs = Fun 's' [Fun 'a' [Var 'x', Var 'y']]
    }

mulRule3 :: Rule Char Char
mulRule3 = Rule
    { lhs = Fun 'm' [Fun '0' [], Var 'y']
    , rhs = Fun '0' []
    }

mulRule4 :: Rule Char Char
mulRule4 = Rule
    { lhs = Fun 'm' [Fun 's' [Var 'x'], Var 'y']
    , rhs = Fun 'a' [Fun 'm' [Var 'x', Var 'y'], Var 'y']
    }

mulRules = [mulRule1, mulRule2, mulRule3, mulRule4]
mulDependencyRules = dependencyPairs mulRules mulRules

mul = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $ geqRules mulDependencyRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ neqRules mulDependencyRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  --constrain $ rtRules mulRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 3))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 3))

{-
Ergebnis Philipp & Luca: Unsatisfiable
Ergebnis TTT2: Satisfiable
-}
---------------------------------------------------

----- TTT Rules for non termination example -------

mulWRule1 :: Rule Char Char
mulWRule1 = Rule
    { lhs = Fun 'a' [Fun '0' [], Var 'y']
    , rhs = Var 'y'
    }

mulWRule2 :: Rule Char Char
mulWRule2 = Rule
    { lhs = Fun 'a' [Fun 's' [Var 'x'], Var 'y']
    , rhs = Fun 's' [Fun 'a' [Var 'x', Var 'y']]
    }

mulWRule3 :: Rule Char Char
mulWRule3 = Rule
    { lhs = Fun 'm' [Fun '0' [], Var 'y']
    , rhs = Fun '0' []
    }

mulWRule4 :: Rule Char Char
mulWRule4 = Rule
    { lhs = Fun 'm' [Var 'x', Var 'y']
    , rhs = Fun 'm' [Fun 's' [Var 'x'], Var 'y']
    }

mulWRules = [mulWRule1, mulWRule2, mulWRule3, mulWRule4]
mulWDependencyRules = dependencyPairs mulWRules mulWRules

mulW = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $ geqRules mulWDependencyRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ neqRules mulWDependencyRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  --constrain $ rtRules mulWRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 2))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))

{-
Ergebnis Philipp & Luca: Unsatisfiable
Ergebnis TTT2: Unsatisfiable
-}
---------------------------------------------------

sccNavigationRules = [rule20, rule21, rule21b, rule22, rule23]
sccNavigationDependencyRules = dependencyPairs sccNavigationRules sccNavigationRules
graph = getSccFromDependencyPairs sccNavigationDependencyRules
numbers = definedSymbols sccNavigationRules
preparation = sccPrepare sccNavigationDependencyRules 1
sccEdges = getVertices preparation preparation
testSCCReachable = reachableNodesFromTerm sccNavigationRules (Fun 'i' [Fun 'k' [Var 'x']])



b = allSat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sInteger "d"
  e <- sInteger "e"
  constrain $ geqRules dependencyRulesBits [('1', a), ('2', b)]
  constrain $ neqRules dependencyRulesBits [('1', a), ('2', b)]
  constrain $ rtRules tttBits [('1', a), ('2', b)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 2))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))

satSanity = allSat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  d <- sInteger "d"
  e <- sInteger "e"
  constrain $ geqRules dependencyRulesSanity [('a', a), ('b', b), ('1', d), ('2', e)]
  constrain $ neqRules dependencyRulesSanity [('a', a), ('b', b), ('1', d), ('2', e)]
  constrain $ rtRules sanity [('a', a), ('b', b), ('1', d), ('2', e)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 2))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))
  constrain $ d .== (literal (-1)) .|| (d .> (literal 0) .&& d .< (literal 2))
  constrain $ e .== (literal (-1)) .|| (e .> (literal 0) .&& e .< (literal 2))

j = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sInteger "d"
  e <- sInteger "e"
  f <- sInteger "f"
  g <- sInteger "g"
  constrain $ geqRules drules [('1', a), ('2', b), ('3', c), ('4', d), ('5', e), ('6', f), ('7', g)] .== sTrue
  constrain $ neqRules drules [('1', a), ('2', b), ('3', c), ('4', d), ('5', e), ('6', f), ('7', g)] .== sTrue


main :: IO ()
main = do
  result <- bits
  aaa <- Main.s
  bits1 <- bits
  bits2 <- bits
  print ((show bits1) == (show bits2))