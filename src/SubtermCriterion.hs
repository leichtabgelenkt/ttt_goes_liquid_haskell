module SubtermCriterion where
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
import qualified Data.SBV as SBV
import qualified Data.SBV.List as SBVList
import qualified Data.List as SBVInternals
import Data.SBV.Internals
import Data.List
import MySCCGraph
import Multiplicity
import Rest
import Rules
import DependencyPairs
import Data.SBV
import Data.SBV.List
import Data.SBV.Internals
import Data.SBV.Trans (getModelValue)


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
neqRules [] _ = sTrue
neqRules rules p = sOr [neq (getLHS rule) (getRHS rule) p | rule <- rules]

rt :: Term Char Char -> Projection -> SBool
rt t p = sNot $ sIsNotProjecting t p

rtRules :: [Rule Char Char] -> Projection -> SBool
rtRules rules p = sAnd [ite (rt lhs p) (geq lhs rhs p) (sTrue) | (Rule rhs lhs) <- rules]

isUnsatisfiable :: SMTResult -> Bool
isUnsatisfiable (Unsatisfiable _ _) = True
isUnsatisfiable _     = False

getArityOfSymbol :: Char -> [Rule Char Char] -> SInteger
getArityOfSymbol _ [] = literal 1
getArityOfSymbol symbol ((Rule lhs rhs) : xs) = help symbol lhs
  where help c (Fun a b) = if c == a then literal (toInteger $ Data.List.length b) else getArityOfSymbol c xs

buildProjection :: [Rule Char Char] -> Projection
buildProjection rules = (buildProjectionNormalSymbols ruleSymbols) Data.List.++ (buildProjectionDependencySymbols dependencySymbols)
  where dependencyRules = dependencyPairs rules rules
        ruleSymbols = findAllSymbols rules
        dependencySymbols = (findAllSymbols dependencyRules) \\ ruleSymbols

buildProjectionNormalSymbols :: String -> Projection
buildProjectionNormalSymbols = Data.List.map (\ x -> (x, -1))

buildProjectionDependencySymbols :: String -> Projection
buildProjectionDependencySymbols (x:xs) = (x,0) : buildProjectionDependencySymbols xs
buildProjectionDependencySymbols [] = []

range = ['1','2','3','4','5','6','7','8','9']

findAllSymbols :: [Rule Char Char] -> [Char]
findAllSymbols (Rule lhs rhs : xs) = Data.List.nub (findAllSymbolsFromTerm lhs Data.List.++ findAllSymbolsFromTerm rhs Data.List.++ findAllSymbols xs)
findAllSymbols [] = []

findAllSymbolsFromTerm :: Term Char Char -> [Char]
findAllSymbolsFromTerm (Fun a b) = a : (Data.List.nub $ Data.List.concat [findAllSymbolsFromTerm t | t <- b])
findAllSymbolsFromTerm (Var _) = []

putValuesIntoProjection :: [SInteger] -> Projection -> Projection
putValuesIntoProjection (x:xs) ((a,b):ys) = ite (b .== 0) ((a,x) : putValuesIntoProjection xs ys) ((a,b) : putValuesIntoProjection (x:xs) ys)
putValuesIntoProjection _ [] = []

ttt3 :: [Rule Char Char] -> Term Char Char -> IO String
ttt3 rules term = do
  let dependencyRules = dependencyPairs rules rules
      projection = buildProjection rules
  wholeTrsResult <- getSatResult dependencyRules projection dependencyRules
  wholeTrsBoolean <- checkTest wholeTrsResult
  if wholeTrsBoolean
    then return ("Success!! The term \"" Data.List.++ (show term) Data.List.++ "\" terminates with the given rules, using the subterm criterion")
    else do
      result <- ttt3Help rules [] term
      if and result
        then return ("Success!! The term \"" Data.List.++ (show term) Data.List.++ "\" terminates with the given rules, using the subterm criterion")
        else return ("The term \"" Data.List.++ (show term) Data.List.++ "\" does NOT terminate with the given rules, using the subterm criterion")

ttt3Help :: [Rule Char Char] -> [Term Char Char] -> Term Char Char -> IO [Bool]
ttt3Help rules@(x:xs) seenTerms term
 | (fullRewrite rules term) == [] = return [True]
 | otherwise = do
    let dependencyRules = dependencyPairs rules rules
        reachableNodes = reachableNodesFromTerm rules term
        reachableRulesFromNodes = findSccNode dependencyRules (sccPrepare dependencyRules 1) reachableNodes
        projection = buildProjection rules
        prepare = sccPrepare dependencyRules 1
        edges = getEdges prepare prepare
        scc = getSccFromDependencyPairs dependencyRules
        reachableAndInSCCNodes = nub $ reachableAndInSCC reachableNodes reachableNodes scc scc
        importantRules = Data.List.map (findSccNode dependencyRules (sccPrepare dependencyRules 1)) reachableAndInSCCNodes
    checkedValues <- mapM (intermediateStep dependencyRules projection) importantRules
    let result = and checkedValues
    if result == False 
      then do
        return [False] 
      else do
          let newTerms = (findAllNewTerms rules term)
          if term `Data.List.elem` seenTerms
            then do
              return [False]
            else do
              let allSeenTerms = term : seenTerms
              resultList <- mapM (ttt3Help rules allSeenTerms) ((Data.List.nub $ newTerms) \\ [term])
              let combined = Data.List.concat resultList
              return combined


intermediateStep :: [Rule Char Char] -> Projection -> [Rule Char Char] -> IO Bool
intermediateStep dependencyRules projection rules = do
  satRes <- getSatResult dependencyRules projection rules
  satBoolean <- checkTest satRes
  if satBoolean
    then do
      return True
    else do
      return False
      -- putStrLn "Not Here, surely"
      -- satisfiability <- iterativeMethod dependencyRules projection rules
      -- return satisfiability

getSatResult :: [Rule Char Char] -> Projection -> [Rule Char Char] -> IO SatResult
getSatResult dependencyRules projection rules = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sInteger "d"
  e <- sInteger "e"
  f <- sInteger "f"
  g <- sInteger "g"
  h <- sInteger "h"
  i <- sInteger "i"
  let
    constrainList :: [SInteger]
    constrainList = [a,b,c,d,e,f,g,h,i]
  let
    newProjection :: Projection
    newProjection = putValuesIntoProjection constrainList projection
  constrain $ geqRules rules newProjection
  constrain $ neqRules rules newProjection
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .<= (getArityOfSymbol (getSymbol newProjection 0) dependencyRules))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .<= (getArityOfSymbol (getSymbol newProjection 1) dependencyRules))
  constrain $ c .== (literal (-1)) .|| (c .> (literal 0) .&& c .<= (getArityOfSymbol (getSymbol newProjection 2) dependencyRules))
  constrain $ d .== (literal (-1)) .|| (d .> (literal 0) .&& d .<= (getArityOfSymbol (getSymbol newProjection 3) dependencyRules))
  constrain $ e .== (literal (-1)) .|| (e .> (literal 0) .&& e .<= (getArityOfSymbol (getSymbol newProjection 4) dependencyRules))
  constrain $ f .== (literal (-1)) .|| (f .> (literal 0) .&& f .<= (getArityOfSymbol (getSymbol newProjection 5) dependencyRules))
  constrain $ g .== (literal (-1)) .|| (g .> (literal 0) .&& g .<= (getArityOfSymbol (getSymbol newProjection 6) dependencyRules))
  constrain $ h .== (literal (-1)) .|| (h .> (literal 0) .&& h .<= (getArityOfSymbol (getSymbol newProjection 7) dependencyRules))
  constrain $ i .== (literal (-1)) .|| (i .> (literal 0) .&& i .<= (getArityOfSymbol (getSymbol newProjection 8) dependencyRules))

findAllNewTerms :: [Rule Char Char] -> Term Char Char -> [Term Char Char]
findAllNewTerms (x:xs) term = if (getNewReduct term x) == [] then findAllNewTerms xs term else (getNewReduct term x) Data.List.++ (findAllNewTerms xs term)
findAllNewTerms [] _ = []

checkTest :: SatResult -> IO Bool
checkTest result = do
  r2 <- bits
  if (show result) == (show r2) then return False else return True

getSymbol :: [(Char, SInteger)] -> Integer -> Char
getSymbol [] _ = '1'
getSymbol ((c,_):xs) 0 = if c `Data.List.elem` range then c else getSymbol xs 0
getSymbol ((c,_):xs) y = if c `Data.List.elem` range then getSymbol xs (y-1) else getSymbol xs y

bits = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $ geqRules bitsDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  --constrain $ neqRules bitsDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  --constrain $ rtRules mulRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 2))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))