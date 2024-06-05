{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
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


examplerules = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]
drules = dependencyPairs examplerules examplerules
resultExample = getSccFromDependencyPairs drules
see = sccPrepare drules 1
index = [drules Data.List.!! 7, drules Data.List.!! 9]
sccTest = getSccFromDependencyPairs (dependencyPairs rulesTest rulesTest)
findScc = findSccNode drules (sccPrepare drules 1) [7,9]

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

s = allSat $ do
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

teeeest = [rule11, rule12]
dependencyRules = dependencyPairs teeeest teeeest

k = allSat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  constrain $ geqRules dependencyRules [('a', a), ('s', b), ('1', c)]
  constrain $ neqRules dependencyRules [('a', a), ('s', b), ('1', c)]
  --constrain $ rtRules teeeest [('a', a), ('s', b), ('1', c)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 3))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))
  constrain $ c .== (literal (-1)) .|| (c .> (literal 0) .&& c .< (literal 3))
  

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
  result <- k
  print result