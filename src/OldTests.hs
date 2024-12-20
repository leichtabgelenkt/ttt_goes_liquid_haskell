module OldTests where
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
import DependencyPairs
import Rules
import Rest
import Data.SBV
import Data.SBV.List
import Data.SBV.Internals
import Data.SBV.Trans (getModelValue)
import SubtermCriterion

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



bits2 = optimize Lexicographic $ do
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $ geqRules bitsDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  --constrain $ neqRules bitsDependencyRules [('1', a), ('2', b), ('s', -1), ('b', -1), ('h', -1), ('s', -1), ('0', -1)]
  --constrain $ rtRules mulRules [('1', a), ('2', b), ('m', -1), ('a', -1), ('s', -1), ('0', -1)]
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .< (literal 2))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .< (literal 2))
  let sumVars = a + b
  maximize "Sum of aa to ii" sumVars
{-
Ergebnis Philipp & Luca: Unsatisfiable
Ergebnis TTT2: Satisfiable
-}
-------------------------------------------------

----- TTT Bits which should not terminate -------

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

add = sat $ do
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
sccEdges = getEdges preparation preparation
testSCCReachable = reachableNodesFromTerm sccNavigationRules (Fun 'i' [Fun 'k' [Var 'x']])


-------- old REST-implementation test --------
-- Example 1 of Rest

-- n is intersection
-- u is union
-- s means s0
-- t means s1
example1RuleSet :: [Rule Char Char]
example1RuleSet = [distribUnion, idemInter, disjointnessAss, emptyUnion]

example1StartTerm :: Term Char Char
example1StartTerm = Fun 'n' [Fun 'u' [Fun 's' [], Fun 't' []], Fun 's' []]

uuuuu :: Rule Char Char
uuuuu = Rule
    { lhs = Fun 'f' [Var 'x']
    , rhs = Fun 'f' [Var 'x'] 
    }

uuStart :: Term Char Char
uuStart = Fun 'f' [Fun 'a' []]

example1ResultTerms :: RewriteSequence
example1ResultTerms = rest [uuuuu] uuStart
-- =  [Fun 'n' [Fun 'u' [Var 's',Var 't'],Var 's'],Fun 'u' [Fun 'n' [Var 's',Var 's'],Fun 'n' [Var 't',Var 's']]]


example2RuleSet :: [Rule Char Char]
example2RuleSet = [distribInter, idemUnion, commutUnion, subsetAss, idemInter]

example2StartTerm :: Term Char Char
example2StartTerm = Fun 'u' [Fun 'n' [Var 's', Var 't'], Var 's']

example2ResultTerms :: RewriteSequence
example2ResultTerms = rest example2RuleSet example2StartTerm

-- no solution so far because of the commutativity

-- Example 3 of Rest
-- currently not possible because of associativity and commutativity

---------------------------------------------------