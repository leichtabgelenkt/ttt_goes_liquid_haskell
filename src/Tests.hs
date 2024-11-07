module Tests where

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

import Multiplicity

term1 = Fun 'f' [Var 'x', Var 'y']
term2 = Fun 'g' [Fun 'h' [Var 'x'], Fun 'f' [Var 'x', Var 'y']]
term3 = Fun 'h' [Fun 'f' [Var 'z', Var 'x']]
term4 = Fun 'f' [Var 'x', Var 'z']

projection1 :: Projection
projection1 = [('f',[1]),('g',[]),('h',[1])]
projection2 = [('f',[1]),('g',[1,2]),('h',[1])]
projection3 = [('f',[]),('g',[1,2]),('h',[1])]


-- Test cases for the multiplicity function
test1 = multiplicity 1 term1 (Var 'x') projection1 == 1
test2 = multiplicity 1 term1 (Var 'y') projection1 == 0
test3 = multiplicity 1 term2 (Var 'x') projection1 == 0
test4 = multiplicity 1 term2 (Var 'x') projection2 == 2
test5 = multiplicity 1 term2 term1 projection1 == 0
test6 = multiplicity 1 term2 term1 projection3 == 1

main :: IO ()
main = do
    putStrLn "Running tests..."
    putStrLn $ "Test 1: " ++ show test1
    putStrLn $ "Test 2: " ++ show test2
    putStrLn $ "Test 3: " ++ show test3
    putStrLn $ "Test 4: " ++ show test4
    putStrLn $ "Test 5: " ++ show test5
    putStrLn $ "Test 6: " ++ show test6