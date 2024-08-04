module TTT3TestSets where
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

    --------------- TestSet1 -------------------------
    set1rule1 :: Rule Char Char
    set1rule1 = Rule
        { lhs = Fun 'f' [Var 'x']
        , rhs = Fun 'g' [Var 'x']
        }

    set1rule2 :: Rule Char Char
    set1rule2 = Rule
        { lhs = Fun 'g' [Var 'x']
        , rhs = Fun 'a' []
        }

    set1Rules = [set1rule1, set1rule2]

    set1Term1 = Fun 'f' [Var 'z']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.00s
    -}

    set1Term2 = Fun 'h' [Var 'z']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.00s
    -}

    set1Term3 = Fun 'g' [Fun 'f' [Var 'z']]
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.00s
    -}

    --------------------------------------------------



    --------------- TestSet2 -------------------------
    set2rule1 :: Rule Char Char
    set2rule1 = Rule
        { lhs = Fun 'f' [Var 'x']
        , rhs = Fun 'g' [Var 'x']
        }

    set2rule2 :: Rule Char Char
    set2rule2 = Rule
        { lhs = Fun 'g' [Var 'x']
        , rhs = Fun 'f' [Var 'x']
        }

    set2rule3 :: Rule Char Char
    set2rule3 = Rule
        { lhs = Fun 'h' [Var 'x']
        , rhs = Fun 'a' []
        }

    set2Rules = [set2rule1, set2rule2, set2rule3]

    set2Term1 = Fun 'f' [Var 'z']
    {-
    Expected Outcome: Does not terminate
    Actual Outcome: Does not terminate
    Time elapsed: 0.97s
    -}

    set2Term2 = Fun 'h' [Var 'z']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.00s
    -}

    set2Term3 = Fun 'g' [Fun 'f' [Var 'z']]
    {-
    Expected Outcome: Does not terminate
    Actual Outcome: Does not terminate
    Time elapsed: 2.52s
    -}

    --------------------------------------------------


    --------------- TestSet from termrewriting slides 13, slide 22 -----------------------------------
    cycleRule1 :: Rule Char Char
    cycleRule1 = Rule
        { lhs = Fun '+' [Fun '0' [], Var 'y']
        , rhs = Var 'y'
        }

    cycleRule2 :: Rule Char Char
    cycleRule2 = Rule
        { lhs = Fun '+' [Fun 's' [Var 'x'], Var 'y']
        , rhs = Fun 's' [Fun '+' [Var 'x', Var 'y']]
        }

    cycleRule3 :: Rule Char Char
    cycleRule3 = Rule
        { lhs = Fun '*' [Fun '0' [], Var 'y']
        , rhs = Fun '0' []
        }

    cycleRule4 :: Rule Char Char
    cycleRule4 = Rule
        { lhs = Fun '*' [Fun 's' [Var 'x'], Var 'y']
        , rhs = Fun '+' [Fun '*' [Var 'x', Var 'y'], Var 'y']
        }

    cycleRules = [cycleRule1, cycleRule2, cycleRule3, cycleRule4]
    cycleTerm = Fun '*' [Fun 's' [Var 'x'], Var 'y']
    {-
    Expected: Terminates
    Actual: Terminates
    Time elapsed: 0.18s
    -}

    ------------------------------------------------------------------------



    --------------- TestSet5; TestSet from termrewriting slides 13, slides 37-39 -----------------------------------
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

    set4Rules = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]
    
    set4Term1 = Fun 'q' [Fun ':' [Var 'n', Var 'x']]
    {-
    Expected: Terminates (But only with help of LPO)
    Actual: Doesn't terminate
    Comment: Aquivalent of starting at Node 13 from the slides
    Time elapsed: 2.04s
    -}

    set4Term2 = Fun 'l' [Var 'n', Fun ':' [Var 'm', Var 'x']]
    {-
    Expected: Terminates
    Actual: Terminates
    Comment: Aquivalent of starting at Node 1 from the slides
    Time elapsed: 1.03s
    -}


    -------------------------------------------------------------------------------------


    --------------- TestSet5; TestSet from termrewriting slides 6, slide 8 -------------------------
    set5rule1 :: Rule Char Char
    set5rule1 = Rule
        { lhs = Fun '*' [Fun 'e' [], Var 'x']
        , rhs = Var 'x'
        }

    set5rule2 :: Rule Char Char
    set5rule2 = Rule
        { lhs = Fun '*' [Fun '-' [Var 'x'], Var 'x']
        , rhs = Fun 'e' []
        }

    set5rule3 :: Rule Char Char
    set5rule3 = Rule
        { lhs = Fun '*' [Fun '*' [Var 'x', Var 'y'], Var 'z']
        , rhs = Fun '*' [Var 'x', Fun '*' [Var 'y', Var 'z']]
        }

    set5rule4 :: Rule Char Char
    set5rule4 = Rule
        { lhs = Fun '-' [Fun 'e' []]
        , rhs = Fun 'e' []
        }

    set5rule5 :: Rule Char Char
    set5rule5 = Rule
        { lhs = Fun '*' [Fun '-' [Var 'x'], Fun '*' [Var 'x', Var 'y']]
        , rhs = Var 'y'
        }

    set5rule6 :: Rule Char Char
    set5rule6 = Rule
        { lhs = Fun '*' [Var 'x', Fun 'e' []]
        , rhs = Var 'x'
        }

    set5rule7 :: Rule Char Char
    set5rule7 = Rule
        { lhs = Fun '*' [Var 'x', Fun '-' [Var 'x']]
        , rhs = Fun 'e' []
        }

    set5rule8 :: Rule Char Char
    set5rule8 = Rule
        { lhs = Fun '-' [Fun '-' [Var 'x']]
        , rhs = Var 'x'
        }

    set5rule9 :: Rule Char Char
    set5rule9 = Rule
        { lhs = Fun '-' [Fun '*' [Var 'x', Var 'y']]
        , rhs = Fun '*' [Fun '-' [Var 'y'], Fun '-' [Var 'x']]
        }

    set5rule10 :: Rule Char Char
    set5rule10 = Rule
        { lhs = Fun '*' [Var 'x', Fun '*' [Fun '-' [Var 'x'], Var 'y']]
        , rhs = Var 'y'
        }

    set5Rules = [set5rule1, set5rule2, set5rule3, set5rule4, set5rule5, set5rule6, set5rule7, set5rule8, set5rule9, set5rule10]

    set5Term1 = Fun '*' [Fun 'e' [], Var 'x']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.04s
    -}

    set5Term2 = Fun '*' [Fun '*' [Fun 'e' [], Var 'x'], Fun '-' [Var 'x']]
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.15s
    -}

    set5Term3 = Fun '*' [Fun 'e' [], Fun '*' [Fun '*' [Fun 'e' [], Var 'y'], Var 'z']]
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.09s
    -}

    set5Term4 = Fun '*' [Fun 'e' [], Fun '*' [Fun '*' [Fun 'e' [], Var 'y'], Fun '-' [Fun '-' [Fun '*' [Var 'x', Fun '*' [Fun '-' [Var 'x'], Var 'z']]]]]]
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 22.69s
    -}

    --------------------------------------------------


    --------------- TestSet6; TestSet from termrewriting book, page 81 -------------------------
    set6rule1 :: Rule Char Char
    set6rule1 = Rule
        { lhs = Fun 'p' []
        , rhs = Fun 'v' [Fun 'f' [Fun 's' [Fun 's' [Fun '0' []]]]]
        }

    set6rule2 :: Rule Char Char
    set6rule2 = Rule
        { lhs = Fun 'f' [Var 'x']
        , rhs = Fun ':' [Var 'x', Fun 'f' [Fun 's' [Var 'x']]]
        }

    set6rule3 :: Rule Char Char
    set6rule3 = Rule
        { lhs = Fun 'h' [Fun ':' [Var 'x', Var 'y']]
        , rhs = Var 'x'
        }

    set6rule4 :: Rule Char Char
    set6rule4 = Rule
        { lhs = Fun 't' [Fun ':' [Var 'x', Var 'y']]
        , rhs = Var 'y'
        }

    set6rule5 :: Rule Char Char
    set6rule5 = Rule
        { lhs = Fun 'v' [Fun ':' [Fun '0' [], Var 'y']]
        , rhs = Fun 'v' [Var 'y']
        }

    set6rule6 :: Rule Char Char
    set6rule6 = Rule
        { lhs = Fun 'v' [Fun 's' [Var 'x'], Var 'y']
        , rhs = Fun ':' [Fun 's' [Var 'x'], Fun 'v' [Fun 'r' [Var 'x', Var 'y', Var 'x']]]
        }

    set6rule7 :: Rule Char Char
    set6rule7 = Rule
        { lhs = Fun 'r' [Fun '0' [], Fun ':' [Var 'y', Var 'z'], Var 'w']
        , rhs = Fun ':' [Fun '0' [], Fun 'r' [Var 'w', Var 'z', Var 'w']]
        }

    set6rule8 :: Rule Char Char
    set6rule8 = Rule
        { lhs = Fun 'r' [Fun 's' [Var 'x'], Fun ':' [Var 'y', Var 'z'], Var 'w']
        , rhs = Fun ':' [Var 'y', Fun 'r' [Var 'x', Var 'z', Var 'w']]
        }


    set6Rules = [set6rule1, set6rule2, set6rule3, set6rule4, set6rule5, set6rule6, set6rule7, set6rule8]

    set6Term1 = Fun 'f' [Var 'x']
    {-
    Expected Outcome: Does not terminate
    Actual Outcome: Does not terminate
    Time elapsed: 0.24s
    -}

    set6Term2 = Fun 'r' [Fun 's' [Var 'x'], Fun ':' [Var 'y', Var 'z'], Var 'w']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.02s
    -}

    set6Term3 = Fun 'v' [Fun 's' [Fun 'h' [Fun ':' [Var 'x', Var 'y']]], Fun 't' [Fun ':' [Var 'x', Var 'y']]]
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 1.33s
    -}

    set6Term4 = Fun 'p' []
    {-
    Expected Outcome: Does not terminate
    Actual Outcome: Does not terminate
    Time elapsed: 0.58s
    -}

    --------------------------------------------------

    --------------- TestSet7; TestSet from the equations on sets -------------------------
    set7rule1 :: Rule Char Char
    set7rule1 = Rule
        { lhs = Fun 'u' [Var 'x', Var 'x']
        , rhs = Var 'x'
        }

    set7rule2 :: Rule Char Char
    set7rule2 = Rule
        { lhs = Fun 'i' [Var 'x', Var 'x']
        , rhs = Var 'x'
        }

    set7rule3 :: Rule Char Char
    set7rule3 = Rule
        { lhs = Fun 'u' [Var 'x', Fun 'e' []]
        , rhs = Var 'x'
        }

    set7rule4 :: Rule Char Char
    set7rule4 = Rule
        { lhs = Fun 'i' [Var 'x', Fun 'e' []]
        , rhs = Fun 'e' []
        }

    set7rule5 :: Rule Char Char
    set7rule5 = Rule
        { lhs = Fun 'u' [Fun 'i' [Var 'x', Var 'z'], Fun 'i' [Var 'y', Var 'z']]
        , rhs = Fun 'i' [Fun 'u' [Var 'x', Var 'y'], Var 'z']
        }

    set7rule6 :: Rule Char Char
    set7rule6 = Rule
        { lhs = Fun 'u' [Fun 'i' [Var 'x', Var 'y'], Var 'y']
        , rhs = Fun 'i' [Fun 'u' [Var 'x', Var 'y'], Var 'y']
        }

    set7rule7 :: Rule Char Char
    set7rule7 = Rule
        { lhs = Fun 'u' [Var 'x', Fun 'i' [Var 'y', Var 'x']]
        , rhs = Fun 'i' [Fun 'u' [Var 'x', Var 'y'], Var 'x']
        }


    set7Rules = [set7rule1, set7rule2, set7rule3, set7rule4, set7rule5, set7rule6, set7rule7]

    set7Term1 = Fun 'u' [Fun 'i' [Var 'x', Var 'y'], Fun 'u' [Var 'y', Fun 'e' []]]
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    Time elapsed: 0.01s
    -}
    --------------------------------------------------