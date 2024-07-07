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

    set1rules = [set1rule1, set1rule2]

    set1term1 = Fun 'f' [Var 'z']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    -}

    set1term2 = Fun 'h' [Var 'z']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    -}

    set1term3 = Fun 'g' [Fun 'f' [Var 'z']]
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
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

    set2rules = [set2rule1, set2rule2]

    set2term1 = Fun 'f' [Var 'z']
    {-
    Expected Outcome: Does not terminate
    Actual Outcome: Does not terminate
    -}

    set2term2 = Fun 'h' [Var 'z']
    {-
    Expected Outcome: Terminates
    Actual Outcome: Terminates
    -}

    set2term3 = Fun 'g' [Fun 'f' [Var 'z']]
    {-
    Expected Outcome: Does not terminate
    Actual Outcome: Does not terminate
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
    -}

    ------------------------------------------------------------------------



    --------------- TestSet from termrewriting slides 13, slide 37-39 -----------------------------------
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
    
    set4term1 = Fun 'q' [Fun ':' [Var 'n', Var 'x']]
    {-
    Expected: Terminates
    Actual: Doesn't terminate
    -}