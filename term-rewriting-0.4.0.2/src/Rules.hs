module Rules where
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
    -- Define a rules
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