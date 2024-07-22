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

    