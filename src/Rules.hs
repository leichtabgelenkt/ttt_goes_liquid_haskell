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
    import DependencyPairs
    import Rest
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

    