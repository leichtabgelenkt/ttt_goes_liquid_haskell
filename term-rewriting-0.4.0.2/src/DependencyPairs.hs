module DependencyPairs where
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
    import Rest
    
    -- Finds the outermost function symbol in the left-hand side of a rule
    outermostSymbolRule :: Rule Char Char -> [Char]
    outermostSymbolRule (Rule lhs _) = outermostSymbol lhs

    outermostSymbolRuleRight :: Rule Char Char -> [Char]
    outermostSymbolRuleRight (Rule _ rhs) = outermostSymbol rhs

    -- Returns list of all defined symbols in a set of rules
    definedSymbols :: [Rule Char Char] -> [(Char, Char)]
    definedSymbols rules = temp 1 (Prelude.map Data.List.head (nub (Prelude.map outermostSymbolRule rules)))
        where temp x [] = []
              temp x y = (Data.List.head y, Data.List.head (show x)) : temp (x + 1) (Data.List.tail y)


    -- This function takes the same list of rules twice as input and returns the list of dependency pairs
    dependencyPairs :: [Rule Char Char] -> [Rule Char Char] -> [Rule Char Char]
    dependencyPairs [] _ = []
    dependencyPairs ((Rule lhs rhs) : xs) rules = case drhs of
        [Nothing] -> dependencyPairs xs rules
        x ->  composeRules dlhs x Data.List.++ dependencyPairs xs rules
        where dSymbols = definedSymbols rules
              dlhs = changeSymbol lhs dSymbols
              drhs = changeSymbolRHS rhs dSymbols

    -- Takes a term as a lhs of a rule and a list of Maybe terms as input and constructs a list of rules out of the lhs and all rhs terms which are not Nothing in the list
    composeRules :: Term Char Char -> [Maybe (Term Char Char)] -> [Rule Char Char]
    composeRules x [] = []
    composeRules x (y:ys) = case y of
        Just z -> (Rule x z) : composeRules x ys
        Nothing -> composeRules x ys

    -- Takes a term and the list of defined functions and returns a term where the function symbol of the term is changed with the corresponding dependency symbol.
    -- This function is only used for lhs of a dependecy pair, so the function symbol of the term will always be inside the list of defined symbols
    changeSymbol :: Term Char Char -> [(Char, Char)] -> Term Char Char
    changeSymbol (Fun a b) ys = case lookup a ys of
        Just x -> Fun x b
        _ -> Fun a b

    -- This function takes a term (which represents a rhs of a dependency pair) and a list of defined symbols. It checks if the outermost function symbol matches one of the defined symbols.
    -- If it does, it gets changed with the corresponding dependency symbol, if it doesn't we do nothing. In both cases we continue with the sub functions.
    changeSymbolRHS ::  Term Char Char -> [(Char, Char)] -> [Maybe (Term Char Char)]
    changeSymbolRHS (Var _) ys = [Nothing]
    changeSymbolRHS (Fun c d) ys = case lookup c ys of
        Just x -> Just (Fun x d) : nub (Data.List.concat [changeSymbolRHS t ys | t <- d])
        Nothing -> nub (Data.List.concat [changeSymbolRHS t ys | t <- d])