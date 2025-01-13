module Rest where
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
    import Data.SBV (constrain, sInteger, allSat)
    import Data.Bool (Bool(True, False))
    import Data.Char

    newtype RewriteSequence = RewriteSequence [(Term Char Char, Term Char Char)]
    instance Show RewriteSequence where
        show (RewriteSequence lst) = "RewriteSequence [" Data.List.++ show lst Data.List.++ "]"


    -- Splits a String
    splitString :: String -> [String]
    splitString []     = []
    splitString (x:xs) = [x] : splitString xs

    -- Gets all function symbols in a set of rules
    collectFunctionsInRuleList :: [Rule Char Char] -> [String]
    collectFunctionsInRuleList rules = nub (splitString (Data.List.concatMap (\rule -> TermOps.funs (lhs rule) Data.List.++ TermOps.funs (rhs rule)) rules))

    -- Gets all possible hirachies of function symbols in a set of rules
    getAllFamilies :: [Rule Char Char] -> [[String]]
    getAllFamilies rules = permutations (collectFunctionsInRuleList rules)

    -- Finds the outermost function symbol in a term 
    outermostSymbol :: Term Char Char -> [Char]
    outermostSymbol (Var _) = error "Variable has no outermost symbol"
    -- outermostSymbol (Var v) = [v]
    outermostSymbol (Fun f _) = [f]

    -- Returns a list of the combined outermost symbols of two terms
    compareOutermostSymbols :: Term Char Char -> Term Char Char -> [String]
    compareOutermostSymbols term1 term2 = splitString (outermostSymbol term1 Data.List.++ outermostSymbol term2)

    isBefore :: Eq a => [a] -> a -> a -> Bool
    isBefore ordering a b = 
        case (a `elemIndex` ordering, b `elemIndex` ordering) of
            (Just indexA, Just indexB) -> indexA < indexB
            _ -> False

    -- Removes all orderings of a family where the ordering is not the same as in the first argument, which is a sub family
    findOrderings :: [String] -> [[String]] -> [[String]]
    findOrderings _ [] = []
    findOrderings [x, y] allOrderings = 
        Data.List.filter (\ordering -> isBefore ordering x y) allOrderings
            

    -- Makes a sub function order out of two terms and then refines the family of orderings 
    refine :: [[String]] -> Term Char Char -> [Term Char Char] -> [[String]]
    refine rules t1 [] = []
    refine rules t1 t2 = findOrderings (compareOutermostSymbols t1 (Data.List.head t2)) rules

    lpoAlternativeOne :: Term Char Char -> Term Char Char -> Int -> Int -> [String] -> Bool
    lpoAlternativeOne f@(Fun x a) g@(Fun y b) step index family
     | step /= 3 && index == (Data.List.length b) = False
     | step == 3 && index == (Data.List.length b) = True
     | step == 1 && index == 0 = if (a Data.List.!! 0) == (b Data.List.!! 0) then (lpoAlternativeOne f g 1 1 family) else False
     | step == 1 = if (a Data.List.!! index) == (b Data.List.!! index) then (lpoAlternativeOne f g 1 (index+1) family) else (lpoAlternativeOne f g 2 index family)
     | step == 2 = if lpoFamily (a Data.List.!! index) (b Data.List.!! index) family then (lpoAlternativeOne f g 3 (index+1) family) else False
     | step == 3 = if lpoFamily f (b Data.List.!! index) family then (lpoAlternativeOne f g 3 (index+1) family) else False
     | otherwise = False

    lpoAlternativeTwo :: Term Char Char -> Term Char Char -> [String] -> Bool
    lpoAlternativeTwo f@(Fun x a) g@(Fun y b) family
     | x /= y = False
     | otherwise = and (Data.List.map (\u -> lpoFamily f u family) b)

    
    lpoAlternativeThree :: Term Char Char -> Term Char Char -> [String] -> Bool
    lpoAlternativeThree f@(Fun x a) g@(Fun y b) family = or (Data.List.map (\u -> (u == g) || (lpoFamily u g family)) a)

    lpoFamily :: Term Char Char -> Term Char Char -> [String] -> Bool
    lpoFamily f@(Fun x a) g@(Fun y b) family
     | x == y = lpoAlternativeOne f g 1 0 family
     | x /= y && ((findOrderings (compareOutermostSymbols f g) [family]) /= []) && (b == []) = True
     | x /= y && ((findOrderings (compareOutermostSymbols f g) [family]) /= []) = lpoAlternativeTwo f g family
     -- | (a /= []) = lpoAlternativeThree f g family
     | otherwise = False
    --lpoFamily _ _ _ = False

    -- Just checks if a list is empty or not
    mySat :: [[a]] -> Bool
    mySat [] = False
    mySat _ = True

    -- Start of the REST algorithm
    rest :: [Rule Char Char] -> Term Char Char -> RewriteSequence
    rest rules term = p ([term], (getAllFamilies rules)) rules

    -- Adds the Data.List.head element to the result and then hands the work down
    p :: ([Term Char Char], [[String]]) -> [Rule Char Char] -> RewriteSequence
    p ([], _) _ = RewriteSequence []  -- Handle the case of an empty list
    p (terms, families) rules = RewriteSequence (help2 (Data.List.head terms) families rules rules)


    -- Checks if a rules is applicable on a certain term. If so, then it returns the reduct, otherwise it returns an empty list
    getNewReduct :: Term Char Char -> Rule Char Char -> [Term Char Char]
    getNewReduct term rule =
        case fullRewrite [rule] term of
            [] -> [] -- Rule doesn't apply, return Nothing
            (reduct:_) -> [result reduct]

    -- Here is where most of the work for the REST algorithm happens. This function takes a term, a family of function orderings, the whole list of rewrite rules and the list of rewrite rules which have not been tested yet.
    -- If all rules have been tested then it returns an empty list. If the next rules isn't applicable on the term, then a recursive call happens with the Data.List.tail of the rule list. If the rule is applicable, but the arising subordering
    -- of functions isn't mySatisfiable for the given family, then the result will be ignored and process will be continued with the next rule. However, if everything works out, then the reduct gets into the result list and we get two new
    -- function calls: The first one on the old term with the Data.List.tail of the rules and the second one on the new term with all rules
    help2 :: Term Char Char -> [[String]] -> [Rule Char Char] ->  [Rule Char Char] -> [(Term Char Char, Term Char Char)]
    help2 term families rules rSet
     | rSet == [] = []
     | Data.List.null newReduct = help2 term families rules (Data.List.tail rSet)
     | not (mySat newFamilies) = help2 term families rules (Data.List.tail rSet)
     | otherwise = [(term, Data.List.head newReduct)] Data.List.++ (help2 term newFamilies rules (Data.List.tail rSet)) Data.List.++ (help2 (Data.List.head newReduct) newFamilies rules rules)
     where lhs = getLHS (Data.List.head rSet)
           rhs = getRHS (Data.List.head rSet)
           newReduct = getNewReduct term (Data.List.head rSet)
           newFamilies = Data.List.filter (\x -> lpoFamily lhs rhs x) families

    getLHS :: Rule Char Char -> Term Char Char
    getLHS (Rule lhs _) = lhs

    getRHS :: Rule Char Char -> Term Char Char
    getRHS (Rule _ rhs) = rhs




    -- Start of the REST algorithm
    restTermination :: [Rule Char Char] -> Term Char Char -> Bool
    restTermination rules term = pTermination ([term], (getAllFamilies rules)) rules

    -- Adds the Data.List.head element to the result and then hands the work down
    pTermination :: ([Term Char Char], [[String]]) -> [Rule Char Char] -> Bool
    pTermination ([], _) _ = True  -- Handle the case of an empty list
    pTermination (terms, families) rules = and (help2Termination (Data.List.head terms) families rules rules)

    help2Termination :: Term Char Char -> [[String]] -> [Rule Char Char] ->  [Rule Char Char] -> [Bool]
    help2Termination term families rules rSet
     | rSet == [] = []
     | Data.List.null newReduct = help2Termination term families rules (Data.List.tail rSet)
     | not (mySat newFamilies) = [False] --help2 term families rules (Data.List.tail rSet)
     | otherwise = (help2Termination term newFamilies rules (Data.List.tail rSet)) Data.List.++ (help2Termination (Data.List.head newReduct) newFamilies rules rules)
     where lhs = getLHS (Data.List.head rSet)
           rhs = getRHS (Data.List.head rSet)
           newReduct = getNewReduct term (Data.List.head rSet)
           newFamilies = Data.List.filter (\x -> lpoFamily lhs rhs x) families --refine families lhs [rhs]