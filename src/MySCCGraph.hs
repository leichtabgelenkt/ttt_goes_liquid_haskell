module MySCCGraph where
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
    import DependencyPairs
    import Rest
    import qualified Data.Set as Set
    import Control.Parallel.Strategies


    
    -- Given rules, this function returns a list of triple like (Numeration of node, outermost symbol of left side, outermost symbol of right side)
    sccPrepare :: [Rule Char Char] -> Int -> [(Int, String, String)]
    sccPrepare [] _ = []
    sccPrepare (x:xs) y = (y, outermostSymbolRule x, outermostSymbolRuleRight x) : sccPrepare xs (y+1)

    -- Compute vertices of dependency graph
    getEdges :: [(Int, String, String)] -> [(Int, String, String)] -> [(Int, Int)]
    getEdges [] _ = []
    getEdges (x:xs) y = help x y Data.List.++ getEdges xs y
        where help _ [] = []
              help s@(i, _, r) ((i2, l, _):xs) = if r == l then (i, i2) : help s xs else help s xs

    -- Returns the min-node and max-node of the vertices 
    getMinMax :: [(Int, String, String)] -> (Int, Int) -> (Int, Int)
    getMinMax [] (x,y) = (x,y)
    getMinMax ((i, _, _):xs) (x,y)
     | x == 0 && y == 0 = getMinMax xs (i,i)
     | i < x = getMinMax xs (i,y)
     | i > y = getMinMax xs (x,i)
     | otherwise = getMinMax xs (x,y)

     -- Returns the SCCs from a set of Dependency Rules 
    getSccFromDependencyPairs2 :: [Rule Char Char] -> [SCC Vertex]
    getSccFromDependencyPairs2 x = list
        where prepare = sccPrepare x 1
              edges = getEdges prepare prepare
              graphs = Data.List.map (buildG (getMinMax prepare (0,0))) (subLists edges)
              lists = Data.List.map (sccList) graphs
              list = Data.List.concat lists

    subLists :: [a] -> [[a]]
    subLists [] = [[]]  -- Base case: the sublist of an empty list is just the empty list
    subLists (x:xs) = let others = subLists xs  -- Get sublists of the tail
                    in others Data.List.++ Data.List.map (x:) others  -- Add the current element to each sublist

    notWantedEdges :: [(Int,Int)] -> [SCC Vertex] -> [(Int,Int)]
    notWantedEdges _ c = []
    notWantedEdges (x:xs) c = if checkAcyclic x c then notWantedEdges xs c else x : notWantedEdges xs c

    checkAcyclic :: (Int,Int) -> [SCC Vertex] -> Bool
    checkAcyclic (a,b) ((CyclicSCC x):xs) = checkAcyclic (a,b) xs
    checkAcyclic (a,b) ((AcyclicSCC x):xs) = if a == x || b == x then True else checkAcyclic (a,b) xs
    checkAcyclic _ [] = False

    extraCycles :: [SCC (Vertex, [Vertex])] -> [(Int, String, String)] -> [SCC Vertex]
    extraCycles [] _ = []
    extraCycles ((CyclicSCC x):xs) p = helpExtraCycles x p Data.List.++ extraCycles xs p
    extraCycles ((AcyclicSCC x):xs) p = extraCycles xs p

    helpExtraCycles :: [(Vertex, [Vertex])] -> [(Int, String, String)] -> [SCC Vertex]
    helpExtraCycles [] _ = []
    helpExtraCycles a p = lists
     where newEdges = generateSpecificPairs a
           graphs = Data.List.map (buildG (getMinMax p (0,0))) newEdges
           lists = Data.List.concat $ Data.List.map sccList graphs

    generateSpecificPairs :: [(a, [b])] -> [[(a, b)]]
    generateSpecificPairs input = go input []
     where go [] acc = [acc | not (Data.List.null acc)]
           go ((key, values):rest) acc =
            let pairs = [(key, value) | value <- values]
            in go rest (acc Data.List.++ pairs) Data.List.++ go rest acc

    -- Returns the SCCs from a set of Dependency Rules 
    getSccFromDependencyPairs :: [Rule Char Char] -> [SCC Vertex]
    getSccFromDependencyPairs x = Data.List.nub $ extraCycles extraList prepare
        where prepare = sccPrepare x 1
              edges = getEdges prepare prepare
              graph = buildG (getMinMax prepare (0,0)) edges
              extraList = sccListR graph


    -- extraCycles :: [SCC (Vertex, [Vertex])] -> [(Int, String, String)] -> [SCC Vertex]
    -- extraCycles sccs p = Data.List.concat $ parMap rdeepseq processSCC sccs
    --   where
    --     processSCC (CyclicSCC x) = helpExtraCycles x p
    --     processSCC (AcyclicSCC _) = []
    
    -- helpExtraCycles :: [(Vertex, [Vertex])] -> [(Int, String, String)] -> [SCC Vertex]
    -- helpExtraCycles a p = Data.List.concat $ parMap rdeepseq (sccList . buildGraph) (generateSpecificPairs a)
    --   where
    --     buildGraph = buildG (getMinMax p (0, 0))
    
    -- -- Optimized version of generateSpecificPairs to avoid redundant concatenations
    -- generateSpecificPairs :: [(a, [b])] -> [[(a, b)]]
    -- generateSpecificPairs = Data.List.foldr go [[]]
    --   where
    --     go (key, values) acc = [[(key, value) | value <- values] Data.List.++ sublist | sublist <- acc] Data.List.++ acc
    
    -- -- Returns the SCCs from a set of Dependency Rules
    -- getSccFromDependencyPairs :: [Rule Char Char] -> [SCC Vertex]
    -- getSccFromDependencyPairs rules = extraCycles extraList prepare
    --   where
    --     prepare = sccPrepare rules 1
    --     edges = getEdges prepare prepare
    --     graph = buildG (getMinMax prepare (0, 0)) edges
    --     extraList = sccListR graph

    findRule :: [Rule Char Char] -> (Int, String, String) -> Rule Char Char
    findRule s p@(_, a, b) = if (outermostSymbolRule (Data.List.head s) == a) && (outermostSymbolRuleRight (Data.List.head s) == b) then (Data.List.head s) else findRule (Data.List.tail s) p

    findSccNode :: [Rule Char Char] -> [(Int, String, String)] -> [Int] -> [Rule Char Char]
    findSccNode _ _ [] = []
    findSccNode rules prepare (y:ys) = findRule rules (getIndex prepare y) : findSccNode (delete (findRule rules (getIndex prepare y)) rules) prepare ys
        where getIndex ((i,a,b):zs) y = if i == y then (i,a,b) else getIndex zs y

    instance (Eq f, Eq v, Eq v') => Eq (Reduct f v v') where
        (Reduct result1 _ _ _) == (Reduct result2 _ _ _) = result1 == result2

    createNewTerm :: Char -> Term Char Char -> Term Char Char
    createNewTerm a (Fun b c) = Fun a c

    findStartingNodes :: [Rule Char Char] -> [Int] -> [(Int, String, String)] -> Term Char Char -> [(Char, Char)] -> [Int]
    findStartingNodes _ [] _ _ _ = []
    findStartingNodes rules (y:ys) preparation term numbers = if (fullRewrite (alterDependencyRules (findSccNode rules preparation [y]) numbers) term) /= [] then y : findStartingNodes (delete (Data.List.head $ findSccNode rules preparation [y]) rules) ys preparation term numbers else findStartingNodes (delete (Data.List.head $ findSccNode rules preparation [y]) rules) ys preparation term numbers
    

    alterDependencyRules :: [Rule Char Char] -> [(Char, Char)] -> [Rule Char Char]
    alterDependencyRules [] _ = []
    alterDependencyRules (Rule lhs rhs : xs) p = (Rule (alterTerm lhs p) rhs) : alterDependencyRules xs p

    alterTerm :: Term Char Char -> [(Char, Char)] -> Term Char Char
    alterTerm (Var a) _ = Var a
    alterTerm (Fun x y) ((c,d) : zs) = if x == d then (Fun c y) else alterTerm (Fun x y) zs
        
    -- Arguments: The normal rewrite rules, the term of which you want to now the reach in the dependency graph  
    reachableNodesFromTerm :: [Rule Char Char] -> Term Char Char -> [Int]
    reachableNodesFromTerm s m@(Fun t o) = nub (reachableHelpSearch startingNodes startingNodes edges edges)
     where
        numbers = definedSymbols s --Gets defined symbols
        k = findNumber t numbers
        findNumber e ((r, p):ts) = if r == e then p else findNumber e ts
        findNumber e [] = '0'
        newTerm = createNewTerm k m
        dependencyRules = dependencyPairs s s
        preparation = sccPrepare dependencyRules 1
        edges = getEdges preparation preparation
        startingNodes = findStartingNodes dependencyRules [1..Data.List.length dependencyRules] preparation m numbers

    --[Rule {lhs = Fun '2' [Var 'x',Fun 'u' [Var 'y',Var 'z']], rhs = Fun '2' [Var 'x',Var 'y']}]
    -- [Fun '2' [Fun 'u' [Fun 'a' [],Fun 'a' []],Fun 'a' []]]

    reachableNodesFromTerm2 :: [Rule Char Char] -> Term Char Char -> [(Int, String, String)]
    reachableNodesFromTerm2 s m@(Fun t o) = preparation--(findSccNode dependencyRules preparation [8])
     where
        numbers = definedSymbols s --Gets defined symbols
        k = findNumber t numbers
        findNumber e ((r, p):ts) = if r == e then p else findNumber e ts
        findNumber e [] = '0'
        newTerm = createNewTerm k m
        dependencyRules = dependencyPairs s s
        preparation = sccPrepare dependencyRules 1
        edges = getEdges preparation preparation
        startingNodes = findStartingNodes dependencyRules [1..Data.List.length dependencyRules] preparation newTerm

    reachableHelpSearch :: [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
    reachableHelpSearch [] z _ _ = z
    reachableHelpSearch (x:xs) z v [] = reachableHelpSearch xs z v v
    reachableHelpSearch (x:xs) z v ((a,b) : ys) = if (x == a) && (not $ b `Data.List.elem` z) then (reachableHelpSearch (z Data.List.++ [b]) (z Data.List.++ [b]) v v) else (reachableHelpSearch (x:xs) z v ys)
    

    -- Evtl. ineffizient
    reachableAndInSCC :: [Int] -> [Int] -> [SCC Vertex] -> [SCC Vertex] -> [[Int]]
    reachableAndInSCC _ _ _ [] = []
    reachableAndInSCC (x:xs) z v (CyclicSCC a : ys) = if x `Data.List.elem` a then a : reachableAndInSCC xs z v (CyclicSCC a : ys) else reachableAndInSCC xs z v (CyclicSCC a : ys)
    reachableAndInSCC [] z v b = reachableAndInSCC z z v (Data.List.tail b)
    reachableAndInSCC b z v (AcyclicSCC a : ys) = reachableAndInSCC b z v ys

    sccsToRules :: [SCC Vertex] -> [[Int]]
    sccsToRules [] = [[]]
    sccsToRules (CyclicSCC x : xs) = x : sccsToRules xs
    sccsToRules (AcyclicSCC _ : xs) = sccsToRules xs