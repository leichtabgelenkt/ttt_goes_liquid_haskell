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
    import GHC.Exts (fromList)
    import GHC.Cmm (CmmNode(res))
    import Data.SBV (constrain, sInteger, allSat)
    import GHC.Prelude (Show(show))
    import Data.Bool (Bool(True, False))
    import Data.Char
    import DependencyPairs
    
    -- Given rules, this function returns a list of triple like (Numeration of node, outermost symbol of left side, outermost symbol of right side)
    sccPrepare :: [Rule Char Char] -> Int -> [(Int, String, String)]
    sccPrepare [] _ = []
    sccPrepare (x:xs) y = (y, outermostSymbolRule x, outermostSymbolRuleRight x) : sccPrepare xs (y+1)

    -- Compute vertices of dependency graph
    getVertices :: [(Int, String, String)] -> [(Int, String, String)] -> [(Int, Int)]
    getVertices [] _ = []
    getVertices (x:xs) y = help x y Data.List.++ getVertices xs y
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
    getSccFromDependencyPairs :: [Rule Char Char] -> [SCC Vertex]
    getSccFromDependencyPairs x = sccList graph
        where prepare = sccPrepare x 1
              vertices = getVertices prepare prepare
              graph = buildG (getMinMax prepare (0,0)) vertices

    findRule :: [Rule Char Char] -> (Int, String, String) -> Rule Char Char
    findRule s p@(_, a, b) = if (outermostSymbolRule (Data.List.head s) == a) && (outermostSymbolRuleRight (Data.List.head s) == b) then (Data.List.head s) else findRule (Data.List.tail s) p

    findSccNode :: [Rule Char Char] -> [(Int, String, String)] -> [Int] -> [Rule Char Char]
    findSccNode _ _ [] = []
    findSccNode rules prepare (y:ys) = findRule rules (getIndex prepare y) : findSccNode (delete (findRule rules (getIndex prepare y)) rules) prepare ys
        where getIndex ((i,a,b):zs) y = if i == y then (i,a,b) else getIndex zs y