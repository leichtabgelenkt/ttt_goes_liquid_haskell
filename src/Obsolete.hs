{-
These functions where written to use the subterm criterion iteratively, because we thought it wouldn't work with the implementation we had. However, it
does work, we just had a bug. As soon as we fixed the bug all these functions became obsolete. 
-}


iterativeMethod :: [Rule Char Char] -> Projection -> [Rule Char Char] -> IO Bool
iterativeMethod dependencyRules projection rules = do
  result <- getIntermediateResult dependencyRules projection rules
  resultBool <- checkTest2 result
  if resultBool
    then do
      workingRules <- extractValues2 (result) []
      let reducedRules = rules \\ (throwOutRules rules workingRules 0)
      if reducedRules == []
        then do
          return True
        else do
          if reducedRules == rules
            then do
              return False
            else do
              iterativeMethod dependencyRules projection reducedRules
    else do
      return False

throwOutRules :: [Rule Char Char] -> [Integer] -> Integer -> [Rule Char Char]
throwOutRules [] _ _ = []
throwOutRules (x:xs) ys a = if a `Data.List.elem` ys then throwOutRules xs ys (a+1) else x : throwOutRules xs ys (a+1)

getIntermediateResult :: [Rule Char Char] -> Projection -> [Rule Char Char] -> IO OptimizeResult
getIntermediateResult dependencyRules projection rules = optimize Lexicographic $ do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sInteger "d"
  e <- sInteger "e"
  f <- sInteger "f"
  g <- sInteger "g"
  h <- sInteger "h"
  i <- sInteger "i"
  aa <- sInteger "aa"
  bb <- sInteger "bb"
  cc <- sInteger "cc"
  dd <- sInteger "dd"
  ee <- sInteger "ee"
  ff <- sInteger "ff"
  gg <- sInteger "gg"
  hh <- sInteger "hh"
  ii <- sInteger "ii"
  jj <- sInteger "jj"
  kk <- sInteger "kk"
  ll <- sInteger "ll"
  mm <- sInteger "mm"
  nn <- sInteger "nn"
  oo <- sInteger "oo"
  pp <- sInteger "pp"
  qq <- sInteger "qq"
  rr <- sInteger "rr"
  let
    constrainList :: [SInteger]
    constrainList = [a,b,c,d,e,f,g,h,i]
  let
    newProjection :: Projection
    newProjection = putValuesIntoProjection constrainList projection
  let ruleConstrainList = [aa,bb,cc,dd,ee,ff,gg,hh,ii, jj, kk, ll, mm, nn, oo, pp, qq, rr]

  let ruleConstraints = putContraintsWithRules rules ruleConstrainList
  let sumVars = countOnes ruleConstraints newProjection 0
  constrain $ sumVars .> 0
  maximize "Sum of aa to ii" sumVars
  constrain $ geqRules rules newProjection
  constrain $ neqRulesS ruleConstraints newProjection []
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .<= (getArityOfSymbol (getSymbol newProjection 0) dependencyRules))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .<= (getArityOfSymbol (getSymbol newProjection 1) dependencyRules))
  constrain $ c .== (literal (-1)) .|| (c .> (literal 0) .&& c .<= (getArityOfSymbol (getSymbol newProjection 2) dependencyRules))
  constrain $ d .== (literal (-1)) .|| (d .> (literal 0) .&& d .<= (getArityOfSymbol (getSymbol newProjection 3) dependencyRules))
  constrain $ e .== (literal (-1)) .|| (e .> (literal 0) .&& e .<= (getArityOfSymbol (getSymbol newProjection 4) dependencyRules))
  constrain $ f .== (literal (-1)) .|| (f .> (literal 0) .&& f .<= (getArityOfSymbol (getSymbol newProjection 5) dependencyRules))
  constrain $ g .== (literal (-1)) .|| (g .> (literal 0) .&& g .<= (getArityOfSymbol (getSymbol newProjection 6) dependencyRules))
  constrain $ h .== (literal (-1)) .|| (h .> (literal 0) .&& h .<= (getArityOfSymbol (getSymbol newProjection 7) dependencyRules))
  constrain $ i .== (literal (-1)) .|| (i .> (literal 0) .&& i .<= (getArityOfSymbol (getSymbol newProjection 8) dependencyRules))
  constrain $ aa .>= (literal 0) .&& aa .<= (literal 1)
  constrain $ bb .>= (literal 0) .&& bb .<= (literal 1)
  constrain $ cc .>= (literal 0) .&& cc .<= (literal 1)
  constrain $ dd .>= (literal 0) .&& dd .<= (literal 1)
  constrain $ ee .>= (literal 0) .&& ee .<= (literal 1)
  constrain $ ff .>= (literal 0) .&& ff .<= (literal 1)
  constrain $ gg .>= (literal 0) .&& gg .<= (literal 1)
  constrain $ hh .>= (literal 0) .&& hh .<= (literal 1)
  constrain $ ii .>= (literal 0) .&& ii .<= (literal 1)
  
range = ['1','2','3','4','5','6','7','8','9']

neqRulesIterative :: [Rule Char Char] -> Projection -> SBool
neqRulesIterative [] _ = sTrue
neqRulesIterative rules p = sAnd [neq (getLHS rule) (getRHS rule) p | rule <- rules]

countOnes :: [(Rule Char Char, SInteger)] -> Projection -> SInteger -> SInteger
countOnes [] p r = r
countOnes ((rule, x):xs) p r = ite (x .== 0) (countOnes xs p r) (countOnes xs p (r + 1))


getIntermediateResultSat :: [Rule Char Char] -> Projection -> [Rule Char Char] -> IO SatResult
getIntermediateResultSat dependencyRules projection rules = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sInteger "d"
  e <- sInteger "e"
  f <- sInteger "f"
  g <- sInteger "g"
  h <- sInteger "h"
  i <- sInteger "i"
  aa <- sInteger "aa"
  bb <- sInteger "bb"
  cc <- sInteger "cc"
  dd <- sInteger "dd"
  ee <- sInteger "ee"
  ff <- sInteger "ff"
  gg <- sInteger "gg"
  hh <- sInteger "hh"
  ii <- sInteger "ii"
  let
    constrainList :: [SInteger]
    constrainList = [a,b,c,d,e,f,g,h,i]
  let
    newProjection :: Projection
    newProjection = putValuesIntoProjection constrainList projection
  let ruleConstrainList = [aa,bb,cc,dd,ee,ff,gg,hh,ii]
  let ruleConstraints = putContraintsWithRules rules ruleConstrainList
  constrain $ geqRulesS ruleConstraints newProjection []
  constrain $ neqRulesS ruleConstraints newProjection []
  constrain $ (neqRulesSLength ruleConstraints) .> (0 :: SInteger)
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .<= (getArityOfSymbol '1' dependencyRules))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .<= (getArityOfSymbol '2' dependencyRules))
  constrain $ c .== (literal (-1)) .|| (c .> (literal 0) .&& c .<= (getArityOfSymbol '3' dependencyRules))
  constrain $ d .== (literal (-1)) .|| (d .> (literal 0) .&& d .<= (getArityOfSymbol '4' dependencyRules))
  constrain $ e .== (literal (-1)) .|| (e .> (literal 0) .&& e .<= (getArityOfSymbol '5' dependencyRules))
  constrain $ f .== (literal (-1)) .|| (f .> (literal 0) .&& f .<= (getArityOfSymbol '6' dependencyRules))
  constrain $ g .== (literal (-1)) .|| (g .> (literal 0) .&& g .<= (getArityOfSymbol '7' dependencyRules))
  constrain $ h .== (literal (-1)) .|| (h .> (literal 0) .&& h .<= (getArityOfSymbol '8' dependencyRules))
  constrain $ i .== (literal (-1)) .|| (i .> (literal 0) .&& i .<= (getArityOfSymbol '9' dependencyRules))
  constrain $ aa .>= (literal 0) .&& aa .<= (literal 1)
  constrain $ bb .>= (literal 0) .&& bb .<= (literal 1)
  constrain $ cc .>= (literal 0) .&& cc .<= (literal 1)
  constrain $ dd .>= (literal 0) .&& dd .<= (literal 1)
  constrain $ ee .>= (literal 0) .&& ee .<= (literal 1)
  constrain $ ff .>= (literal 0) .&& ff .<= (literal 1)
  constrain $ gg .>= (literal 0) .&& gg .<= (literal 1)
  constrain $ hh .>= (literal 0) .&& hh .<= (literal 1)
  constrain $ ii .>= (literal 0) .&& ii .<= (literal 1)
  
putContraintsWithRules :: [Rule Char Char] -> [SInteger] -> [(Rule Char Char, SInteger)]
putContraintsWithRules [] _ = []
putContraintsWithRules (x:xs) (y:ys) = (x,y) : putContraintsWithRules xs ys

neqRulesS :: [(Rule Char Char, SInteger)] -> Projection -> [Rule Char Char] -> SBool
neqRulesS [] p r = neqRulesIterative r p 
neqRulesS ((rule, x):xs) p r = ite (x .== 0) (neqRulesS xs p r) (neqRulesS xs p ([rule] Data.List.++ r))

geqRulesS :: [(Rule Char Char, SInteger)] -> Projection -> [Rule Char Char] -> SBool
geqRulesS [] p r = geqRules r p 
geqRulesS ((rule, x):xs) p r = ite (x .== 0) (geqRulesS xs p r) (geqRulesS xs p ([rule] Data.List.++ r))

neqRulesSLength :: [(Rule Char Char, SInteger)] -> SInteger
neqRulesSLength [] = 0
neqRulesSLength ((_, x):xs) = ite (x .== 0) (neqRulesSLength xs) (1 + neqRulesSLength xs)

extractValues :: SatResult -> [String] -> IO [Integer]
extractValues a _ = do
  let singleLines = Data.List.lines $ show a
  let doubleCharacterLines = Data.List.filter (\ x -> (Data.List.length x > 19) && ((x Data.List.!!) 3 /= ' ')) singleLines
  let indexesOfRulesToRedo = filterIndexRules doubleCharacterLines 0
  return $ indexesOfRulesToRedo

extractValues2 :: OptimizeResult -> [String] -> IO [Integer]
extractValues2 a _ = do
  let singleLines = Data.List.lines $ show a
  let doubleCharacterLines = Data.List.filter (\ x -> (Data.List.length x > 19) && ((x Data.List.!!) 3 /= ' ')) singleLines
  let indexesOfRulesToRedo = filterIndexRules doubleCharacterLines 0
  return $ indexesOfRulesToRedo


filterIndexRules :: [String] -> Integer -> [Integer]
filterIndexRules [] _ = []
filterIndexRules (x:xs) a = if "0" `Data.List.isInfixOf` x then a : filterIndexRules xs (a+1) else filterIndexRules xs (a+1)

getSatResult :: [Rule Char Char] -> Projection -> [Rule Char Char] -> IO SatResult
getSatResult dependencyRules projection rules = sat $ do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sInteger "d"
  e <- sInteger "e"
  f <- sInteger "f"
  g <- sInteger "g"
  h <- sInteger "h"
  i <- sInteger "i"
  let
    constrainList :: [SInteger]
    constrainList = [a,b,c,d,e,f,g,h,i]
  let
    newProjection :: Projection
    newProjection = putValuesIntoProjection constrainList projection
  constrain $ geqRules rules newProjection
  constrain $ neqRules rules newProjection
  constrain $ a .== (literal (-1)) .|| (a .> (literal 0) .&& a .<= (getArityOfSymbol (getSymbol newProjection 0) dependencyRules))
  constrain $ b .== (literal (-1)) .|| (b .> (literal 0) .&& b .<= (getArityOfSymbol (getSymbol newProjection 1) dependencyRules))
  constrain $ c .== (literal (-1)) .|| (c .> (literal 0) .&& c .<= (getArityOfSymbol (getSymbol newProjection 2) dependencyRules))
  constrain $ d .== (literal (-1)) .|| (d .> (literal 0) .&& d .<= (getArityOfSymbol (getSymbol newProjection 3) dependencyRules))
  constrain $ e .== (literal (-1)) .|| (e .> (literal 0) .&& e .<= (getArityOfSymbol (getSymbol newProjection 4) dependencyRules))
  constrain $ f .== (literal (-1)) .|| (f .> (literal 0) .&& f .<= (getArityOfSymbol (getSymbol newProjection 5) dependencyRules))
  constrain $ g .== (literal (-1)) .|| (g .> (literal 0) .&& g .<= (getArityOfSymbol (getSymbol newProjection 6) dependencyRules))
  constrain $ h .== (literal (-1)) .|| (h .> (literal 0) .&& h .<= (getArityOfSymbol (getSymbol newProjection 7) dependencyRules))
  constrain $ i .== (literal (-1)) .|| (i .> (literal 0) .&& i .<= (getArityOfSymbol (getSymbol newProjection 8) dependencyRules))

checkTest2 :: OptimizeResult -> IO Bool
checkTest2 result = do
  r2 <- bits2
  if (show result) == (show r2) then return False else return True




-------------------- following functions never got used ---------------------------------------------

instance Show RewriteSequence where
  show s@(RewriteSequence a) = show $ Prelude.map listToStringWithArrows $ nub $ removeSublists $ removeEmptySublists $ Prelude.map nub $ showRewriteSequence s (fst (Data.List.head a))

listToStringWithArrows :: [Term Char Char] -> String
listToStringWithArrows [] = ""
listToStringWithArrows [x] = show x
listToStringWithArrows (x:xs) = show x Data.List.++ " -> " Data.List.++ listToStringWithArrows xs

newtype PrintRule = PrintRule (Rule Char Char)

instance Show PrintRule where
  show (PrintRule (Rule lhs rhs)) = show lhs Data.List.++ " -> " Data.List.++ show rhs Data.List.++ "\n"

showRewriteSequence :: RewriteSequence -> Term Char Char -> [[Term Char Char]]
showRewriteSequence (RewriteSequence []) x = [[]]
showRewriteSequence (RewriteSequence ((start, end):xs)) x = if start == x then [start : end : t | t <- splitAndRemove $ getL end xs] Data.List.++ showRewriteSequence (RewriteSequence xs) x else showRewriteSequence (RewriteSequence xs) x -- Data.List.++ showRewriteSequence (RewriteSequence xs)
--[([start, end] Data.List.++ getL end xs)]

splitAndRemove :: [Term Char Char] -> [[Term Char Char]]
splitAndRemove [] = []
splitAndRemove xs = Data.List.filter (not . Data.List.null) $ case break (== Fun '$' []) xs of
  (before, []) -> [before]
  (before, _ : after) -> before : splitAndRemove after

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist sublist list = sublist `Data.List.elem` Data.List.tail (Data.List.init $ subsequences list)

removeEmptySublists :: [[Term a b]] -> [[Term a b]]
removeEmptySublists = Data.List.filter (not . Data.List.null)

removeSublists :: Eq a => [[a]] -> [[a]]
removeSublists lists = Data.List.filter (\x -> not (Data.List.any (`isSublist` x) (delete x lists))) lists

mysequence = RewriteSequence [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]

startElement = fst $ Data.List.head [(Fun 'n' [Fun 'u' [Fun 's' [],Fun 't' []],Fun 's' []],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []]),(Fun 'u' [Fun 'n' [Fun 's' [],Fun 's' []],Fun 'e' []],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' []),(Fun 'u' [Fun 's' [],Fun 'n' [Fun 't' [],Fun 's' []]],Fun 'u' [Fun 's' [],Fun 'e' []]),(Fun 'u' [Fun 's' [],Fun 'e' []],Fun 's' [])]
