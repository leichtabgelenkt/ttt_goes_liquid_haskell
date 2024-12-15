{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
import Data.Rewriting.Term as ParseTerm
import Data.Rewriting.Term.Parse
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
import qualified Data.Set               as S
import Data.Function (on, const)
import Data.Graph
import Data.Graph.SCC
import Data.SBV
import Data.SBV.List
import Data.SBV.Internals
import Data.SBV.Trans (getModelValue)
import Data.Maybe (fromMaybe)
import GHC.Exts (fromList)
import Data.SBV (constrain, sInteger, allSat)
import Data.Bool (Bool(True, False))
import Data.Char
import Rules
import Rest
import Multiplicity
import OldTests
import SubtermCriterion
import DependencyPairs
import MySCCGraph
import Control.Monad.Trans.RWS.Lazy (get)
import Control.Monad.IO.Class (MonadIO(liftIO))
import TTT3TestSets
import Data.Rewriting.Problem.Type
import System.Environment (getArgs)
import System.FilePath (takeExtension)
import System.Clock
import Control.Exception (evaluate)
import Text.Printf (printf)

instance (Show f, Show v, Show v') => Show (Reduct f v v') where
  show (Reduct result pos rule subst) =
    "Reduct { result = " Data.List.++ show result
    Data.List.++ ", pos = " Data.List.++ show pos
    Data.List.++ ", rule = " Data.List.++ show rule
    Data.List.++ ", subst = " Data.List.++ show subst
    Data.List.++ " }"


getStrictRules :: Problem f v -> [Rule f v]
getStrictRules problem = strictRules (rules problem)

getVariables :: Problem f v -> [v]
getVariables problem = variables problem

getCharTerm ::  Term String String -> Term Char Char
getCharTerm (Fun a b) = Fun (Data.List.head a) [getCharTerm subterm | subterm <- b]
getCharTerm (Var x) = Var (Data.List.head x)

getCharRule :: Rule String String -> Rule Char Char
getCharRule (Rule lhs rhs) = Rule (getCharTerm lhs) (getCharTerm rhs)

containsVariable :: Term Char Char -> Bool
containsVariable (Fun _ b) = or (False : [containsVariable subterm | subterm <- b])
containsVariable _ = True

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, term] -> runMain filePath term
        _ -> putStrLn "Usage: subterm-criterion <path to your .trs file> \"<start-term>\""

runMain :: FilePath -> String -> IO ()
runMain filePath term = do
    result <- fromFile filePath
    case result of
        Left err -> putStrLn $ "Error: " Data.List.++ show(err)
        Right trs -> do 
          let variables = getVariables trs
          stringTerm <- ParseTerm.parseIO variables term
          let term = getCharTerm $ stringTerm
          if containsVariable term 
            then putStrLn $ "Please enter a ground term!"
            else do
              start <- getTime Monotonic
              result <- ttt3 (Data.List.map getCharRule $ getStrictRules trs) term
              end <- getTime Monotonic
              putStrLn result
              let diff = diffTimeSpec end start
              let seconds = toSeconds diff
              putStrLn $ "Execution time: " Data.List.++ printf "%.4f" seconds Data.List.++ " seconds"

toSeconds :: System.Clock.TimeSpec -> Double
toSeconds (System.Clock.TimeSpec s ns) = fromIntegral s + fromIntegral ns / 1000000000