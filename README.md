# ttt_goes_liquid_haskell

Tyrolean-Termitation-Tool goes liquid haskell

## Start the program
To run the program on a Linux system:
```
$> stack run subterm-criterion "./path/to/example.trs" "starting-term"
```
Some example TRSs are already created and one can find them in the examples folder.
## For using the SMT-Solver

[Z3](https://github.com/Z3Prover/z3/blob/master/README.md) needs to be installed.


## Modules
- **Main.hs:** Contains the main function, which gets executed when starting the program.
- **DependencyPairs.hs:** Contains all the functions, which are used to compute the dependency pairs.
- **MySCCGraph.hs:** Contains all the functions, which are used to to compute the Dependency Graph and the corresponding SCCs.
- **Multiplicity.hs:** Contains all the functions for the Multiplicity function. The Multiplicity function then gets used by the subterm criterion.
- **SubtermCriterion.hs:** Contains all the functions for the subterm criterion and it also contains all the SMT-Solver computations.
- **Rest.hs:** Contains the reimplemented core algorithm of the REST tool.
- **Test.hs:** Contains tests, which we used to check the results of the Multiplicity function.
- **OldTests.hs:** Contains old tests for the rest function and subterm criterion. These test are outdated.
- **TTT3TestSets.hs:** Contains the old tests of our tool, before it was possible to use it as a command line tool. Some of the tests don't represent the results we get now with the current version (The current results are in /examples/term-results).
- **Obsolete.hs:** Contains functions, which we thought we needed to use the subterm criterion iteratively. After fixing some bugs, we realised we don't need them any more.
- **Rules.hs:** Contains some terms, rewrite rules and TRS, which were used for testing.


## REST

This is our Implementation of the Rest-Algorithm from the paper:
```
REST: Integrating Term Rewriting with Program Verification
```

## Authors

Philipp Dablander <Philipp.Dablander@student.uibk.ac.at> \
Luca Maahs <Luca.Maahs@student.uibk.ac.at>