# SAT Solver

The purpose of this assignment is to implement an elementary SAT-solver using the SAT solving method called the [Davis-Putnam-Logemann-
Loveland(DPLL)](https://en.wikipedia.org/wiki/DPLL_algorithm) procedure and use it to solve problems like [n-queens](https://en.wikipedia.org/wiki/Eight_queens_puzzle) and [Sudoku](https://en.wikipedia.org/wiki/Sudoku).

Please refer to `assignment.pdf` document for complete information.

## Requirements
[DrRacket](https://download.racket-lang.org/) software can be used for testing.

The software can also be installed using the bash command `sudo apt install racket`.

## Usage and Testing

You can run the file `model-implementation/test-model-implementaion.rkt` for some testcases and their outputs. (Use DrRacket 6.11)

Open and run the file `satSolver.rkt` using DrRacket.

Then on the DrRacket terminal, run `satisfy?` to check for the satisfiability and run `assign` to output the actual assignment of the variable. #t - True, #f - False.

Edit the testcase provided at the end of the file to test different inputs.

## License
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)