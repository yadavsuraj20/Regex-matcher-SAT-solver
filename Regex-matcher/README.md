# Regex Matcher/Recognizer

The purpose of this assignment is two-fold:
1. Understanding regular expressions as a way to describe strings with certain properties.
2. Implementing a matcher(more correctly a recognizer) for regular expressions using trees and graphs.

Please refer to `assignment.pdf` document for complete information.

## Requirements
[DrRacket](https://download.racket-lang.org/) software can be used for testing.

The software can also be installed using the bash command `sudo apt install racket`.

## Usage and Testing

You can run the file `model-implementation/test-model-implementaion.rkt` for some testcases and their outputs. (Use DrRacket 6.11)

Open and run the file `regToDfa.rkt` using DrRacket.

Then on the DrRacket terminal, run `(matches? graph "abba")` to check if the input string matches the given regex and run `graph` to output the graph for the given regex.

Edit the testcase provided at the end of the file to test different inputs.

## License
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)