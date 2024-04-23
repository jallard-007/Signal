# Signal

WIP

A compiler for my custom language

Uses CMake as a build tool and Catch2 for testing

TODO:

- add array support (in progress)
- add named arguments (in progress)
- create raw string parser (assigned to lukas)

- Parser
    - change array access expression type to binary op (?)

- Checker
    - change error reporting to be done on the spot (straight to stdout/passed in output stream)
        - need to standardize output format so that it can be parsed and tested
        - error code; location; description of the error;
            code snippet (whole statement, pull straight from tokenizer content),
            highlight which part of the statement is the problem
    - testing

- Code gen
    - full struct support
    - copying / storing structs

Potential future tasks:
- Parser error recovery. Currently, simply fails after a single syntax error (only 1 error reported at a time). So allow for all (or at least more) syntax errors to be reported.

IDEAS:
- change function call to unary op. would then need a tuple expression type as the operand
