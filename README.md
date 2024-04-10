# Signal

WIP

A compiler for my custom language

Uses CMake as a build tool and Catch2 for testing

TODO:

- add array support
- add named arguments
- create raw string parser (partially done)

- Parser
    - change array access expression type to binary op
- Checker
    - testing
- Code gen
    - full struct support
    - copying / storing structs
    - testing unary operator
    - testing updating jump ops with the correct offset

Potential future tasks:
- Parser error recovery. Currently, simply fails after a single syntax error (only 1 error reported at a time). So allow for all (or at least more) syntax errors to be reported.

IDEAS:
- change function call to unary op. would then need a tuple expression type as the operand
