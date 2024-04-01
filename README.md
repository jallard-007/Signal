# Signal

WIP

A compiler for my custom language

Uses CMake as a build tool and Catch2 for testing

TODO:

create raw string parser

- Parser
    - change array access expression type to binary op
- Code gen
    - writing return value on return
    - using return value from function call
    - testing unary operator
    - testing updating jump ops with the correct offset
    - copying / storing structs

Potential future tasks:
- Parser error recovery. Currently, simply fails after a single syntax error (only 1 error reported at a time). So allow for all (or at least more) syntax errors to be reported.
