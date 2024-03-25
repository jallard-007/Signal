# Signal

WIP

A compiler for my custom language

Uses CMake as a build tool and Catch2 for testing

TODO:

create raw string parser

- Parser
    - change array access expression type to binary op
- Code gen
    - storing values back to memory (assignment bin ops), need address and type of expression
    - functions, scopes, nested scopes, breaking out of loops (Use stack markers)
    - unary operators
    - what to do with registers that contain struct members (maybe "variableName.structVariableName" for its name to info map, then store offset info)

Potential future tasks:
- Parser error recovery. Currently, simply fails after a single syntax error (only 1 error reported at a time). So allow for all (or at least more) syntax errors to be reported.
