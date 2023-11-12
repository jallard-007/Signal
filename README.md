# Signal

WIP

A compiler for my custom language

Uses CMake as a build tool and Catch2 for testing

TODO:
- Code gen
    - unary operators
    - functions
    - loading variables into registers, if it cannot fit in reg then pointer to variable (have to know if its a pointer to the value or the value itself, maybe based on size?)
    - freeing registers that contain variables with changed values (have to store offset info that is accessible from register value, regIndex to var name map)
    - what to do with registers that contain struct members (maybe "variableName.structVariableName" for its name to info map, then store offset info)

Potential future tasks:
- Parser error recovery. Currently, simply fails after a single syntax error (only 1 error reported at a time). So allow for all (or at least more) syntax errors to be reported.
