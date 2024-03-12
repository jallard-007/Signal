# Signal

WIP

A compiler for my custom language

Uses CMake as a build tool and Catch2 for testing

TODO:
- Code gen
    - figure out how to update jump ops. we start with a RS_JUMP to keep it compact, but what if 
        the relative jump is more than what a int8 can do? have to change to a larger jump, which means changing to R_JUMP (or bigger) and shifting the code over to fit the extra data. this results in indexes changing, and relative jumps being incorrect.
        potential solution: update all jump ops at the end of function generation, track how much shifting as been done so that they can be updated accordingly. NEED TO DRAW IT OUT

    - functions, scopes, nested scopes, breaking out of loops (Use stack markers)
    - unary operators
    - loading variables into registers, if it cannot fit in reg then pointer to variable (have to know if its a pointer to the value or the value itself, maybe based on size?)
    - freeing registers that contain variables with changed values (have to store offset info that is accessible from register value, regIndex to var name map)
    - what to do with registers that contain struct members (maybe "variableName.structVariableName" for its name to info map, then store offset info)

Potential future tasks:
- Parser error recovery. Currently, simply fails after a single syntax error (only 1 error reported at a time). So allow for all (or at least more) syntax errors to be reported.
