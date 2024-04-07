cmake_minimum_required(VERSION 3.10)
if ( UNIX )
    find_package(Catch2 3 REQUIRED)
endif()
project(main CXX)
set(CMAKE_CXX_STANDARD 20)

if ( MSVC )
    add_compile_options(/W4 /WX)
else()
    add_compile_options(-Wall -Wextra -Wpedantic -Werror -Wno-attributes)
    set ( CMAKE_CXX_FLAGS_DEBUG  "${CMAKE_CXX_FLAGS_DEBUG} -O0" )
    set ( CMAKE_CXX_FLAGS_RELEASE  "${CMAKE_CXX_FLAGS_RELEASE} -flto" )
endif()

# common lib
add_library(common STATIC
    src/codeGen/interpreter/codeGen.cpp
    src/checker/checker.cpp
    src/prettyPrint/prettyPrint.cpp
    src/parser/parser.cpp
    src/nodes.cpp
    src/tokenizer/tokenizer.cpp
    src/token.cpp
    src/utils.cpp
    src/bytecodeDesign/bytecodeDesign.cpp
    src/compTime/compTime.cpp
)
target_include_directories(common PRIVATE src)

# interpreter lib
add_library(interpreter STATIC src/interpreter/interpreter.cpp src/bytecodeDesign/bytecodeDesign.cpp)
target_include_directories(interpreter PRIVATE src)

# set lib properties
set_target_properties(interpreter common PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/out)

# interpreter executable
add_executable(signal src/interpreter/interpreterMain.cpp src/utils.cpp)
target_include_directories(signal PRIVATE src)
target_link_libraries(signal PRIVATE interpreter)

# main executable
add_executable(main src/main.cpp)
target_include_directories(main PRIVATE src)
target_link_libraries(main PRIVATE common)

# tests
if ( UNIX )
    add_executable(tests src/testingMemPool.cpp src/tokenizer/test_tokenizer.cpp src/parser/test_parser.cpp src/prettyPrint/test_prettyPrint.cpp src/checker/test_checker.cpp src/interpreter/test_interpreter.cpp src/codeGen/interpreter/test_codeGen.cpp src/bytecodeDesign/test_bytecodeDesign.cpp src/compTime/test_compTime.cpp)
    target_include_directories(tests PRIVATE src/)
    target_link_libraries(tests PRIVATE common interpreter Catch2::Catch2WithMain)
    include(CTest)
    include(Catch)
    catch_discover_tests(tests)
endif()

# current method
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 9.67076
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 9.60463
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 9.67158
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 9.56606

# strncmp
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 12.2402
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 12.1476
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 12.1315
# jallard@NoSignal:~/documents/github/CL$ ./main sampleCode/benchmark.pr 
# Execution time: 12.0802

# -fprofile-arcs -ftest-coverage -fPIC -O0 or -O1

# -pg
# gprof main gmon.out | gprof2dot -s | dot -Gdpi=200 -Tpng -o output.png
