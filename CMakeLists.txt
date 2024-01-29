cmake_minimum_required(VERSION 3.10)
if ( UNIX )
    find_package(Catch2 3 REQUIRED)
endif()
project(main CXX)
set(CMAKE_CXX_STANDARD 20)

add_library(common STATIC
    ./src/codeGen/codeGen.cpp
    ./src/checker/checker.cpp
    ./src/prettyPrint/prettyPrint.cpp
    ./src/parser/parser.cpp
    ./src/nodes.cpp
    ./src/tokenizer/tokenizer.cpp
    ./src/token.cpp
    ./src/utils.cpp
)
add_library(interpreter STATIC ./src/interpreter/interpreter.cpp)

set_target_properties(interpreter common PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/out)

add_executable(main ./src/main.cpp)
target_link_libraries(main PRIVATE common)

if ( UNIX )
    add_executable(tests ./src/testingMemPool.cpp ./src/tokenizer/test_tokenizer.cpp ./src/parser/test_parser.cpp ./src/prettyPrint/test_prettyPrint.cpp ./src/checker/test_checker.cpp ./src/interpreter/test_interpreter.cpp ./src/codeGen/test_codeGen.cpp)
    target_link_libraries(tests PRIVATE common interpreter Catch2::Catch2WithMain)
    include(CTest)
    include(Catch)
    catch_discover_tests(tests)
endif()

set(DEV 1)
if ( DEV )
    set(CMAKE_BUILD_TYPE Debug)
else()
    set(CMAKE_BUILD_TYPE Release)
endif()
if ( UNIX )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic -Werror")
    if ( DEV )
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -flto")
    endif()
endif()

if ( MSVC )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4 /WX")
endif()

# -fprofile-arcs -ftest-coverage -fPIC -O0 or -O1

# -pg
# gprof main gmon.out | gprof2dot -s | dot -Gdpi=200 -Tpng -o output.png
