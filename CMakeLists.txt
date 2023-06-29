cmake_minimum_required(VERSION 3.10)
find_package(Catch2 3 REQUIRED)
project(main CXX)
set(CMAKE_CXX_STANDARD 17)

add_library(tokenizer STATIC ./src/tokenizer/tokenizer.cpp ./src/token.cpp)
add_library(parser STATIC ./src/parser/parser.cpp ./src/nodes.cpp)
target_link_libraries(parser PRIVATE tokenizer)
add_library(prettyPrint STATIC ./src/prettyPrint/prettyPrint.cpp)
target_link_libraries(prettyPrint PRIVATE parser)
add_library(checker STATIC ./src/checker/checker.cpp)
target_link_libraries(checker PRIVATE prettyPrint)

set_target_properties(tokenizer parser prettyPrint checker PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/out)

add_executable(main ./src/main.cpp)
target_link_libraries(main PRIVATE prettyPrint checker)

add_executable(test ./src/tokenizer/test_tokenizer.cpp ./src/parser/test_parser.cpp ./src/prettyPrint/test_prettyPrint.cpp ./src/checker/test_checker.cpp)
target_link_libraries(test PRIVATE checker prettyPrint Catch2::Catch2WithMain)

if ( UNIX )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -Wall -Wextra -Wpedantic -Werror")
endif()
if ( MSVC )
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
endif()

# -fprofile-arcs -ftest-coverage
# -pg
# gprof main gmon.out | gprof2dot -s | dot -Gdpi=200 -Tpng -o output.png
