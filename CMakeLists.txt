cmake_minimum_required(VERSION 3.10)
find_package(Catch2 3 REQUIRED)
project(main, CXX)
set(CMAKE_CXX_STANDARD 17)

add_library(tokenizer STATIC ./src/tokenizer/tokenizer.cpp ./src/token.cpp)
add_library(parser STATIC ./src/parser/parser.cpp ./src/nodes.cpp)
target_link_libraries(parser PRIVATE tokenizer)

set_target_properties(tokenizer parser PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/out)

add_executable(main ./src/main.cpp)
target_link_libraries(main PRIVATE tokenizer)

add_executable(test ./src/tokenizer/test_tokenizer.cpp ./src/parser/test_parser.cpp)
target_link_libraries(test PRIVATE parser)
target_link_libraries(test PRIVATE Catch2::Catch2WithMain)

if ( CMAKE_COMPILER_IS_GNUCC )
    target_compile_options(main PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
if ( MSVC )
    target_compile_options(main PRIVATE /W4)
endif()