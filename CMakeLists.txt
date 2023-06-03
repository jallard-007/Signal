cmake_minimum_required(VERSION 3.10)
project(main, CXX)
set(CMAKE_CXX_STANDARD 17)
add_executable(main ./src/main.cpp ./src/lexer.cpp)