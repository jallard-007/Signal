# Compiler and its settings
GXX=g++
GXXFLAGS=-std=c++17 -Wall -Wpedantic -Wextra -Wconversion -Werror

# Source and object directories
SRC_DIR=./src
OBJ_DIR=./obj

# compiles client/room
default:
	mkdir -p $(OBJ_DIR)
	make all

all: obj/main.o obj/lexer.o 
	$(GXX) $(GXXFLAGS) $^ -o main 

# src/main.cpp
obj/main.o: src/main.cpp
	$(GXX) $(GXXFLAGS) -c $< -o $@

obj/lexer.o: src/lexer.cpp src/lexer.hpp
	$(GXX) $(GXXFLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ_DIR) main
