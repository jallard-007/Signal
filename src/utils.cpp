#include <cstdint>
#include <fstream>
#include <iostream>
#include "utils.hpp"

bool openAndReadFile(const std::string& file, std::vector<unsigned char>& buffer) {
    std::ifstream t(file, std::ios::binary);
    if (!t.is_open()) {
        std::cerr << "Could not open file: " << file << '\n';
        return false;
    }
    t.seekg(0, std::ios::end);
    size_t size = (size_t)t.tellg();
    buffer.resize(size);
    t.seekg(0);
    t.read((char *)&buffer[0], size);
    buffer[size - 1] = 0;
    return true;
}

bool openAndWriteFile(const std::string& file, std::vector<unsigned char>& content) {
    std::ofstream t;
    t.open(file, std::ios::out | std::ios::binary);
    if (!t.is_open()) {
        std::cerr << "Could not open file: " << file << '\n';
        return false;
    }
    t.write((char *)content.data(), content.size());
    return true;
}
