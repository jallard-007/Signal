#pragma once
#include <string>
#include <vector>

bool openAndReadFile(const std::string&, std::vector<unsigned char>&);
bool openAndWriteFile(const std::string&, std::vector<unsigned char>&);
bool isBigEndian();
