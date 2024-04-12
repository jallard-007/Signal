#pragma once
#include <string>
#include <vector>

bool openAndReadFile(const std::string& filePath, std::vector<unsigned char>& dest);
bool openAndWriteFile(const std::string& filePath, std::vector<unsigned char>& src);
