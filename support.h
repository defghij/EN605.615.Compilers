#pragma once

#include <string>

enum DepthDelta {
    Start = 1,
    None = 0,
    End = -1,
};

void report_debug(std::string msg, std::string func, int line);
void report_info(std::string msg, DepthDelta depth);
void report_error(std::string msg, std::string func, int line);
