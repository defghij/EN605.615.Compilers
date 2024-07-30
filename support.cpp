#include <string>
#include <support/Any.h>
#include "support.h"

#define DEBUG 0
#define INFO 0
#define ERROR 0

static int depth = 0;

std::string get_depth_str() {
      std::string indent;
      for (int i = 0; i < depth; ++i) {
        indent.append("  |");
      }
      return indent;
}

void report_debug(std::string msg, std::string func, int line) {
    std::string indent = get_depth_str();
    if (DEBUG) {
        std::cout << indent + "  |---[D] " + msg + " (" + __FILE_NAME__  + "::" + func + "::" + std::to_string(line) + ")" << std::endl;
    }
}

void report_info(std::string msg, DepthDelta delta) {
    depth += delta;
    std::string state = "";
    if (delta == DepthDelta::Start) {
        state = "begin";
    } else
    if (delta == DepthDelta::End) {
        state = "end";
    }
    std::string indent = get_depth_str();
    if (delta == DepthDelta::End) { indent.append("  |"); }
    if (state != "") {
      state = "[" + state + "]";
    }
    if (delta == DepthDelta::End) { state.append("  "); }
    if (INFO) {
        std::cout << indent << " [I]" + state + " " + msg << std::endl;
    }
}

void report_error(std::string msg, std::string func, int line) {
    std::string indent = get_depth_str();
    if (ERROR) {
        std::cout << indent +  "  |---[E] " + msg + " (" + __FILE_NAME__  + "::" + func + "::" + std::to_string(line) + ")" << std::endl;
    }
}
