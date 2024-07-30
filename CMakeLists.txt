# minimum required CMAKE version
CMAKE_MINIMUM_REQUIRED(VERSION 3.15 FATAL_ERROR)

# Follow Directions from here:
#   https://github.com/antlr/antlr4/tree/master/runtime/Cpp/cmake
# TO BUILD PROJECT:
# mkdir build && cd build 
# cmake -DCMAKE_BUILD_TYPE=Release ../ && make && ./cminus ../../Cminus-Input-3.txt
project(CMinusGrammar)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)


# Define Our Project
###########################
set(BINARY_NAME                   "cminus_executable")
set(ANTLR_TARGET_NAME             "cminus")
set(CMAKE_CXX_STANDARD            17)
set(LLVM_C_INCLUDE_DIRECTORY      "/usr/include/llvm-c-17/")
set(LLVM_INCLUDE_DIRECTORY        "/usr/include/llvm-17/")
set(ANTLR4_WITH_STATIC_CRT        OFF)
set(ANTLR_EXECUTABLE              /home/chuck/programming/ANTLR-4.7.2/antlr-4.13.1-complete.jar)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)


#### Configure ANTLR
###############################
# required if linking to static library
add_definitions(-DANTLR4CPP_STATIC)

# using /MD flag for antlr4_runtime (for Visual C++ compilers only)


# add external build for antlrcpp
include(ExternalAntlr4Cpp)
include_directories(${ANTLR4_INCLUDE_DIRS})

find_package(ANTLR REQUIRED)

# Call macro to add lexer, parser, and visitor for the grammar to your build dependencies.
antlr_target("${ANTLR_TARGET_NAME}" Cminus.g4
             LEXER
             PARSER
             VISITOR)


#### Configure LLVM
#################################
# Required ZLib, LibEdit, LibCurl, and LibZstd
find_package(LLVM 17 REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})



#### Configure Target
#################################

if(CMAKE_BUILD_TYPE MATCHES "Debug")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -fsanitize=address")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -fsanitize=address")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -g -fsanitize=address")
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -g -fsanitize=address")
endif()

# add generated grammar to demo binary target
add_executable(${BINARY_NAME}
               CminusMain.cpp
               pass_manager.cpp
               symboltable.cpp
               codegen.cpp
               support.cpp
               ${ANTLR_${ANTLR_TARGET_NAME}_CXX_OUTPUTS}
               )
target_compile_options(${BINARY_NAME} PRIVATE "-g")

# include generated files in project environment
target_include_directories(${BINARY_NAME} 
  PUBLIC
    ${ANTLR4_INCLUDE_DIRS}
    ${ANTLR_${ANTLR_TARGET_NAME}_OUTPUT_DIR}
    ${LLVM_INCLUDE_DIRECTORY}
    ${LLVM_C_INCLUDE_DIRECTORY}
)

llvm_map_components_to_libnames(llvm_libs support core irreader analysis passes)


target_link_libraries(${BINARY_NAME}
                      antlr4_static
                      ${llvm_libs})

#add_custom_command(TARGET ${BINARY_NAME}
#                   POST_BUILD
#                   COMMAND ${CMAKE_COMMAND}
#                     -E copy ${ANTLR4_RUNTIME_LIBRARIES} .
#                   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

message(STATUS "Project:...... ${CMAKE_PROJECT_NAME}")
message(STATUS "CXX Compiler:. ${CMAKE_CXX_COMPILER}")
message(STATUS "Source Dir:... ${PROJECT_SOURCE_DIR}")
message(STATUS "Binary Name:.. ${BINARY_NAME}")
message(STATUS "Binary Dir:... ${PROJECT_BINARY_DIR}")
message(STATUS "Antlr4 Ver:... ${ANTLR_VERSION}")
message(STATUS "Antlr4 Tag:... ${ANTLR4_TAG}")
message(STATUS "Antlr4 Dir:... ${ANTLR_${ANTLR_TARGET_NAME}_OUTPUT_DIR}")

# Add a custom target to move `compile_commands.json` to source dir.
# `compile_commands.json` is used by LSP for hints, completions, etc.
if(${CMAKE_EXPORT_COMPILE_COMMANDS})
  message(STATUS "Moving compile_commands.json to ${PROJECT_SOURCE_DIR}")
  add_custom_target(
    copy-compile-commands ALL
    ${CMAKE_COMMAND} -E copy_if_different
      "${PROJECT_BINARY_DIR}/compile_commands.json"
      "${PROJECT_SOURCE_DIR}")
endif()
