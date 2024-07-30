#pragma once

// CPP HEADERS
#include <memory>
#include <map>

// LLVM HEADERS
#include "llvm/IR//LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

// LOCAL HEADERS
#include "CminusParser.h"

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, llvm::Value *> NamedValues;

void InitializeModule();
void visitor_passes(CminusParser::ProgramContext *tree);
