#include <RuleContext.h>
#include <any>
#include <bits/fs_fwd.h>
#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <endian.h>
#include <memory>
#include <support/Any.h>
#include <string>
#include <iostream>
#include <sstream>

#include <antlr4-common.h>
#include <vector>

#include "llvm/ADT/APInt.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "codegen.h"
#include "support.h"
#include "symboltable.h"

static bool generate_compound_bb = true;
static bool create_load_pointer = false;

std::shared_ptr<llvm::Module> IRGenerationVisitor::get_module() {
    return this->module;
}

std::string IRGenerationVisitor::ir_to_string() {
    std::string str;
    std::cout.flush();
    llvm::raw_string_ostream OS(str);
    OS << *this->module;
    OS.flush();
    std::vector<std::string> lines;

    std::stringstream ss(str);
    std::string line;
    std::string out; 
    while (std::getline(ss, line)) {
      out.append(line + '\n');
    }
    return out;
}

template <typename T>
std::optional<T> cast_opt(const std::any& a, std::string f, int l) {
    if (const T* v = std::any_cast<T>(&a))
        return std::optional<T>(*v);
    else {
        report_error("Bad cast; expected " + std::string(a.type().name()), f, l);
        return std::nullopt;
    }
}

template <typename T>
T cast_opt_reported(const std::any& result, std::string f, int l) {
    T value = nullptr;
    std::optional opt_value = cast_opt<T>(result, f, l);
    if (opt_value.has_value()) { value = opt_value.value(); }
    return value;
}


static std::vector<bool> selection_stmt_needs_merge_block = std::vector<bool>();
static int selection_stmt_idx = 0;

class FunctionPrototypeBuilder {
    llvm::Type* type;
    std::string name;
    std::vector<llvm::Type*> argument_types;
    std::vector<std::string> argument_names;
    llvm::Function* function;
    bool is_variadic = false;
    public:
        FunctionPrototypeBuilder() {
            this->name = "";
            this->argument_types = std::vector<llvm::Type*>();
            this->argument_names = std::vector<std::string>();
        };
        FunctionPrototypeBuilder* add_return_type(llvm::Type* type) { this->type = type; return this; }
        FunctionPrototypeBuilder* add_name(std::string name) { this->name = name; return this; }
        FunctionPrototypeBuilder* add_parameter(llvm::Type* t, std::string n) { 
            this->argument_types.push_back(t);
            this->argument_names.push_back(n); 
            return this; 
        }
        FunctionPrototypeBuilder* create_function(std::shared_ptr<llvm::Module> m) {
            llvm::FunctionType* ftype = nullptr;
            if (this->argument_names.size() == 0) {
                ftype = llvm::FunctionType::get(this->type, this->is_variadic);
            } else {
                ftype = llvm::FunctionType::get(this->type,
                                                this->argument_types,
                                                this->is_variadic);
            }
            this->function = llvm::Function::Create(ftype,
                                                    llvm::Function::ExternalLinkage,
                                                    this->name,
                                                    *m);
            return this;
        }
        FunctionPrototypeBuilder* set_argument_names() {
            int i = 0;
            for(auto &Arg: this->function->args()) {
                Arg.setName(this->argument_names[i++]);
            }
            return this;
        }
        FunctionPrototypeBuilder* set_argument_values(std::shared_ptr<SymbolTable> scope) {
            // Base assumption is function scope is a child of the one passed in.
            std::shared_ptr<SymbolTable> function_scope = scope->get_child_scope_with_name(this->name); 
            if (function_scope == nullptr) { // Base assumption was wrong!
                return this;
            }
            if (this->argument_names.size() == 1 && this->argument_names[0] == "") { 
                //report_info("No arguments to add");
                return this ;
            }

            //report_info("Adding arguments");
            for (auto& Arg : this->function->args()) {
                function_scope->get_symbol_local(Arg.getName().str())->value = &Arg;
            }
            return this;
        }
        FunctionPrototypeBuilder* set_variadic() { this->is_variadic = true; return this; }
        llvm::Function* take_function() {
            llvm::Function* f = this->function;
            this->function = nullptr;
            return f;
        }
};


antlrcpp::Any IRGenerationVisitor::visitProgram(CminusParser::ProgramContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);



    llvm::FunctionType* printf_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(*this->context),
                                                              {llvm::PointerType::get(*this->context, 0)},
                                                              true);
    this->module->getOrInsertFunction("printf", printf_type);

    llvm::FunctionType* scanf_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(*this->context),
                                                              {llvm::PointerType::get(*this->context, 0)},
                                                              true);
    this->module->getOrInsertFunction("__isoc99_scanf", scanf_type);
       
    // DEFINE INPUT AT GLOBAL SCOPE: `int input(void)`
    //////////////////////////////////////////////////////////////////////////
    if (!this->active_scope->exists("input")) { std::cout << "[Panic] Couldn't find `input` in symbol table" << std::endl; }

    llvm::Function* input =  FunctionPrototypeBuilder().add_name          ("input")
                                                      ->add_return_type   (llvm::Type::getInt32Ty(*this->context))
                                                      ->create_function   (this->module)
                                                      ->set_argument_names()
                                                      ->take_function     ();

    llvm::BasicBlock*   input_basic_block = llvm::BasicBlock::Create(*this->context,
                                                                     "input_entry",
                                                                     input);

    if (input_basic_block == nullptr) { report_error("No input_basic_block\n", __FUNCTION__, __LINE__); }

    this->builder->SetInsertPoint(input_basic_block);

    llvm::Value* scanned_val = this->builder->CreateAlloca(llvm::Type::getInt32Ty(*this->context));
    llvm::Value* format_string = this->builder->CreateGlobalStringPtr("%d", "format_string_decimal");
    this->builder->CreateCall(this->module->getFunction("__isoc99_scanf"), {format_string, scanned_val}, "scanf_call");
    llvm::Value* return_value = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), scanned_val, "scanned_val");

    this->builder->CreateRet(return_value);
    

     // DEFINE OUTPUT AT GLOBAL SCOPE: `void output(int)`
    //////////////////////////////////////////////////////////////////////////
    if (!this->active_scope->exists("output")) { std::cout << "[Panic] Couldn't find `output` in symbol table" << std::endl; }

    llvm::Function* output =  FunctionPrototypeBuilder().add_name          ("output")
                                                       ->add_parameter     (llvm::Type::getInt32Ty(*this->context), "val" )
                                                       ->add_return_type   (llvm::Type::getVoidTy(*this->context))
                                                       ->create_function   (this->module)
                                                       ->set_argument_names()
                                                       ->take_function     ();

    llvm::BasicBlock*   output_basic_block = llvm::BasicBlock::Create(*this->context,
                                                                      "output_entry",
                                                                       output);

    if (output_basic_block == nullptr) { report_error("No output_basic_block", __FUNCTION__, __LINE__); }

    this->builder->SetInsertPoint(output_basic_block);

    llvm::Value* val = output->getArg(0);
    llvm::Value* format_string_newline = this->builder->CreateGlobalStringPtr("%d\n", "format_string_decimal_newline");
    this->builder->CreateCall(this->module->getFunction("printf"), {format_string_newline, val}, "printf_call");

    this->builder->CreateRetVoid();


    antlrcpp::Any v = visitChildren(context);

    report_info(__FUNCTION__, DepthDelta::End);
    return v;
}

antlrcpp::Any IRGenerationVisitor::visitVar_declaration(CminusParser::Var_declarationContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    std::string name = context->ID()->getText();
    std::shared_ptr<Symbol> symbol = this->active_scope->get_symbol_local(name);
    if (symbol == nullptr) { report_error("Failed to find `" + name + "` in local scope; returning nullptr.", __FUNCTION__, __LINE__); return nullptr; }
    bool is_global = symbol->attributes.scope.name.compare("global") == 0 ? true : false;

    llvm::Value* var = nullptr;

    // This is gross, but I'm not sure how to best extract these two axis: type, scope-- in a type safe way
    if (context->NUM()) { // Array
        llvm::ArrayType* type = llvm::ArrayType::get(llvm::Type::getInt32Ty(*this->context), std::stoi(context->NUM()->getText()));
        if (is_global) {
            var = this->module->getOrInsertGlobal("global_array_" + name, type);
            llvm::GlobalVariable* global = this->module->getGlobalVariable("global_array_" + name);
            global->setInitializer(
                    llvm::ConstantArray::get(type,
                                             llvm::ConstantInt::get(llvm::Type::getInt32Ty(*this->context), 0)
                        )
                    );
            
            //llvm::GlobalVariable* global = std::any_cast<llvm::GlobalVariable*>(
            //        this->module->getOrInsertGlobal("global_array_" + name, type));
            //if (global->hasInitializer()) {
            //    global->setInitializer(
            //            llvm::ConstantArray::get(type,
            //                                     llvm::ConstantInt::get(llvm::Type::getInt32Ty(*this->context), 0)
            //                )
            //            );
            //}
            var = global;
            
        } else {
            var = this->builder->CreateAlloca(type, nullptr, "array_" + name);
        }
    } else {
        if (is_global) {
            llvm::IntegerType* type = llvm::Type::getInt32Ty(*this->context);
            var = this->module->getOrInsertGlobal("global_array_" + name, type);
        } else {
            var = this->builder->CreateAlloca(llvm::Type::getInt32Ty(*this->context), nullptr, "scalar_" + name);
        }
    }
    // Adding this to globals causes segfault
    // llvm::GlobalVariable* gVar = this->module->getNamedGlobal(symbol->name->c_str());
    // gVar->setInitializer(llvm::UndefValue::get(type));

    symbol->value = var;

    if (var == nullptr) { report_error("Declaration for variable `" + name + "` returned nullptr", __FUNCTION__, __LINE__); }
    report_info(__FUNCTION__, DepthDelta::End);
    return var;
} 

antlrcpp::Any IRGenerationVisitor::visitFun_declaration(CminusParser::Fun_declarationContext *context) {
    // fun_declaration : type_specifier ID OPEN_PAREN params CLOSE_PAREN compound_stmt;
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }

    // We start the construction of the function prototype here since we have enough knowledge to 
    // start such. That is, we know the type and ID. We'll descend further into the tree
    // to finish the prototype. Specifically, the params.
    if (this->prototype_builder != nullptr) { this->prototype_builder = nullptr; }
    this->prototype_builder = std::make_shared<FunctionPrototypeBuilder>();

    this->prototype_builder->add_name(context->ID()->getText());

    llvm::Type* return_type;
    if (context->type_specifier()->INT() != nullptr) { return_type = llvm::Type::getInt32Ty(*this->context); } 
    else if (context->type_specifier()->VOID() != nullptr) { return_type = llvm::Type::getVoidTy(*this->context); } 
    else { std::cout << "[E] Unreachable?" << std::endl; exit(1);}

    this->prototype_builder->add_return_type(return_type);

    antlrcpp::Any v = visitChildren(context);
    
    // TODO: Possibly need to check last basic block to see if:
    // - it is empty
    // - no return was added (if `void <func>(..)`)
    // Then add a `ret void`
    llvm::BasicBlock* currentBlock = this->builder->GetInsertBlock();
    if(return_type->isVoidTy()) {
        return this->builder->CreateRetVoid();  // Unconditionally add a void return type if a void func;
    }
    report_info(__FUNCTION__, DepthDelta::End);
    return v;
} 

antlrcpp::Any IRGenerationVisitor::visitParams(CminusParser::ParamsContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    // At this point we either have a VOID argument in which case we return to the function declaration or
    // we descend into param_list to get the list of parameters
    if (context->VOID()) {
        this->prototype_builder->add_parameter(llvm::Type::getVoidTy(*this->context), "");
    }

    visitChildren(context);

    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
} 

antlrcpp::Any IRGenerationVisitor::visitParam(CminusParser::ParamContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    // param : type_specifier ID ( OPEN_BRACKET CLOSE_BRACKET )?;

    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    // Here we add the parameter that triggered the rule before returning back to param_list
    llvm::Type* parameter_type = nullptr;
    bool is_void  = context->type_specifier()->VOID() != nullptr ? true: false;
    bool is_array = context->OPEN_BRACKET() != nullptr && context->CLOSE_BRACKET() != nullptr ? true : false;
    bool is_int   = !is_array && !is_void;

    if (is_array) { parameter_type = llvm::Type::getInt32PtrTy(*this->context); } else
    if (is_int)   { parameter_type = llvm::Type::getInt32Ty(*this->context);    } else 
    if (is_void)  { parameter_type = llvm::Type::getVoidTy(*this->context);     } 
    else { std::cout << "[E] Unreachable?" << std::endl; exit(1);}

    this->prototype_builder->add_parameter(parameter_type, context->ID()->getText());
    
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
} 

antlrcpp::Any IRGenerationVisitor::visitCompound_stmt(CminusParser::Compound_stmtContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    /* Compound Statement is special. At this point we know we've finished the function prototype!
      So complete that here. Then we create the basic block and set the insert point. Then, we 
      set `this->prototype_builder` to null because the compound statement may have another Function
      Prototype. But either way, we're done with the prototype at this point. After the Compound
     Statement has been visited then we call it a day (return nullptr;).
     */     

    // This is a hack. This basically iterates through each of the following statements. Then
    // if that statement is a selection_stmt and that selection statement is not the last statement
    // it presumes there is more code after the selection statement and will store true in
    // `selection_stmt_needs_merge_block` which is used by `visitSelection_stmt`. 
    // This does not address nested compound statements with their own if/else statements.
    if (context->statement_list() != nullptr) {
        int num_stmts = context->statement_list()->statement().size();
        for (int i = 0; i < num_stmts; ++i) {
            if (context->statement_list()->statement()[i] != nullptr &&
                context->statement_list()->statement()[i]->selection_stmt() != nullptr) { 
                if (i < num_stmts - 1) { selection_stmt_needs_merge_block.push_back(true); }
                else { selection_stmt_needs_merge_block.push_back(false); }
            }
        }
        selection_stmt_idx = selection_stmt_needs_merge_block.size(); 
    }
    
    llvm::Function* f = nullptr;
    bool is_function_compound_stmt = false;
    if (this->prototype_builder != nullptr) { // If this is null then we didnt visit a function
                                              // declaration prior to this.
        if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
        f = this->prototype_builder->create_function(this->module)
                                   ->set_argument_names()
                                   ->set_argument_values(this->active_scope)
                                   ->take_function();
        this->function_call_stack.push_back(f);
        is_function_compound_stmt = true;
    } else {
        f = this->function_call_stack.back();
    }

    if (generate_compound_bb) {
        llvm::BasicBlock* basic_block = llvm::BasicBlock::Create(*this->context,
                                                                 f->getName().str() + "_entry",
                                                                 f);

        if (basic_block == nullptr) { report_error("No basic_block", __FUNCTION__, __LINE__); }

        this->builder->SetInsertPoint(basic_block); // At this point we can begin adding IR from the
                                                    // compound statement 
    }
 
    this->prototype_builder = nullptr; // Set prototype builder to null before descending further.

    //TODO Handle anonymous compound statements. This leads to a segmentation fault.
    // Before we descend into the compound statement, add values from function parameter to the scope.
    std::shared_ptr<SymbolTable> next_scope = nullptr;
    if (this->active_scope->child_scope_exists(f->getName().str())) {
        next_scope = this->active_scope->get_child_scope_with_name(f->getName().str());
        next_scope->visited = true;
    } else {
        report_debug("Attempt to find child scope with name " +
                     f->getName().str() +
                     " failed. Searching for Anonymous scopes",
                     __FUNCTION__, __LINE__); 
        report_debug("Current Scope is: " + this->active_scope->get_scope_name(), 
                     __FUNCTION__, __LINE__); 
        
        // If we're in this branch then we are in a function and need to descend into anonymous scopes.
        // There can be any number of these anonymous scopes and they may be nested. So we check if there
        // are any we haven't visited. These scopes should be pushed onto the children list in the order
        // they occur so hopefully this is safe...
        next_scope = this->active_scope->get_next_unvisited_anonymous_child_scope();
        if(!next_scope) { 
            report_error("Next scope is null! This is going to hurt...", __FUNCTION__, __LINE__);
        }
    }

    report_debug("Entering Scope: " + next_scope->get_scope_name() + " and leaving scope " + next_scope->get_parent_scope()->get_scope_name(),__FUNCTION__, __LINE__);
    if (next_scope) { next_scope->visited = true; }
    this->active_scope = next_scope;

    antlrcpp::Any v = visitChildren(context);

    report_debug("Leaving Scope " + next_scope->get_scope_name() + " and entering " + this->active_scope->get_parent_scope()->get_scope_name(),__FUNCTION__, DepthDelta::Start);
    this->active_scope = this->active_scope->get_parent_scope();

    if (is_function_compound_stmt) { this->function_call_stack.pop_back(); }

    selection_stmt_needs_merge_block.clear();
    selection_stmt_idx = 0;

    report_info(__FUNCTION__, DepthDelta::End);
    return v; 
} 

antlrcpp::Any IRGenerationVisitor::visitAssignment_stmt(CminusParser::Assignment_stmtContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    std::string name = context->var()->ID()->getText();
    std::shared_ptr<Symbol> symbol = this->active_scope->get_symbol(name);

    if (symbol == nullptr) { report_error("Symbol for `" + name + "` was not found; returned nullptr", __FUNCTION__, __LINE__); }
    else { report_info("Found symbol: " + name, DepthDelta::None); }

    llvm::Value* var = cast_opt_reported<llvm::Value*>(visitVar(context->var()),__FUNCTION__, __LINE__);
    llvm::Value* value = cast_opt_reported<llvm::Value*>(visitExpression(context->expression()),__FUNCTION__, __LINE__);

    if (value == nullptr) { report_error("Expression for variable assignment of `" + name + "` returned nullptr", __FUNCTION__, __LINE__); }
    if (var == nullptr) { report_error("Variable `" + name + "` returned nullptr", __FUNCTION__, __LINE__); }

    llvm::Value* store = this->builder->CreateStore(value, var);

    if (store == nullptr) { report_error("Creation of store for variable `" + name + "` returned nullptr", __FUNCTION__, __LINE__); }

    if (store == nullptr) { report_error("Returned nullptr", __FUNCTION__, __LINE__); }
    report_info(__FUNCTION__, DepthDelta::End);
    return store;
} 

antlrcpp::Any IRGenerationVisitor::visitSelection_stmt(CminusParser::Selection_stmtContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    /*  selection_stmt : IF OPEN_PAREN expression CLOSE_PAREN statement
                       | IF OPEN_PAREN expression CLOSE_PAREN statement ELSE statement;
        Note that this rule can lead to nested if(-else) statements.
    */
    static int selection_stmt_idx = 0;

    llvm::BasicBlock* merge = nullptr;
    bool needs_merge_block = selection_stmt_needs_merge_block[selection_stmt_idx++];
    bool needs_else_block = context->ELSE() != nullptr ? true : false;
    if (!needs_else_block) { needs_merge_block = true; }

    llvm::Function* fn_block = this->function_call_stack.back();
    
    llvm::Value* proposition = cast_opt_reported<llvm::Value*>(visitExpression(context->expression()),__FUNCTION__, __LINE__);

    llvm::Value* truth_value = this->builder->CreateICmpNE(proposition, this->builder->getInt1(0), "prop_true");

    llvm::BasicBlock* affirm = llvm::BasicBlock::Create(*this->context, "cond_then", fn_block);
    llvm::BasicBlock* deny   = llvm::BasicBlock::Create(*this->context, "cond_else", fn_block);
    if (needs_merge_block) { merge = llvm::BasicBlock::Create(*this->context, "cond_exit", fn_block); }

    if (needs_else_block) { this->builder->CreateCondBr(truth_value, affirm, deny); } else 
    if (needs_merge_block && !needs_else_block) {
        this->builder->CreateCondBr(truth_value, affirm, merge); 
    }

    // Affirm; then
    fn_block->insert(fn_block->end(), affirm);
    this->builder->SetInsertPoint(affirm);

    visitStatement(context->statement()[0]);

    if(needs_merge_block) { this->builder->CreateBr(merge); }

    // Codegen of `then` can chagne the current block. Update `affirm` for the PHI.
    affirm = this->builder->GetInsertBlock();

    // Deny; else (may or may not exist)
    if (needs_else_block) { // This rule may only have two, at most, statements.
        fn_block->insert(fn_block->end(), deny);
        this->builder->SetInsertPoint(deny);

        
        visitStatement(context->statement()[1]);

        if (needs_merge_block) { this->builder->CreateBr(merge); }

        // Codegen of `then` can change the current block. Update `affirm` for the PHI.
        deny = this->builder->GetInsertBlock();
    }
    
    // Continue past the if(-else) blocks.      
    if (needs_merge_block) {
        fn_block->insert(fn_block->end(), merge);
        this->builder->SetInsertPoint(merge);
    }

    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;// visitChildren(context); 
} 

antlrcpp::Any IRGenerationVisitor::visitIteration_stmt(CminusParser::Iteration_stmtContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    /* iteration_stmt : WHILE OPEN_PAREN expression CLOSE_PAREN statement;
     */

    llvm::Function* fn_block = this->function_call_stack.back();

    // LOOP CONDITION
    //-------------------------------------
    llvm::BasicBlock* loop_condition = llvm::BasicBlock::Create(*this->context, "loop_prop", fn_block);
    this->builder->CreateBr(loop_condition);  // Branch unconditionally to check the loop condition
    fn_block->insert(fn_block->end(), loop_condition);
    this->builder->SetInsertPoint(loop_condition);

    // Visit the expression to generate the IR for the loop condition
    llvm::Value* condition = cast_opt_reported<llvm::Value*>(visitExpression(context->expression()),
                                                             __FUNCTION__,
                                                             __LINE__);
    llvm::Value* truth_value = this->builder->CreateICmpNE(condition, this->builder->getInt1(0), "prop_true");

    llvm::BasicBlock* loop_body = llvm::BasicBlock::Create(*this->context, "loop_body", fn_block);
    llvm::BasicBlock* loop_exit = llvm::BasicBlock::Create(*this->context, "loop_exit", fn_block);
    this->builder->CreateCondBr(truth_value, loop_body, loop_exit); 

    fn_block->insert(fn_block->end(), loop_body);
    this->builder->SetInsertPoint(loop_body);

    // Visit the statement to generate the IR for the loop body
    //  statement returning nullptr is fine. We dont expect the while statment body to return a value
    //  Additionally, we tell any follow on `Compound_stmt`s to not generate a new basic block. 
    //  This is so the loop_body basic block gets the content of "visitStatement"
    generate_compound_bb = false;
    llvm::Value* statement = cast_opt_reported<llvm::Value*>(visitStatement(context->statement()),
                                                             __FUNCTION__,
                                                             __LINE__);
    generate_compound_bb = true;

    this->builder->CreateBr(loop_condition);  // Branch unconditionally to check the loop condition

    //TODO: Only insert Merge BB if needed... NEED TO DO MERGE BLOCK
    fn_block->insert(fn_block->end(), loop_exit);
    this->builder->SetInsertPoint(loop_exit);

    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr; 
}

antlrcpp::Any IRGenerationVisitor::visitReturn_stmt(CminusParser::Return_stmtContext *context)  {
    report_info(__FUNCTION__, DepthDelta::Start);
    if (context->expression() == nullptr) { 
        report_info(__FUNCTION__, DepthDelta::End);
        return this->builder->CreateRetVoid(); 
    }


    llvm::Value* return_value = std::any_cast<llvm::Value*>(visitExpression(context->expression()));
    if (return_value == nullptr) { report_error("Expression returned nullptr", __FUNCTION__, __LINE__); }

    llvm::Value* v = this->builder->CreateRet(return_value);

    if (v == nullptr) { report_error("Returned nullptr", __FUNCTION__, __LINE__); }
    report_info(__FUNCTION__, DepthDelta::End);
    return v;
} 

antlrcpp::Any IRGenerationVisitor::visitVar(CminusParser::VarContext *context)  { 
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    llvm::Value* value = nullptr;
    std::string name               = context->ID()->getText();
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }

    std::shared_ptr<Symbol> symbol = this->active_scope->get_symbol(name);

    if (symbol == nullptr) { 
        std::string dbg_str = "Could not find " + name + " in current scope. Looking at in parent function scope!";
        report_debug(dbg_str,__FUNCTION__, DepthDelta::Start);

        llvm::Function* fn_block = this->function_call_stack.back();
        std::string fn_name = fn_block->getName().str();
        report_debug("Parent Function: " + fn_name,__FUNCTION__, DepthDelta::Start);

        std::shared_ptr<SymbolTable> fn_scope = this->active_scope->get_ancestor_scope_with_name(fn_name);
        if (fn_scope == nullptr) {
            report_debug("No Parent Function Scope: " + fn_name,__FUNCTION__, DepthDelta::Start);
        } else {
           symbol = fn_scope->get_symbol(name);
            if (symbol == nullptr) {
               report_debug("No Symbol: " + name + " in scope for function " + fn_name,__FUNCTION__, DepthDelta::Start);
            } else {
               report_debug("Found Symbol: " + name + " in scope for function " + fn_name,__FUNCTION__, DepthDelta::Start);
               symbol = fn_scope->get_symbol(name);
            }
        }

    }

    if (context->OPEN_BRACKET() && context->CLOSE_BRACKET()) {
        llvm::Value* idx = std::any_cast<llvm::Value*>(visitExpression(context->expression()));   
        llvm::Value* arr = symbol->value;

        llvm::ArrayType* array_type = llvm::ArrayType::get(llvm::Type::getInt32Ty(*this->context), 3);
        value = this->builder->CreateGEP(array_type, arr, {this->builder->getInt32(0), idx}, "elem_" + name);
    } else {
        if (symbol == nullptr)         { report_error("Could not find symbol for `" + name + "`, continuing with nullptr", __FUNCTION__, __LINE__ ); }
        if (symbol->value == nullptr)  { report_error("Could not find value for `"  + name + "`, continuing with nullptr", __FUNCTION__, __LINE__ ); }

        value = symbol->value;
    }
    if (value == nullptr) { report_error("Returned nullptr", __FUNCTION__, __LINE__); }
    report_info(__FUNCTION__, DepthDelta::End);
    return value;
} 

antlrcpp::Any IRGenerationVisitor::visitSimple_expression(CminusParser::Simple_expressionContext *context) { 
    // simple_expression : additive_expression relop additive_expression | additive_expression; 
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    llvm::Value* result = nullptr;
    llvm::Value* left = std::any_cast<llvm::Value*>(visitAdditive_expression(context->additive_expression()[0]));
    if (left == nullptr) { report_error("Additive Expression returned nullptr", __FUNCTION__, __LINE__); }
    
    // Either two additive_stmt values to compare or simply return left value.
    if (context->additive_expression().size() == 2 && context->relop() != nullptr) { // We have an binary operator in the mix
        
        llvm::Value* right = std::any_cast<llvm::Value*>(visitAdditive_expression(context->additive_expression()[1]));
        if (right == nullptr) { report_error("Additive Expression returned nullptr", __FUNCTION__, __LINE__); }

        // Operands may be pointers. If so, load them.
        if (left->getType()->isPointerTy() )  { left   = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), left, "left_relop"); }
        if (right->getType()->isPointerTy() ) { right  = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), right, "right_relop"); }
        
        llvm::CmpInst::Predicate op = std::any_cast<llvm::CmpInst::Predicate>(visitRelop(context->relop())); // This will probably wreck havoc
        result = this->builder->CreateICmp(op, left, right, "RelOp");

    } else 
    if (context->additive_expression().size() == 1) { // We dont actually conduct a relop
        if (left->getType()->isPointerTy() && !create_load_pointer) { 
            // We find ourselves in this case also when we have a function call with a simple expression.
            left = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), left, "simp_expr"); 
        }
        result = left;
    }
    else { report_error("Unreachable", __FUNCTION__, __LINE__); }

    if (result == nullptr) { report_error("Returned nullptr", __FUNCTION__, __LINE__); }
    report_info(__FUNCTION__, DepthDelta::End);
    return result;
} 

antlrcpp::Any IRGenerationVisitor::visitRelop(CminusParser::RelopContext *context) { 
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    if (context->EQUAL() != nullptr) { 
        return llvm::CmpInst::Predicate::ICMP_EQ;
    } else
    if (context->NOT_EQUAL() != nullptr) {
        return llvm::CmpInst::Predicate::ICMP_NE;
    } else
    if (context->GREATER() != nullptr) { 
        return llvm::CmpInst::Predicate::ICMP_SGT;
    } else
    if (context->GREATER_EQUAL() != nullptr) { 
        return llvm::CmpInst::Predicate::ICMP_SGE;
    } else
    if (context->LESS() != nullptr) { 
        return llvm::CmpInst::Predicate::ICMP_SLT;
    } else
    if (context->LESS_EQUAL() != nullptr) { 
        return llvm::CmpInst::Predicate::ICMP_SLE;
    }  
    else { report_error("Unreachable", __FUNCTION__, __LINE__); return nullptr; }
} 

antlrcpp::Any IRGenerationVisitor::visitAdditive_expression(CminusParser::Additive_expressionContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }

    if (context->term().size() == 1) { // Handle case where only single term!
        antlrcpp::Any v = visitTerm(context->term()[0]);
        report_info(__FUNCTION__, DepthDelta::End);
        return  v;
    }

    int terms = context->term().size();
    int ops = context->addop().size();
    assert(terms == (ops + 1));

    // This is really awkward. Grab first two operands (term addop term).
    // Create appropriate operation from that. The result of that operation
    // gets stored back into left for any future (left addop term) iterations.
    // These future (left addop term) are handled in a while loop until we
    // run out of right hand terms.
    int i = 0; // Operand index
    int j = 0; // Operation index
    llvm::Value* left;
    llvm::Value* right;
    antlrcpp::Any result;

    
    // Left Operand
    // Here we visit the `term`, get its result before casting it to a `Value*` if valid (otherwise all hope is lost)
    // Then we check if the value is a pointer. If it is then we create a load for it.
    result = visitTerm(context->term()[i++]);
    if (result.has_value() && result.type() == typeid(llvm::Value*)) { left = std::any_cast<llvm::Value*>(result); }
    else { report_error("Expected `llvm::Value*` but found " + std::string(result.type().name()), __FUNCTION__, __LINE__ ); }
    if (left->getType()->isPointerTy() ) { left  = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), left, "left_addop"); }

    // Right Operand
    // Same process as above.
    result = visitTerm(context->term()[i++]);
    if (result.has_value() && result.type() == typeid(llvm::Value*)) { right = std::any_cast<llvm::Value*>(result); }
    else { report_error("Expected `llvm::Value*` but found " + std::string(result.type().name()), __FUNCTION__, __LINE__ ); }
    if (right->getType()->isPointerTy()) { right = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), right, "right_addop"); }

    if (left == nullptr || right == nullptr) { report_error("One or more operands is null!", __FUNCTION__, __LINE__); }
    
    
    // Operation
    if (context->addop()[j]->getText() == "+") { left = this->builder->CreateAdd(left, right, "addop"); } else
    if (context->addop()[j]->getText() == "-") { left = this->builder->CreateSub(left, right, "subop"); } 
    else { report_error("Unreachable", __FUNCTION__, __LINE__);  }

    // Do remaining (left addop term) operations
    ++j;
    while (i < terms) {
        right = std::any_cast<llvm::Value*>(visitTerm(context->term()[i++]));
        if (left->getType()->isPointerTy() ) { left  = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), left, "left_addop"); }
        if (right->getType()->isPointerTy()) { right = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), right, "right_addop"); }

        if (context->addop()[j]->getText() == "+") { left = this->builder->CreateAdd(left, right, "addop"); } else
        if (context->addop()[j]->getText() == "-") { left = this->builder->CreateSub(left, right, "subop"); } 
        else { report_error("Unreachable", __FUNCTION__, __LINE__); }
        j++;
    }
        
    if (left == nullptr) { report_error("Returned nullptr", __FUNCTION__, __LINE__); }
    report_info(__FUNCTION__, DepthDelta::End);
    return left;
} 

antlrcpp::Any IRGenerationVisitor::visitTerm(CminusParser::TermContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    if (context->factor().size() == 1) { // Handle case where only single factor!
        llvm::Value* v = std::any_cast<llvm::Value*>(visitChildren(context));
        if (v == nullptr) { report_error("Returned nullptr", __FUNCTION__, __LINE__); }
        report_info(__FUNCTION__, DepthDelta::End);
        return v;
    }

    int factors = context->factor().size();
    int ops = context->mulop().size();

    // This is really awkward. Grab first two operands (factor mulop factor).
    // Create appropriate operation from that. The result of that operation
    // gets stored back into left for any future (left mulop factor) iterations.
    // These future (left mulop factor) are handled in a while loop until we
    // run out of right hand factor.
    int i = 0;
    int j = 0;
    llvm::Value* left;
    llvm::Value* right;
    antlrcpp::Any result;
    
    // Left Operand
    result = visitFactor(context->factor()[i++]);
    if (result.has_value() && result.type() == typeid(llvm::Value*)) { left = std::any_cast<llvm::Value*>(result); }
    else { report_error("Expected `llvm::Value*` but found " + std::string(result.type().name()), __FUNCTION__, __LINE__ ); }
    if (left->getType()->isPointerTy()) { left = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), left, "left_mulop"); }

    // Right Operand
    result = visitFactor(context->factor()[i++]);
    if (result.has_value() && result.type() == typeid(llvm::Value*)) { right = std::any_cast<llvm::Value*>(result); }
    else { report_error("Expected `llvm::Value*` but found " + std::string(result.type().name()), __FUNCTION__, __LINE__ ); }
    if (right->getType()->isPointerTy()) { right = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), right, "right_mulop"); }

    if (left == nullptr || right == nullptr) { report_error("One or more operands is null!", __FUNCTION__, __LINE__); }

    // Operation
    if (context->mulop()[j]->getText() == "*") { left = this->builder->CreateMul(left,  right, "mulop"); } else
    if (context->mulop()[j]->getText() == "/") { left = this->builder->CreateSDiv(left, right, "divop"); }
    else { report_error("Unreachable", __FUNCTION__, __LINE__ ); exit(-1); }

    // Iterate through remained of (left mulop factor)
    ++j;
    while (i < factors) {
        right = std::any_cast<llvm::Value*>(visitFactor(context->factor()[i++]));
        if (left->getType()->isPointerTy()) { left = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), left, "left_mulop"); }
        if (right->getType()->isPointerTy()) { right = this->builder->CreateLoad(llvm::Type::getInt32Ty(*this->context), right, "right_mulop"); }

        if (context->mulop()[j]->getText() == "*") { left = this->builder->CreateMul(left,  right, "mulop"); } else
        if (context->mulop()[j]->getText() == "/") { left = this->builder->CreateSDiv(left, right, "divop"); }
        else { report_error("Unreachable",__FUNCTION__, __LINE__); }
        j++;
    }

    if (left == nullptr) { report_error("Returned nullptr", __FUNCTION__, __LINE__); }
    report_info(__FUNCTION__, DepthDelta::End);
    return left;
} 

antlrcpp::Any IRGenerationVisitor::visitFactor(CminusParser::FactorContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    antlrcpp::Any v = nullptr;
    if (this->active_scope == nullptr) { report_error("Active scope is a nullptr, this is going to hurt!", __FUNCTION__, __LINE__); }
    if (context->expression()) {
        report_info(__FUNCTION__, DepthDelta::None);
        v = visitExpression(context->expression());
    } else 
    if (context->var()) { 
        report_info(__FUNCTION__, DepthDelta::None);
        v = visitVar(context->var());
    } else 
    if (context->call()) { 
        report_info(__FUNCTION__, DepthDelta::None);
        v = visitCall(context->call());
    } else 
    if (context->NUM()) {
        report_info("num", DepthDelta::None);
        llvm::Value* value = llvm::ConstantInt::get(*this->context, llvm::APInt(32, std::stoi(context->NUM()->getText()), true));
        if (value == nullptr) { report_error("Constant Value returned nullptr!", __FUNCTION__, __LINE__); }
        report_info(__FUNCTION__, DepthDelta::End);
        return value;
    } else {
        report_error("Unreachable: ", __FUNCTION__, __LINE__);
        report_info(__FUNCTION__, DepthDelta::End);
      return nullptr;
    }
    report_info(__FUNCTION__, DepthDelta::End);
    return v;
}

antlrcpp::Any IRGenerationVisitor::visitCall(CminusParser::CallContext *context) { 
    report_info(__FUNCTION__, DepthDelta::Start);
    llvm::Value* v = nullptr;

    std::string name = context->ID()->getText();
    llvm::Function* callee = this->module->getFunction(name);
    if(!callee) { report_error("Unable to find `" + name + "` to create function call", __FUNCTION__, __LINE__ ); }

    std::vector<llvm::Value*> arguments = std::vector<llvm::Value*>();
    if (context->args() != nullptr && context->args()->arg_list() != nullptr) { // relies on short-circuiting
        int i = 0;
        for (auto arg : context->args()->arg_list()->expression()) {
            if(arg->simple_expression()->additive_expression().size() == 1) {
                FunctionAttributes* f_attr = this->active_scope->get_function_parameters(name);
                create_load_pointer = f_attr->parameters[i].is_array;
            }
            v = cast_opt_reported<llvm::Value*>(visitExpression(arg),__FUNCTION__, __LINE__);
            if (v != nullptr) {
                arguments.push_back(v);
            }
            create_load_pointer = false;
            ++i;
        }
    }
    
    if (callee->getReturnType()->isVoidTy()) { // Instructions returning void cannot have a name!
        v = this->builder->CreateCall(callee, arguments);
    } else {
        v = this->builder->CreateCall(callee, arguments, "call_" + name );
    }
    if (v == nullptr) { report_error("CreateCall for " + name + " returned nullptr", __FUNCTION__, __LINE__ ); }
    

    report_info(__FUNCTION__, DepthDelta::End);
    return v;
} 

antlrcpp::Any IRGenerationVisitor::visitMain_declaration(CminusParser::Main_declarationContext *context)  { 
    report_info(__FUNCTION__, DepthDelta::Start);

    if (this->prototype_builder != nullptr) { this->prototype_builder = nullptr; }
    this->prototype_builder = std::make_shared<FunctionPrototypeBuilder>();
    this->prototype_builder
        ->add_name(context->MAIN()->getText())
        ->add_return_type(llvm::Type::getVoidTy(*this->context));
    
    this->function_call_stack.clear();

    antlrcpp::Any v = visitCompound_stmt(context->compound_stmt());

    // Main only returns void. And it does it here (no `return_stmt` grammar rule).
    this->builder->CreateRetVoid();
    this->function_call_stack.pop_back();
    report_info(__FUNCTION__, DepthDelta::End);
    return v;
} 

antlrcpp::Any IRGenerationVisitor::visitType_specifier(CminusParser::Type_specifierContext *context) {
    return visitChildren(context);
} 
antlrcpp::Any IRGenerationVisitor::visitDeclaration_list(CminusParser::Declaration_listContext *context)       { 
    return visitChildren(context);
}
antlrcpp::Any IRGenerationVisitor::visitDeclaration(CminusParser::DeclarationContext *context) { 
    return visitChildren(context);
}           
antlrcpp::Any IRGenerationVisitor::visitParam_list(CminusParser::Param_listContext *context) { 
    return visitChildren(context);
} 
antlrcpp::Any IRGenerationVisitor::visitLocal_declarations(CminusParser::Local_declarationsContext *context) {
    return visitChildren(context); 
} 
antlrcpp::Any IRGenerationVisitor::visitStatement_list(CminusParser::Statement_listContext *context) {
    // TODO, this can iterate through statements.
    // Here, we'll know if the next statement is a selection/iteration statement and if there
    // are any that follow it. This means we can set a boolean here
    return visitChildren(context);
}
antlrcpp::Any IRGenerationVisitor::visitStatement(CminusParser::StatementContext *context) {
    return visitChildren(context); 
} 
antlrcpp::Any IRGenerationVisitor::visitExpression_stmt(CminusParser::Expression_stmtContext *context) {
    return visitChildren(context);
} 
antlrcpp::Any IRGenerationVisitor::visitMulop(CminusParser::MulopContext *context) { 
    return visitChildren(context);
} 
antlrcpp::Any IRGenerationVisitor::visitArgs(CminusParser::ArgsContext *context) { 
    return visitChildren(context);
} 
antlrcpp::Any IRGenerationVisitor::visitArg_list(CminusParser::Arg_listContext *context) { 
    return visitChildren(context);
} 
antlrcpp::Any IRGenerationVisitor::visitAddop(CminusParser::AddopContext *context) {
    return visitChildren(context);
} 
antlrcpp::Any IRGenerationVisitor::visitExpression(CminusParser::ExpressionContext *context) { 
    return visitSimple_expression(context->simple_expression()); 
} 
