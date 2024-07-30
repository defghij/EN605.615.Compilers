#include <RuleContext.h>
#include <bits/fs_fwd.h>
#include <memory>
#include <support/Any.h>
#include <map>

#include <antlr4-common.h>

#include "llvm/IR//LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include "CminusBaseVisitor.h"
#include "CminusParser.h"
#include "symboltable.h"

class FunctionPrototypeBuilder;

class IRGenerationVisitor : public CminusBaseVisitor {
    std::unique_ptr<llvm::LLVMContext> context;
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::shared_ptr<llvm::Module> module;
    std::unique_ptr<std::map<std::string, llvm::Value*>> named_values;
    std::shared_ptr<FunctionPrototypeBuilder> prototype_builder = nullptr;
    std::vector<llvm::Function*> function_call_stack;
    std::vector<llvm::Value*> value_stack;
    std::shared_ptr<SymbolTable> global_scope;
    std::shared_ptr<SymbolTable> active_scope;
    
    public:

        IRGenerationVisitor(std::shared_ptr<SymbolTable> symbol_table) {
            this->context = std::make_unique<llvm::LLVMContext>();
            this->builder = std::make_unique<llvm::IRBuilder<>>(*this->context);
            this->module  = std::make_shared<llvm::Module>("Cminus_module", *this->context);
            this->global_scope = symbol_table;
            this->active_scope = symbol_table;
            this->function_call_stack = std::vector<llvm::Function*>();
            this->value_stack = std::vector<llvm::Value*>();
        }
        std::string ir_to_string();
        std::shared_ptr<llvm::Module> get_module();

        // VISITOR PATTERN FUNCTIONS
        llvm::Value* codegen_program(CminusParser::ProgramContext *context)                         ;
        llvm::Value* codegen_declaration_list(CminusParser::Declaration_listContext *context)       ;
        llvm::Value* codegen_declaration(CminusParser::DeclarationContext *context)                 ;           
        llvm::Value* codegen_var_declaration(CminusParser::Var_declarationContext *context)         ; 
        llvm::Value* codegen_type_specifier(CminusParser::Type_specifierContext *context)           ; 
        llvm::Value* codegen_fun_declaration(CminusParser::Fun_declarationContext *context)         ; 
        llvm::Value* codegen_params(CminusParser::ParamsContext *context)                           ; 
        llvm::Value* codegen_param_list(CminusParser::Param_listContext *context)                   ; 
        llvm::Value* codegen_param(CminusParser::ParamContext *context)                             ; 
        llvm::Value* codegen_compound_stmt(CminusParser::Compound_stmtContext *context)             ; 
        llvm::Value* codegen_local_declarations(CminusParser::Local_declarationsContext *context)   ; 
        llvm::Value* codegen_statement_list(CminusParser::Statement_listContext *context)           ;
        llvm::Value* codegen_statement(CminusParser::StatementContext *context)                     ; 
        llvm::Value* codegen_expression_stmt(CminusParser::Expression_stmtContext *context)         ; 
        llvm::Value* codegen_assignment_stmt(CminusParser::Assignment_stmtContext *context)         ; 
        llvm::Value* codegen_selection_stmt(CminusParser::Selection_stmtContext *context)           ; 
        llvm::Value* codegen_iteration_stmt(CminusParser::Iteration_stmtContext *context)           ; 
        llvm::Value* codegen_return_stmt(CminusParser::Return_stmtContext *context)                 ; 
        llvm::Value* codegen_expression(CminusParser::ExpressionContext *context)                   ; 
        llvm::Value* codegen_var(CminusParser::VarContext *context)                                 ; 
        llvm::Value* codegen_simple_expression(CminusParser::Simple_expressionContext *context)     ; 
        llvm::Value* codegen_relop(CminusParser::RelopContext *context)                             ; 
        llvm::Value* codegen_additive_expression(CminusParser::Additive_expressionContext *context) ; 
        llvm::Value* codegen_addop(CminusParser::AddopContext *context)                             ; 
        llvm::Value* codegen_term(CminusParser::TermContext *context)                               ; 
        llvm::Value* codegen_mulop(CminusParser::MulopContext *context)                             ; 
        llvm::Value* codegen_factor(CminusParser::FactorContext *context)                           ; 
        llvm::Value* codegen_call(CminusParser::CallContext *context)                               ; 
        llvm::Value* codegen_args(CminusParser::ArgsContext *context)                               ; 
        llvm::Value* codegen_arg_list(CminusParser::Arg_listContext *context)                       ; 
        llvm::Value* codegen_main_declaration(CminusParser::Main_declarationContext *context)       ; 

        // VISITOR PATTERN FUNCTIONS
        antlrcpp::Any visitProgram(CminusParser::ProgramContext *context)                         ;
        antlrcpp::Any visitDeclaration_list(CminusParser::Declaration_listContext *context)       ;
        antlrcpp::Any visitDeclaration(CminusParser::DeclarationContext *context)                 ;           
        antlrcpp::Any visitVar_declaration(CminusParser::Var_declarationContext *context)         ; 
        antlrcpp::Any visitType_specifier(CminusParser::Type_specifierContext *context)           ; 
        antlrcpp::Any visitFun_declaration(CminusParser::Fun_declarationContext *context)         ; 
        antlrcpp::Any visitParams(CminusParser::ParamsContext *context)                           ; 
        antlrcpp::Any visitParam_list(CminusParser::Param_listContext *context)                   ; 
        antlrcpp::Any visitParam(CminusParser::ParamContext *context)                             ; 
        antlrcpp::Any visitCompound_stmt(CminusParser::Compound_stmtContext *context)             ; 
        antlrcpp::Any visitLocal_declarations(CminusParser::Local_declarationsContext *context)   ; 
        antlrcpp::Any visitStatement_list(CminusParser::Statement_listContext *context)           ;
        antlrcpp::Any visitStatement(CminusParser::StatementContext *context)                     ; 
        antlrcpp::Any visitExpression_stmt(CminusParser::Expression_stmtContext *context)         ; 
        antlrcpp::Any visitAssignment_stmt(CminusParser::Assignment_stmtContext *context)         ; 
        antlrcpp::Any visitSelection_stmt(CminusParser::Selection_stmtContext *context)           ; 
        antlrcpp::Any visitIteration_stmt(CminusParser::Iteration_stmtContext *context)           ; 
        antlrcpp::Any visitReturn_stmt(CminusParser::Return_stmtContext *context)                 ; 
        antlrcpp::Any visitExpression(CminusParser::ExpressionContext *context)                   ; 
        antlrcpp::Any visitVar(CminusParser::VarContext *context)                                 ; 
        antlrcpp::Any visitSimple_expression(CminusParser::Simple_expressionContext *context)     ; 
        antlrcpp::Any visitRelop(CminusParser::RelopContext *context)                             ; 
        antlrcpp::Any visitAdditive_expression(CminusParser::Additive_expressionContext *context) ; 
        antlrcpp::Any visitAddop(CminusParser::AddopContext *context)                             ; 
        antlrcpp::Any visitTerm(CminusParser::TermContext *context)                               ; 
        antlrcpp::Any visitMulop(CminusParser::MulopContext *context)                             ; 
        antlrcpp::Any visitFactor(CminusParser::FactorContext *context)                           ; 
        antlrcpp::Any visitCall(CminusParser::CallContext *context)                               ; 
        antlrcpp::Any visitArgs(CminusParser::ArgsContext *context)                               ; 
        antlrcpp::Any visitArg_list(CminusParser::Arg_listContext *context)                       ; 
        antlrcpp::Any visitMain_declaration(CminusParser::Main_declarationContext *context)       ; 
};

/*
class ExprAST {
    public:
        virtual ~ExprAST() = default;
        virtual Value* codegen() = 0;
};

class NumberExprAST : public ExprAST {
    int64_t Val;
    public: 
        NumberExprAST(int64_t Val) : Val(Val) {}
        Value* codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
    std::string Name;

    public:
        VariableExprAST(const std::string &Name) : Name(Name) {}
        Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
            : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
        Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;

    public:
        CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args) 
            : Callee(Callee), Args(std::move(Args)) {}
        Value *codegen() override;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;

    public:
        PrototypeAST(const std::string &Name, std::vector<std::string> Args)
            : Name(Name), Args(std::move(Args)) {}
        Function *codegen();
        const std::string &getName() const { return Name; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

    public:
        FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body) 
            : Proto(std::move(Proto)), Body(std::move(Body)) {}
        Function *codegen();
}
*/;
