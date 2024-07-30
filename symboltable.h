#pragma once

#include <memory>
#include <string>
#include <support/Any.h>
#include <vector>
#include <variant>

#include "CminusBaseVisitor.h"
#include "llvm/IR/Value.h"
class SymbolTable;
class SymbolTableVisitor;

enum SymbolInvocation {
    Set,
    Declare,
    Call,
};

enum AttributeKind {
    Function,
    Variable,
    Array,
    Constant,
};


struct ScopeAttributes {
    std::string name;
    int depth;
    std::shared_ptr<SymbolTable> scope;
};


struct FunctionParameter {
    std::string type;
    std::string id;
    bool is_array;
};

struct FunctionAttributes {
    std::string type;
    std::vector<FunctionParameter> parameters;
};

struct VariableAttributes {
    std::string type;
};

struct ArrayAttributes {
    std::string type;
    int num_elements;
};

struct ConstantAttributes {
    std::string type;
    int value;
};  

// Can use `if auto* attr = std::get_if<Variant>(&attribute)) { /* do something with attr */ }
using KindAttributes = std::variant<FunctionAttributes,VariableAttributes,ArrayAttributes,ConstantAttributes>;

struct Attribute {
    AttributeKind kind;
    KindAttributes attributes;
    ScopeAttributes scope;
};

struct Symbol {
    std::shared_ptr<std::string> name;
    Attribute attributes;
    llvm::Value* value = nullptr;
};

struct KindAttributesString {
    void operator()(const FunctionAttributes& fa) {
        //do something
    }   
    void operator()(const VariableAttributes& va) {
        //do something
    }   
    std::string operator()(const ArrayAttributes& aa) {
        return "{ .type = " + aa.type + ", .num_elements = " + std::to_string(aa.num_elements) + " }";
    }   
    std::string operator()(const ConstantAttributes& ca) {
        return "{ .type = " + ca.type + ", .value = " + std::to_string(ca.value) + " }";
    }   

};
using AttributeTable = std::vector<Attribute>;

class SymbolTable : public std::enable_shared_from_this<SymbolTable>{
    std::unordered_map<std::string, std::shared_ptr<Symbol>>  symbols; 
    std::shared_ptr<SymbolTable> parent;                   
    std::string name;
    int depth;
    std::vector<std::shared_ptr<SymbolTable>> children;

    public:
        bool visited = false; 
        // Initialize a Symbol Table
        SymbolTable(std::string scope_name) {
            name = scope_name;
            depth = 0;
            symbols = std::unordered_map<std::string, std::shared_ptr<Symbol>>();
            parent = nullptr;
            children = std::vector<std::shared_ptr<SymbolTable>>();
        }
    
        bool add_symbol(std::shared_ptr<Symbol> symbol);
        bool exists(const std::string name);
        std::shared_ptr<Symbol> get_symbol_global(const std::string name);
        std::shared_ptr<Symbol> get_symbol_function(const std::string name);
        std::shared_ptr<Symbol> get_symbol_local(const std::string name);
        std::shared_ptr<Symbol> get_symbol(const std::string name);
        std::string get_scope_name();
        std::shared_ptr<SymbolTable> create_child_scope();
        std::shared_ptr<SymbolTable> create_child_scope(std::string name);
        std::shared_ptr<SymbolTable> get_parent_scope();
        std::shared_ptr<SymbolTable> get_global_scope();
        std::shared_ptr<SymbolTable> get_child_scope_with_name(std::string);
        std::shared_ptr<SymbolTable> get_next_unvisited_anonymous_child_scope();
        bool child_scope_exists(std::string name);
        std::shared_ptr<SymbolTable> get_child_scope_at_index(int);
        int get_children_count();
        std::shared_ptr<SymbolTable> get_ancestor_scope_with_name(std::string);
        std::shared_ptr<SymbolTable> get_first_named_ancestor_scope();
        std::string to_string();
        FunctionAttributes* get_function_parameters(std::string name);
};

class SymbolTableVisitor : public CminusBaseVisitor {
    std::shared_ptr<SymbolTable> global_scope;
    std::shared_ptr<SymbolTable> active_scope;
    std::shared_ptr<AttributeTable> attribute_table;
    int active_depth = 0;
    public:

        SymbolTableVisitor(std::shared_ptr<SymbolTable>    symbol_table,
                           std::shared_ptr<AttributeTable> attribute_table) {
            global_scope    = symbol_table;
            active_scope    = global_scope;
            active_depth    = 0;
            attribute_table = attribute_table;
        }

        void enter_scope(std::string name);
        void enter_scope();
        void exit_scope();
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
        antlrcpp::Any visitMain_declaration(CminusParser::Main_declarationContext *context)            ; 
};
