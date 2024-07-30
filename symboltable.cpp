#include <RuleContext.h>
#include <bits/fs_fwd.h>
#include <memory>
#include <stdexcept>
#include <support/Any.h>
#include <unordered_map>

#include "CminusBaseVisitor.h"
#include <antlr4-common.h>
#include <variant>
#include <vector>
#include "symboltable.h"
#include "support.h"

class SymbolTableVisitor;
class SymbolTable;
//using LiteralTable = std::unordered_set<std::string>;
using AttributeTable = std::vector<Attribute>;
#define DEBUG 1

static std::shared_ptr<std::string> next_scope_name = nullptr; 
static std::shared_ptr<std::vector<Symbol>> symbols_to_add = nullptr;

std::string AttributeString[4] {
    "Function",
    "Variable",
    "Array",
    "Constant"
};

std::string symbol_to_string(std::shared_ptr<Symbol> symbol) {
    std::string out = "";
    out.append("Kind: " + AttributeString[symbol->attributes.kind] + ";  ");

    if (FunctionAttributes* k = std::get_if<FunctionAttributes>(&symbol->attributes.attributes)) {
        out.append("Name: " + *symbol->name + ";  ");
        out.append("Type: " + k->type + ";  ");
        out.append("Args: ");
        for (int i = 0; i < k->parameters.size(); ++i) {
            FunctionParameter param = k->parameters[i];
            out.append( param.type + " " + param.id );
            if (param.is_array) { out.append("[]"); }
            if (i < k->parameters.size() - 1) { out.append(","); }
            else { out.append(";  "); }
        }
        out.append("Scope: " + symbol->attributes.scope.name + ";  ");
        out.append("Depth: " + std::to_string(symbol->attributes.scope.depth) + "\n");
    }

    if (VariableAttributes* k = std::get_if<VariableAttributes>(&symbol->attributes.attributes)) {
        out.append("Name: " + *symbol->name + ";  ");
        out.append("Type: " + k->type + ";  ");
        out.append("Scope: " + symbol->attributes.scope.name + ";  ");
        out.append("Depth: " + std::to_string(symbol->attributes.scope.depth) + "\n");
    }

    if (ArrayAttributes* k = std::get_if<ArrayAttributes>(&symbol->attributes.attributes)) {
        out.append("Name: " + *symbol->name + ";  ");
        out.append("Type: " + k->type + ";  ");
        out.append("Size: " + std::to_string(k->num_elements) + ";  ");
        out.append("Scope: " + symbol->attributes.scope.name + ";  ");
        out.append("Depth: " + std::to_string(symbol->attributes.scope.depth) + "\n");
    }

    if (ConstantAttributes* k = std::get_if<ConstantAttributes>(&symbol->attributes.attributes)) {
        out.append("Name: " + *symbol->name + ";  ");
        out.append("Type: " + k->type + ";  ");
        out.append("Size: " + std::to_string(k->value) + ";  ");
        out.append("Scope: " + symbol->attributes.scope.name + ";  ");
        out.append("Depth: " + std::to_string(symbol->attributes.scope.depth) + "\n");
    }
    return out;
}

int symbol_depth(std::shared_ptr<Symbol> symbol) {
    int depth = 0;
    if (FunctionAttributes* k = std::get_if<FunctionAttributes>(&symbol->attributes.attributes)) {
        depth = symbol->attributes.scope.depth;
    }

    if (VariableAttributes* k = std::get_if<VariableAttributes>(&symbol->attributes.attributes)) {
        depth = symbol->attributes.scope.depth;
    }

    if (ArrayAttributes* k = std::get_if<ArrayAttributes>(&symbol->attributes.attributes)) {
        depth = symbol->attributes.scope.depth;
    }

    if (ConstantAttributes* k = std::get_if<ConstantAttributes>(&symbol->attributes.attributes)) {
        depth = symbol->attributes.scope.depth;
    }
    return depth;
}

////
//// SYMBOL TABLE IMPLEMENTATIONS
///////////////////////////////////////////////////////////////////////

bool SymbolTable::add_symbol(std::shared_ptr<Symbol> symbol) {
    if (exists(*symbol->name)) {
        return false;
    }
    //std::cout << symbol_to_string(symbol) << std::endl;
    symbols[*symbol->name] = std::move(symbol);
    return true;
}
bool SymbolTable::exists(const std::string name) {
    return symbols.find(name) != symbols.end();
}

std::shared_ptr<Symbol> SymbolTable::get_symbol(const std::string name) {
    // This searched in successively higher scopes. In Cminus we cant have free functions.
    // Additionally, this will respect variable shadowing by returning the first encounter.
    std::shared_ptr<Symbol> symbol = this->get_symbol_local(name);
    std::shared_ptr<SymbolTable> parent = this->parent;

    while (parent != nullptr && symbol == nullptr) {
        symbol = parent->get_symbol_local(name);        
        parent = parent->parent;
    }
    return symbol;
}

FunctionAttributes* SymbolTable::get_function_parameters(std::string name){ 

        std::shared_ptr<Symbol> function_symbol = this->get_symbol_global(name);
        if (!function_symbol)
            report_error("No function smybol " + name + " for call!",__FUNCTION__,__LINE__);

        FunctionAttributes* a = 
            std::get_if<FunctionAttributes>(&function_symbol->attributes.attributes);
        return  a;
}

std::shared_ptr<Symbol> SymbolTable::get_symbol_function(const std::string name) {
    // Cminus does not allow nested or free functions. So we can be certain that
    // the first non-anonymous scope is the function-level parent scope of `this`.
    // So we look to see if the symbol is defined there.
    std::shared_ptr<SymbolTable> named_ancestor_scope = this->get_first_named_ancestor_scope();
    std::shared_ptr<Symbol> s;
    try {
        s = named_ancestor_scope->symbols.at(name);
    } catch (const std::out_of_range& oor) {
       s = nullptr; 
    }
    return s;
}

std::shared_ptr<Symbol> SymbolTable::get_symbol_local(const std::string name) {
    std::shared_ptr<Symbol> s;
    try {
        s = symbols.at(name);
    } catch (const std::out_of_range& oor) {
       s = nullptr; 
    }
    return s;
}

std::shared_ptr<Symbol> SymbolTable::get_symbol_global(const std::string name) {
    std::shared_ptr<SymbolTable> global_scope = this->get_global_scope();
    std::shared_ptr<Symbol> s;
    try {
        s = global_scope->symbols.at(name);
    } catch (const std::out_of_range& oor) {
       s = nullptr; 
    }
    return s;
}

std::string SymbolTable::get_scope_name() {
    return name ;
}

std::shared_ptr<SymbolTable> SymbolTable::create_child_scope() {
    std::shared_ptr<SymbolTable> child = std::make_shared<SymbolTable>("anonymous"); // Create a child
    children.push_back(child);                                            // Add child to list of children
    child->parent = shared_from_this();                                   // Set child's parent
    return child;                                                         // Deliver Child
}

std::shared_ptr<SymbolTable> SymbolTable::create_child_scope(std::string name) {
    std::shared_ptr<SymbolTable> child = std::make_shared<SymbolTable>(name); // Create a child
    children.push_back(child);                                            // Add child to list of children
    child->parent = shared_from_this();                                   // Set child's parent
    return child;                                                         // Deliver Child
}

std::shared_ptr<SymbolTable> SymbolTable::get_parent_scope() {
    return parent;
}

std::shared_ptr<SymbolTable> SymbolTable::get_global_scope() {
    std::shared_ptr<SymbolTable> parent_scope = this->get_parent_scope();

    // Base case: already at global scope
    if (parent_scope == nullptr) { return std::shared_ptr<SymbolTable>(this); }

    while(1) {
        if (parent_scope->get_parent_scope() != nullptr) { // Climb until parent is null.
            parent_scope = parent_scope->get_parent_scope();
        } else {
            break;
        }
    }
    assert(parent_scope != nullptr); // Literally die if we return a nullptr for parent scope;
    return parent_scope;
}

std::shared_ptr<SymbolTable> SymbolTable::get_child_scope_with_name(std::string name) {
    for (auto &child : this->children) {
        if ( name.compare(child->name) == 0) { return child; }
    }
    return nullptr;
}

std::shared_ptr<SymbolTable> SymbolTable::get_next_unvisited_anonymous_child_scope() {
    for (auto child : children) {
        if (!child->visited && child->get_scope_name().compare("anonymous") == 0) {
            return child;            
        }
    }
    return nullptr;
}

std::shared_ptr<SymbolTable> SymbolTable::get_ancestor_scope_with_name(std::string name) {
    std::shared_ptr<SymbolTable> parent_scope = this->get_parent_scope();

    // Base case: already at global scope
    if (parent_scope == nullptr) { return nullptr; }  // We didnt find ancestor with name

    while(1) {
        // Found an ancestor with the request name!
        if (parent_scope->name.compare(name) == 0) { return parent_scope; }

        if (parent_scope->get_parent_scope() != nullptr) { // Climb until parent is null.
            parent_scope = parent_scope->get_parent_scope();
        } else { 
            // If parent is null, we dont have an ancestor with the request name,
            return nullptr;
        }
    }
}

std::shared_ptr<SymbolTable> SymbolTable::get_first_named_ancestor_scope() {
    std::shared_ptr<SymbolTable> parent_scope = this->get_parent_scope();

    // Base case: already at global scope
    if (parent_scope == nullptr) { return nullptr; }  // We didnt find a named

    while(1) {
        // Found a named ancestor!
        if (parent_scope->name.compare("anonymouns") != 0) { return parent_scope; }

        if (parent_scope->get_parent_scope() != nullptr) { // Climb until parent is null.
            parent_scope = parent_scope->get_parent_scope();
        } else { 
            // If parent is null, we dont have an ancestor with the request name,
            return nullptr;
        }
    }
}

bool SymbolTable::child_scope_exists(std::string name) {
    bool exists = false;
    for (auto child : children) { 
        if (child->get_scope_name().compare(name) == 0) {
            exists = true;
        }
    }
    return exists;
}

std::shared_ptr<SymbolTable> SymbolTable::get_child_scope_at_index(int idx) {
    if (0 <= idx && idx < this->children.size()) {
        return this->children[idx];
    } 
    return nullptr;
}

int SymbolTable::get_children_count() {
    return this->children.size();
}

std::string SymbolTable::to_string() {
    std::string out;
    out.append("Scope: " + this->name + "\n");
    for (auto const& entry : symbols) {
        std::shared_ptr<Symbol> symbol = entry.second;

        int depth = symbol_depth(symbol);
        for (int i = 0; i < depth; ++i) { out.append("  "); }

        out.append(symbol_to_string(symbol));
    }
    out.append("\n");

    for (auto child : children) { if (nullptr != child) { out.append(child->to_string()); } }

    return out;
}

void SymbolTableVisitor::enter_scope(std::string name) {
    std::shared_ptr<SymbolTable> child = std::make_shared<SymbolTable>(name);
    ++active_depth;
    active_scope = active_scope->create_child_scope(name);
    return;
}

void SymbolTableVisitor::enter_scope() {
    std::shared_ptr<SymbolTable> child = std::make_shared<SymbolTable>("anonymous");
    ++active_depth;
    active_scope = active_scope->create_child_scope();
    return;
}

void SymbolTableVisitor::exit_scope() {
    --active_depth;
    active_scope = active_scope->get_parent_scope();
    return;
}


////
//// SYMBOL TABLE VISITOR IMPLEMENTATIONS
///////////////////////////////////////////////////////////////////////

/* program : declaration_list main_declaration EOF; // Valid program ends with a 'main' function.
*/
antlrcpp::Any SymbolTableVisitor::visitProgram(CminusParser::ProgramContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
       
    // DEFINE INPUT AT GLOBAL SCOPE `int input(void)`
    //////////////////////////////////////////////////////////////////////////
    std::shared_ptr<std::string> iname = std::make_shared<std::string>("input");
    std::shared_ptr<Symbol> isymbol = std::make_shared<Symbol>();
    isymbol->name = iname;
    
    std::vector<FunctionParameter> iparam = std::vector<FunctionParameter>();
    FunctionParameter ivoid = FunctionParameter {
        .type = "void",
        .id = "",
        .is_array = false,
    };
    iparam.push_back(ivoid);

    isymbol->attributes.kind = AttributeKind::Function;
    isymbol->attributes.scope = ScopeAttributes { 
           .name  = active_scope->get_scope_name(),
           .depth = active_depth,
           .scope = active_scope
    };
    isymbol->attributes.attributes = FunctionAttributes {
       .type         = "int",
       .parameters   = iparam,
    };
    active_scope->add_symbol(isymbol);

     // DEFINE OUTPUT AT GLOBAL SCOPE `void output(int)`?
    //////////////////////////////////////////////////////////////////////////
    std::shared_ptr<std::string> oname = std::make_shared<std::string>("output");
    std::shared_ptr<Symbol> osymbol = std::make_shared<Symbol>();
    osymbol->name = oname;
    
    std::vector<FunctionParameter> oparam = std::vector<FunctionParameter>();
    FunctionParameter ovoid = FunctionParameter {
        .type = "int",
        .id = "val",
        .is_array = false,
    };
    oparam.push_back(ovoid);

    osymbol->attributes.kind = AttributeKind::Function;
    osymbol->attributes.scope  = ScopeAttributes { 
           .name  = active_scope->get_scope_name(),
           .depth = active_depth,
           .scope = active_scope
    };
    osymbol->attributes.attributes = FunctionAttributes {
       .type         = "void",
       .parameters   = oparam,
    };
    active_scope->add_symbol(osymbol);

    visitChildren(context);
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/* declaration_list : declaration*;                 // But may have some declarations prior.
 */

antlrcpp::Any SymbolTableVisitor::visitDeclaration_list(CminusParser::Declaration_listContext *context) {
    visitChildren(context);
    return nullptr;
}

/* declaration : var_declaration
               | fun_declaration;
*/
antlrcpp::Any SymbolTableVisitor::visitDeclaration(CminusParser::DeclarationContext *context) {
    visitChildren(context);
    return nullptr;
}

/* var_declaration : INT ID SEMICOLON                   
                   | INT ID OPEN_BRACKET NUM CLOSE_BRACKET SEMICOLON;
*/
antlrcpp::Any SymbolTableVisitor::visitVar_declaration(CminusParser::Var_declarationContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
    // Here we may potentially add one of two types of variable declarations to the current scope.
    // The first is just a variable of the form `int my_var;` and the second is an array of the
    // for `int my_array[<num>];`. We will do neither if the symbol <ID> has already been declared.
    if (!context->ID()) { 
        // Maybe this isnt an issue?
        //report_error("Empty ID found! Crazy, right?",__FUNCTION__,__LINE__);
        visitChildren(context);
        report_info(__FUNCTION__, DepthDelta::End);
        return nullptr;
    } 

    std::shared_ptr<std::string> name = std::make_shared<std::string>(context->ID()->getText());
    std::shared_ptr<Symbol> symbol = std::make_shared<Symbol>();
    symbol->name = name;


    // Check if we've already defined this symbol in the currently active scope. 
    // If so then we simply report an error and return.
    if (active_scope->exists(*name)) {
        report_error("Variable "  + *symbol->name + " redecalared in current scope",__FUNCTION__,__LINE__);
        visitChildren(context);
        report_info(__FUNCTION__, DepthDelta::End);
        return nullptr;
    }
    
    // Create the appropriate symbol for the variable which will be added to the SymbolTable
    if (context->OPEN_BRACKET() && context->CLOSE_BRACKET()) { // Array: INT ID OPEN_BRACKET NUM CLOSE_BRACKET SEMICOLON
       symbol->attributes.kind = AttributeKind::Array;
       symbol->attributes.scope = ScopeAttributes { 
               .name  = active_scope->get_scope_name(),
               .depth = active_depth,
               .scope = active_scope
       };
       symbol->attributes.attributes = ArrayAttributes {
           .type         = context->INT()->getText(),
           .num_elements = std::stoi(context->NUM()->getText()),
       };
    } 
    else { // Variable: INT ID SEMICOLON
       symbol->attributes.kind = AttributeKind::Variable;
       symbol->attributes.scope = ScopeAttributes { 
               .name  = active_scope->get_scope_name(),
               .depth = active_depth,
               .scope = active_scope
       };
       symbol->attributes.attributes = VariableAttributes {
           .type         = context->INT()->getText(),
       };
    }

    active_scope->add_symbol(symbol);

    visitChildren(context);
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/* type_specifier : INT 
                  | VOID;
*/
antlrcpp::Any SymbolTableVisitor::visitType_specifier(CminusParser::Type_specifierContext *context) {
    visitChildren(context);
    return nullptr;
}

/* fun_declaration : type_specifier ID OPEN_PAREN params CLOSE_PAREN compound_stmt;
 */
antlrcpp::Any SymbolTableVisitor::visitFun_declaration(CminusParser::Fun_declarationContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
    // Here we add a function declaration to the symbol table.
    //
    std::shared_ptr<std::string> name = std::make_shared<std::string>(context->ID()->getText());
    std::shared_ptr<Symbol> symbol = std::make_shared<Symbol>();
    symbol->name = name;
    symbol->attributes.kind = AttributeKind::Function;


    // Check if we've already defined this symbol in the currently active scope. 
    // If so then we simply report and error and return.
    if (active_scope->exists(*name)) {
        report_error("Function "  + *symbol->name + " redecalared in current scope",__FUNCTION__,__LINE__);
        visitChildren(context);
        report_info(__FUNCTION__, DepthDelta::End);
        return nullptr;
    }
    symbol->attributes.scope = ScopeAttributes { 
               .name  = active_scope->get_scope_name(),
               .depth = active_depth,
               .scope = active_scope
    };

    FunctionAttributes attributes = { .type = context->type_specifier()->getText()  };

    if (context->params()->param_list()) { // Non-VOID parameters

        // Get the parameter types and IDs for the function declaration.
        for (auto param : context->params()->param_list()->param()) {
            if (param->type_specifier()->INT()) { 
                bool is_array = false;
                if (param->OPEN_BRACKET() && param->CLOSE_BRACKET()) { is_array = true; }
                
                FunctionParameter parameter = FunctionParameter {
                            .type = param->type_specifier()->getText(),
                            .id = param->ID()->getText(),
                            .is_array = is_array,
                };
                attributes.parameters.push_back(parameter);
            } else {
                report_error("Function "+ *symbol->name + " has both INT and VOID parameters!",__FUNCTION__,__LINE__);
            }
        }
        for (auto param : context->params()->param_list()->param()) {
        }
    }

    symbol->attributes.attributes = attributes;
    active_scope->add_symbol(symbol);
    
    next_scope_name = std::make_shared<std::string>(context->ID()->getText());
    symbols_to_add = std::make_shared<std::vector<Symbol>>();
    visitChildren(context);
    
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/* params : param_list
          | VOID;
*/
antlrcpp::Any SymbolTableVisitor::visitParams(CminusParser::ParamsContext *context) {
    visitChildren(context);
    return nullptr;
}

/* param_list : param ( COMMA param )*;
 */
antlrcpp::Any SymbolTableVisitor::visitParam_list(CminusParser::Param_listContext *context) {
    visitChildren(context);
    return nullptr;
}

/* param : type_specifier ID ( OPEN_BRACKET CLOSE_BRACKET )?;
 */
 antlrcpp::Any SymbolTableVisitor::visitParam(CminusParser::ParamContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);

    // Here we queue up symbols to be added to the next invoked scope. This, admittedly,
    // relies on the assumption the next scope will be that from the visitFun_declaration 
    // node. Hope it always works... lol


    if (next_scope_name != nullptr) {
        std::shared_ptr<std::string> name = std::make_shared<std::string>(context->ID()->getText());
        Symbol symbol = Symbol();
        symbol.name = name;

        // Check if we've already defined this symbol in the currently active scope. 
        // If so then we simply report and error and return.
        if (active_scope->exists(*name)) {
            report_error("Variable "  + *symbol.name + " redecalared in current scope",__FUNCTION__,__LINE__);
            visitChildren(context);
            report_info(__FUNCTION__, DepthDelta::End);
            return nullptr;
        }

        if (context->OPEN_BRACKET() && context->CLOSE_BRACKET()) {
            symbol.attributes.kind = AttributeKind::Array;
        } else {
            symbol.attributes.kind = AttributeKind::Variable;
        }


        std::string parameter_type = "";
        if (context->type_specifier()->INT()) { 
            parameter_type.append(context->type_specifier()->INT()->getText()); 
        }
        else { 
            parameter_type.append(context->type_specifier()->VOID()->getText()); 
        }

        VariableAttributes attributes = {
            .type = parameter_type,
        };
        symbol.attributes.attributes = attributes;
        symbol.attributes.scope  = ScopeAttributes { 
                   .name  = *next_scope_name,
                   .depth = active_depth, // update later
                   .scope = active_scope  // update later
        };
        symbols_to_add->push_back(symbol);

    }

    visitChildren(context);
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/* compound_stmt : OPEN_CURLY local_declarations statement_list CLOSE_CURLY;
 */
antlrcpp::Any SymbolTableVisitor::visitCompound_stmt(CminusParser::Compound_stmtContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
    // We always enter a new scope on a compound statement. However, we may have queued up some 
    // symbols that need to be added such as from a function declaration. We add those to here 
    // after entering the new scope. Additionally, we make have defined a named scope. So we use
    // also.
    if (next_scope_name != nullptr) {
        enter_scope(*next_scope_name);

        if (symbols_to_add != nullptr && symbols_to_add->size() != 0) {
            for (auto symbol : *symbols_to_add) {
                std::shared_ptr<Symbol> symbol_ptr = std::make_shared<Symbol>(symbol);
                symbol_ptr->attributes.scope = ScopeAttributes {
                    .name = active_scope->get_scope_name(),
                    .depth = active_depth,
                    .scope = active_scope,
                };
                active_scope->add_symbol(symbol_ptr);
                report_debug("Adding " + *symbol_ptr->name + " to scope " + active_scope->get_scope_name(), __FUNCTION__, __LINE__);
            }
        }
        next_scope_name = nullptr;
        symbols_to_add = nullptr;

    } else {
        enter_scope();
    }
    visitChildren(context);
    exit_scope();
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/* local_declarations : var_declaration*;
 */
antlrcpp::Any SymbolTableVisitor::visitLocal_declarations(CminusParser::Local_declarationsContext *context) {
    visitChildren(context);
    return nullptr;
}

/* statement_list : statement*;
 */
antlrcpp::Any SymbolTableVisitor::visitStatement_list(CminusParser::Statement_listContext *context) {
    visitChildren(context);
    return nullptr;
}

/* statement : expression_stmt
             | compound_stmt 
             | selection_stmt 
             | iteration_stmt 
             | return_stmt
             | assignment_stmt;
*/
antlrcpp::Any SymbolTableVisitor::visitStatement(CminusParser::StatementContext *context) {
    visitChildren(context);
    return nullptr;
}

/* expression_stmt : expression SEMICOLON | SEMICOLON;
 */
antlrcpp::Any SymbolTableVisitor::visitExpression_stmt(CminusParser::Expression_stmtContext *context) {
    visitChildren(context);
    return nullptr;
}

/* assignment_stmt: var ASSIGN expression SEMICOLON;
 */
antlrcpp::Any SymbolTableVisitor::visitAssignment_stmt(CminusParser::Assignment_stmtContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);

    // Check whether the variable exists in the local scope or the global scope.
    std::string name = context->var()->ID()->getText();
    if(!active_scope->exists(name) && !global_scope->exists(name)) {
        report_error("Variable " + context->var()->getText() + " assignment before declaration!",__FUNCTION__,__LINE__);
    }

    visitChildren(context);
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/*selection_stmt : IF OPEN_PAREN expression CLOSE_PAREN statement
                   | IF OPEN_PAREN expression CLOSE_PAREN statement ELSE statement;
*/
antlrcpp::Any SymbolTableVisitor::visitSelection_stmt(CminusParser::Selection_stmtContext *context) {
    visitChildren(context);
    return nullptr;
}

/* iteration_stmt : WHILE OPEN_PAREN expression CLOSE_PAREN statement;
 */
antlrcpp::Any SymbolTableVisitor::visitIteration_stmt(CminusParser::Iteration_stmtContext *context) {
    visitChildren(context);
    return nullptr;
}

/* return_stmt : RETURN SEMICOLON
               | RETURN expression SEMICOLON;
*/
antlrcpp::Any SymbolTableVisitor::visitReturn_stmt(CminusParser::Return_stmtContext *context) {
    visitChildren(context);
    return nullptr;
}

/* expression : simple_expression;
 */
antlrcpp::Any SymbolTableVisitor::visitExpression(CminusParser::ExpressionContext *context) {
    visitChildren(context);
    return nullptr;
}

/* var : ID | ID OPEN_BRACKET expression CLOSE_BRACKET;
 */
antlrcpp::Any SymbolTableVisitor::visitVar(CminusParser::VarContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);
    std::string id = context->ID()->getText();

    if(!active_scope->exists(id) && !global_scope->exists(id)) {
        report_error("Varable " + context->ID()->getText() + " used before declaration!",__FUNCTION__,__LINE__);
    }

    visitChildren(context);
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/* simple_expression : additive_expression relop additive_expression | additive_expression; 
 */
antlrcpp::Any SymbolTableVisitor::visitSimple_expression(CminusParser::Simple_expressionContext *context) {
    visitChildren(context);
    return nullptr;
}

/* relop : LESS_EQUAL
         | LESS
         | GREATER 
         | GREATER_EQUAL 
         | EQUAL 
         | NOT_EQUAL;
*/
antlrcpp::Any SymbolTableVisitor::visitRelop(CminusParser::RelopContext *context) {
    visitChildren(context);
    return nullptr;
}

/* additive_expression : term (addop term)*;
 */
antlrcpp::Any SymbolTableVisitor::visitAdditive_expression(CminusParser::Additive_expressionContext *context) {
    visitChildren(context);
    return nullptr;
}

/* addop : ADD | SUBTRACT;
 */
antlrcpp::Any SymbolTableVisitor::visitAddop(CminusParser::AddopContext *context) {
    visitChildren(context);
    return nullptr;
}

/* term : factor ( mulop factor)*;
 */
antlrcpp::Any SymbolTableVisitor::visitTerm(CminusParser::TermContext *context) {
    visitChildren(context);
    return nullptr;
}

/* mulop : MULTIPLY | DIVIDE; 
 */
antlrcpp::Any SymbolTableVisitor::visitMulop(CminusParser::MulopContext *context) {
    visitChildren(context); return nullptr;
}

/* factor : OPEN_PAREN expression CLOSE_PAREN 
          | var 
          | call 
          | NUM;
*/
antlrcpp::Any SymbolTableVisitor::visitFactor(CminusParser::FactorContext *context) {
    visitChildren(context);
    return nullptr;
}

/* call : ID OPEN_PAREN args CLOSE_PAREN;
 */
antlrcpp::Any SymbolTableVisitor::visitCall(CminusParser::CallContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);

    std::string id = context->ID()->getText();
    if(!active_scope->exists(id) && !global_scope->exists(id)) {
        report_error("Function " + id + " called before declaration!",__FUNCTION__,__LINE__);
    }
    std::shared_ptr<Symbol> symbol;
    // See if this function is declared locally
    std::shared_ptr<Symbol> local_symbol = active_scope->get_symbol_local(context->ID()->getText());
    if (local_symbol != nullptr && local_symbol->attributes.kind != AttributeKind::Function) {
        report_error("Attempted to call " + id + " as a function!",__FUNCTION__,__LINE__);
    }

    visitChildren(context);
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}

/* args : arg_list | (); 
 */
antlrcpp::Any SymbolTableVisitor::visitArgs(CminusParser::ArgsContext *context) {
    visitChildren(context);
    return nullptr;
}

/* arg_list : expression (COMMA expression)*;
 */
antlrcpp::Any SymbolTableVisitor::visitArg_list(CminusParser::Arg_listContext *context) {
    visitChildren(context);
    return nullptr;
}

/* main_declaration: VOID MAIN OPEN_PAREN VOID CLOSE_PAREN compound_stmt;
 */
antlrcpp::Any SymbolTableVisitor::visitMain_declaration(CminusParser::Main_declarationContext *context) {
    report_info(__FUNCTION__, DepthDelta::Start);

    // Here we add a function declaration to the symbol table.
    std::shared_ptr<std::string> name = std::make_shared<std::string>(context->MAIN()->getText());
    std::shared_ptr<Symbol> symbol = std::make_shared<Symbol>();
    symbol->name = name;
    symbol->attributes.kind = AttributeKind::Function;


    // Check if we've already defined this symbol in the currently active scope. 
    // If so then we simply report and error and return.
    if (active_scope->exists(*name)) {
        report_error("Variable "  + *symbol->name + " redecalared in current scope",__FUNCTION__,__LINE__);

        visitChildren(context);
        return nullptr;
    }

    FunctionAttributes attributes = {
        .type = context->VOID(0)->getText(),
    };

    // `main` has a single void parameter which has no ID.
    FunctionParameter param = FunctionParameter {
                .type = context->VOID(1)->getText(),
                .id = "", 
                .is_array = false,
                
    };
    attributes.parameters.push_back(param);

    symbol->attributes.attributes = attributes;
    symbol->attributes.scope        = ScopeAttributes { 
               .name  = active_scope->get_scope_name(),
               .depth = active_depth,
               .scope = active_scope
    };
    active_scope->add_symbol(symbol);

    next_scope_name = std::make_shared<std::string>(context->MAIN()->getText());
    visitChildren(context);
    report_info(__FUNCTION__, DepthDelta::End);
    return nullptr;
}


