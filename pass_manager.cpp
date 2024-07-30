
// ANTLR HEADERS
#include <antlr4-common.h>
#include <memory>
#include "CminusLexer.h"
#include "CminusParser.h"
#include "symboltable.h"

// LOCAL HEADERS
//#include "codegen.h"
#include "pass_manager.h"
#include "symboltable.h"
#include "codegen.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/IPO/GlobalOpt.h"
#include "llvm/Transforms/IPO/DeadArgumentElimination.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
#include "llvm/Transforms//Scalar/InstSimplifyPass.h"

// NAMESPACES
using namespace antlr4;
using namespace std;
using AttributeTable = std::vector<Attribute>;

#define PRINT_ST 0
#define PRINT_IR 1
#define DO_PASSES 1


void InitializeModule() {
    // Open a new context and module.
    TheContext = std::make_unique<llvm::LLVMContext>();
    TheModule = std::make_unique<llvm::Module>("my cool jit", *TheContext);

    // Create a new builder for the module.
    Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);
}

void visitor_passes(CminusParser::ProgramContext *tree) {
    //LiteralTable literal_table = LiteralTable();
    std::shared_ptr<AttributeTable> attribute_table = std::make_shared<AttributeTable>();
    std::shared_ptr<SymbolTable> symbol_table = std::make_shared<SymbolTable>("global");

    SymbolTableVisitor visitor = SymbolTableVisitor(symbol_table, attribute_table);
    
    if (PRINT_ST) { std::cout << "Start, Symbol Table Creation" << std::endl; }
    visitor.visitProgram(tree);
    if (PRINT_ST) { std::cout << "End, Symbol Table Creation" << std::endl;
                    std::cout << std::endl;
                    std::cout<<"Symbol Table---------------\n"<<std::endl;
                    std::cout << symbol_table->to_string() << std::endl; 
                    std::cout<<"---------------------------\n"<<std::endl;
    }


    std::shared_ptr<IRGenerationVisitor> ir_generator = std::make_shared<IRGenerationVisitor>(symbol_table);
    ir_generator->visitProgram(tree);
    
    if (DO_PASSES) { 
        llvm::LoopAnalysisManager LAM;
        llvm::FunctionAnalysisManager FAM;
        llvm::CGSCCAnalysisManager CGAM;
        llvm::ModuleAnalysisManager MAM;
        
        llvm::PassBuilder PB;

        PB.registerModuleAnalyses(MAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerLoopAnalyses(LAM);
        PB.crossRegisterProxies(LAM, FAM , CGAM, MAM);

        llvm::ModulePassManager MPM;

        // GlobalOptPass
        // This pass transforms simple global variables that never have their address
        // taken.  If obviously true, it marks read/write globals as constant, deletes
        // variables only stored to, etc.
        MPM.addPass(llvm::GlobalOptPass());

        // DeadArgumentElemenationPass
        // This pass deletes dead arguments from internal functions.  Dead argument
        // elimination removes arguments which are directly dead, as well as arguments
        // only passed into function calls as dead arguments of other functions.  This
        // pass also deletes dead return values in a similar way.
        MPM.addPass(llvm::DeadArgumentEliminationPass());

        
        // Mem2Reg
        // This pass is a simple pass wrapper around the PromoteMemToReg function call
        // exposed by the Utils library.
        MPM.addPass(llvm::createModuleToFunctionPassAdaptor(llvm::PromotePass()));

        // InstSimplifyPass
        // Run instruction simplification across each instruction in the function.
        //
        // Instruction simplification has useful constraints in some contexts:
        // - It will never introduce *new* instructions.
        // - There is no need to iterate to a fixed point.
        MPM.addPass(llvm::createModuleToFunctionPassAdaptor(llvm::InstSimplifyPass()));

        MPM.run(*ir_generator->get_module(), MAM);
    }
    if (PRINT_IR) { std::cout << ir_generator->ir_to_string() << std::endl; }
}

