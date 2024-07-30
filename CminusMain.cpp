// CPP HEADERS
#include <iostream>
#include <fstream>


// ANTLR HEADERS
#include <antlr4-common.h>
#include "CminusLexer.h"
#include "CminusParser.h"
#include "symboltable.h"
#include "pass_manager.h"

// LOCAL HEADERS
//#include "codegen.h"

// NAMESPACES
using namespace antlrcpp;
using namespace antlr4;
using namespace std;
using namespace llvm;


int main(int argc, const char *args[])
{
    ifstream ins;
    ins.open(args[1]);  // Create the input stream.
    ANTLRInputStream input(ins);

    // Create a lexer which scans the input stream
    // to create a token stream.
    CminusLexer lexer(&input);
    CommonTokenStream tokens(&lexer);
   
    /*
    // Print the token stream.
    cout << "Tokens:" << endl;
    tokens.fill();
    */

    for (Token *token : tokens.getTokens()) {
        std::cout << token->toString() << std::endl;
    }

    /*
    // Create a parser which parses the token stream
    // to create a parse tree.
    CminusParser parser(&tokens);
    tree::ParseTree *tree = parser.program();
    */
    CminusParser parser(&tokens);
    parser.setBuildParseTree(true);
    CminusParser::ProgramContext *tree = parser.program();

    /*
    // Print the parse tree in Lisp format.
    cout << endl << "Parse tree (Lisp format):" << endl;
    std::cout << tree->toStringTree(&parser) << endl;
    */
    
    InitializeModule();
    visitor_passes(tree);


    return 0;
}
