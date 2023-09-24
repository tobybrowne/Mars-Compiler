#include <filesystem>
#include <iostream>
#include <fstream>
#include <sstream>
#include <optional>
#include <vector>
#include <map>
#include <set>
#include <cassert>
#include <variant>
#include <array>
#include <algorithm>
#include <cwctype>
#include <unordered_map>
#include "general.h"
#include "lexer.h"
#include "cst.h"
#include "ast.h"
#include "codegen.h"

// for now we're defining our source code as a variable - later we can parse it as a parameter to the compiler.
std::string source_code = "C:/Users/toby/Documents/Mars/test.clite";


// eventually we can place this data structure into a separate header file.

int main() {
    defineLanguageGrammar();

    bool debugMode = false;
    
    std::cout << "MARS Compiler Alpha       [By Toby Browne] \n" << std::endl;

    std::cout << "Reading Source Code... [";

    // reads source code file.
    std::string contents;{
        std::stringstream contents_stream;
        std::fstream input(source_code, std::ios::in);
        contents_stream << input.rdbuf();
        contents = contents_stream.str();
    } // file doesn't need to be closed because it is defined in it's own scope and has a pre-made destructor
    std::cout << "DONE]" << std::endl;


    std::cout << "Tokenizing Source Code... [";
    std::vector<Token> tokenStream = tokenize(contents, true);
    if (tokenStream.size() == 0) {
        std::cout << "LEXER ERROR]" << std::endl;
        getchar();
    }
    std::cout << "DONE]" << std::endl;

    
    std::cout << "Generating CST... [";
    CstNode* cstRootNode = generateCST(debugMode, tokenStream);
    if (cstRootNode != NULL) {
        std::cout << "DONE]" << std::endl;
    }
    else {
        std::cout << "SYNTAX ERROR]" << std::endl;
        
        getchar();
    }

    std::cout << "Generating AST... [";
    Stmt* astRootNode = createAST(cstRootNode);
    std::cout << "DONE]" << std::endl;

    std::cout << "Generating x86... [";
    std::vector<Instr*> block = generateCodeBlock(astRootNode);
    std::cout << "DONE]" << std::endl;



    std::cout << displayMachineCode(block) << std::endl;;

    //std::cout << countNodes(cstRootNode) << std::endl;
    getchar();
    return 0; 
}