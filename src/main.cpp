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
    programState state;
    std::vector<Instr*> block;

    // just for now.
    int mainLocalSpace = 100;

    block.push_back(new Instr(InstrType::CALL, { x86operand(x86OperandTypes::LABEL, "main") }));
    block.push_back(new Instr(InstrType::INT3, {}));
    block.push_back(new Instr(InstrType::RET, {}));

    
    std::vector<Instr*> funcContent = generateCodeBlock(astRootNode, state);
    block.insert(block.end(), funcContent.begin(), funcContent.end());


   
    std::cout << "DONE]" << std::endl;



    std::string outputStr = displayMachineCode(block);

    std::ofstream MyFile("output.txt");
    MyFile << outputStr;
    MyFile.close();

    //std::cout << countNodes(cstRootNode) << std::endl;
    getchar();
    return 0; 
}