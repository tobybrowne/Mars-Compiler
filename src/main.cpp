#include <iostream>
#include <fstream>
#include <sstream>
#include <optional>
#include <vector>
#include <map>
#include <set>
#include <cstring>

// for now we're defining our source code as a variable - later we can parse it as a parameter to the compiler.
std::string source_code = "C://Users//toby//Documents//Mars//test.clite";
std::set<std::string> tokens = { "_if", "_else", "_while", "_return", "_break", "_int", "_ID", "_NUMCONST", "_lessthanequal", "_lessthan", "_greaterthan", "_greaterthanequal", "_equal", "_notequal", "_plus", "_subtract", "_multiply", "_divide", "_semi", "_opencurly", "_closecurly", "_openbracket", "_closebracket", "_assign", "_and", "_or", "_not", "E" };
std::map<std::string, std::vector<std::vector<std::string>>> patternList;

int inputPtr = 0;

// add position of the token in the code to the object
struct Token {
    std::string type;

    // only one can be true at a time so use a union to save memory?!?
    // using a union throws some stupid constructor errors that need to be resolved.
    std::string varName;
    int numConstVal;
};

// struct CstNode;

class CstNode {
    public:
        std::string val;
        std::vector<CstNode*> childrenNodes;
        bool tokenPresent = false;
        Token token;
};


// AST Nodes:

// the AST should be above everything, no reference to tokens, you should be able to understand the entire program
// with the AST alone.
// annoyingly, this technique does not provide type-checking.
// struct AstNode;

enum class Node {
    IF_NODE,
    WHILE_NODE,
    VAR_NODE,
    NUMCONST_NODE,
    EXPR_NODE,
    DECL_NODE,
    STMTSEQ_NODE
};

enum class Operand {
    VAR_NODE,
    EXPR_NODE,
    NUMCONST_NODE
};

enum class Statement {
    DECL_NODE,
    IF_NODE,
    WHILE_NODE,
    STMT_NODE
};

enum class OpCode {
    ADD,
    SUB,
    DIV,
    LE,
    LT,
    GE,
    GT
};

class VarNode {
    public:
        std::string name;
};

class NumConstNode {
    public:
        int val;
};

class ExprNode {
    public:
        OpCode opcode;
        Operand aType;
        Operand bType;
        union a {
            VarNode *var;
            ExprNode *expr;
            NumConstNode *num;
        } a;
        union b {
            VarNode *var;
            ExprNode *expr;
            NumConstNode *num;
        } b;
        ExprNode(OpCode opcode, Operand aType, Operand bType);
};

class DeclNode {
    public:
        VarNode variable;
        Operand initType;
        union { 
            VarNode var;
            ExprNode expr;
            NumConstNode num;
        } init;
};

class Stmt {
    public:
        Statement type;
        union node {
            struct {
                ExprNode condition;
                Stmt *ifBody;
                Stmt *elseBody;
            } ifNode;
            struct {
                ExprNode condition;
                Stmt *whileBody;
            } whileNode;
            std::vector<Stmt*> seq;
        };
};

ExprNode::ExprNode(OpCode opcode, Operand aType, Operand bType): opcode(opcode), aType(aType), bType(bType)
{
    switch (aType) {
        case Operand::VAR_NODE:
        {
            a.var = new VarNode;
            break;
        }
        case Operand::EXPR_NODE:
        {
            a.expr = new ExprNode;
            break;
        }
        case Operand::NUMCONST_NODE:
        {
            a.num = new NumConstNode;
            break;
        }
    }
    switch (bType) {
        case Operand::VAR_NODE:
        {
            b.var = new VarNode;
            break;
        }
        case Operand::EXPR_NODE:
        {
            b.expr = new ExprNode;
            break;
        }
        case Operand::NUMCONST_NODE:
        {
            b.num = new NumConstNode;
            break;
        }
    }
}

struct AstNode {
    Node type;
    union {
        //ifNode
        struct {
            AstNode* condition; // exprNode
            AstNode* ifBody; // stmtSeqNode
            AstNode* elseBody;
        };

        //whileNode
        struct {
            AstNode* condition;
            AstNode* body;
        };

        //varNode
        struct {
            // remember a variable has no attribute val!
            std::string name;
        };

        //numConstNode
        struct {
            int val;
        };

        //exprNode
        struct {
            // give operand node another go.
            AstNode* operand1;
            std::string opcode;
            AstNode* operand2;
        };

        //declNode
        struct {
            AstNode* variable;
            AstNode* init;
        };

        //stmtSeqNode
        struct {
            std::vector<AstNode*> stmts;
        };
    };

    // include type assignment
    AstNode(Node nodeTypeInput) {
        std::memset(this, 0, sizeof(AstNode));
        type = nodeTypeInput;
    }
};

AstNode* createExprNode(AstNode* operand1, AstNode* operand2, std::string opcode) {
    std::cout << "making expr node" << std::endl;
    AstNode* newExprNode = new AstNode(EXPR_NODE);
    newExprNode->operand1 = operand1;
    newExprNode->operand1 = operand2;
    newExprNode->opcode = opcode;
    return newExprNode;
}

AstNode* createNumConstNode(int a) {
    std::cout << "making num const" << std::endl;
    AstNode* newNumConstNode = new AstNode(NUMCONST_NODE);
    newNumConstNode->val = a;
    return newNumConstNode;
}

AstNode* createVarNode(std::string varName) {
    std::cout << "making var node" << std::endl;
    std::cout << varName << std::endl;
    AstNode* newVarNode = new AstNode(VAR_NODE);
    std::cout << "finished making var node" << std::endl;
    newVarNode->name = varName;
    std::cout << "finished making var node" << std::endl;
    return newVarNode;
}


// searches the names of a list of cstNodes and returns the correct pointer.
CstNode* findChildrenByName(std::vector<CstNode*> childNodes, std::string match) {
    for (CstNode*& child : childNodes) {
        if (child->val == match) {
            return child;
        }
    }
}

// tells you whether an expression in the expression grammar chain is actually in use (contains an operation).
// make sure this works with relExp.
bool expressionInUse(CstNode* cstExprNode) {
    std::string expressionType = cstExprNode->val;
    // checking the children of expression* (make sure this index is correct)
    if (((cstExprNode->childrenNodes)[1]->val).size() == 1) {
        return false;
    }
    else {
        return true;
    }
}

std::vector<Token> tokenStream;

std::string findOpcode(CstNode* cstExprNode) {
    if (cstExprNode->tokenPresent) {
        return findOpcode((cstExprNode->childrenNodes[0]));
    }
    return cstExprNode->val;
}

// converts CST simpleExp into a expression tree.
AstNode* addExprTreeToAST(CstNode* cstSimpleExp) {
    std::cout << "starting express tree stuff" << std::endl;
    std::string expressionType = cstSimpleExp->val;
    std::vector<CstNode*> childrenNodes = cstSimpleExp->childrenNodes;

    if (expressionType == "factor") {
        Token factorToken = childrenNodes[0]->token;
        if (factorToken.type == "_NUMCONST") {
            return createNumConstNode(factorToken.numConstVal); // numbers stored w strings _eeesh (use a union?)
        }
        else if (factorToken.type == "_ID") {
            return createVarNode(factorToken.varName); 
        }

    }

    std::cout << "non terminal" << std::endl;

    if (expressionInUse(cstSimpleExp)) {
        AstNode* operand1 = addExprTreeToAST(childrenNodes[0]);
        AstNode* operand2 = addExprTreeToAST(childrenNodes[1]->childrenNodes[1]);
        std::string opcode = findOpcode(childrenNodes[1]->childrenNodes[0]);
        AstNode* newExprNode = createExprNode(operand1, operand2, opcode);

        return newExprNode;
    }
    else {
        return addExprTreeToAST(childrenNodes[0]);
    }
}

// some nasty code is required to convert the recursive tree structure into a linear list of declarations.
std::vector<AstNode*> addDecListToAST(CstNode* decListCSTNode, std::vector<AstNode*> decASTNodeVector) {
    std::cout << "making dec list" << std::endl;
    std::vector<CstNode*> childrenNodes = decListCSTNode->childrenNodes;
    std::cout << childrenNodes.size() << std::endl;
    if (childrenNodes.size() == 1) { // at dead end...
        return {};
    }
    for (int i = 0; i < 2; i++) {
        if (i == 0) { // varDecl...
            std::cout << "making var decl" << std::endl;
            CstNode* varDeclNode = childrenNodes[0];
            Token idToken = (varDeclNode->childrenNodes)[1]->childrenNodes[0]->token;
            std::cout << "hola" << std::endl;

            CstNode* simpleExp = (varDeclNode->childrenNodes)[1]->childrenNodes[2];

            

            AstNode* newDeclNode = new AstNode(DECL_NODE);

            newDeclNode->variable = createVarNode(idToken.varName);

            std::cout << "hiya" << std::endl;


            newDeclNode->init = addExprTreeToAST(simpleExp);

            decASTNodeVector.push_back(newDeclNode);

            return decASTNodeVector;
        }
        else { // further decList...
            // proper nasty but required.
            std::vector<AstNode*> newDecs = addDecListToAST(childrenNodes[i], decASTNodeVector);
            decASTNodeVector.insert(decASTNodeVector.end(), newDecs.begin(), newDecs.end());
            return decASTNodeVector;

        }

    }
}

// converts CST expStmt to an expression node.
AstNode* addExprNodeToAST(CstNode* cstExprStmt) {
    // probably use a different name for childrenNodes.
    std::vector<CstNode*> childrenNodes = cstExprStmt->childrenNodes;

    if (childrenNodes.size() == 3) { // exp part of grammar...
        AstNode* varNode = createVarNode(childrenNodes[0]->childrenNodes[0]->val);
        AstNode* operand2 = addExprNodeToAST(childrenNodes[0]->childrenNodes[2]);
        return createExprNode(varNode, operand2, "_assign");
    }

    else if (childrenNodes[0]->val == "simpleExp") {
        return addExprTreeToAST(childrenNodes[0]);
    }
}

// forward definition required to manage mutual recursion.
std::vector<AstNode*> addStmtListToAST(CstNode* stmtList, std::vector<AstNode*> smtASTNodeVector);

// converts CST cmpdStmt to AST stmtSeqNode
AstNode* addStmtSeqNodeToAST(CstNode* cstCompoundStmt) {
    std::cout << "making statement sequence" << std::endl;
    std::vector<CstNode*> childrenNodes = cstCompoundStmt->childrenNodes;

    AstNode* newStmtSeqNode = new AstNode(STMTSEQ_NODE);

    for (int i = 0; i < 4; i++) {
        // decList
        if (i == 1) {
            newStmtSeqNode->stmts = addDecListToAST(childrenNodes[1], {});
            std::cout << "finished declist stuff" << std::endl;
        }
        // stmtList
        else if (i == 2) {
            std::cout << "starting stmtList stuff" << std::endl;
            newStmtSeqNode->stmts = addStmtListToAST(childrenNodes[i], {});
        }
    }

    return newStmtSeqNode;
}

AstNode* addWhileNodeToAST(CstNode* cstIterStmt) {
    std::vector<CstNode*> childrenNodes = cstIterStmt->childrenNodes;

    AstNode* newWhileNode = new AstNode(WHILE_NODE);

    // can this be done without for loops.
    for (int i = 0; i < childrenNodes.size(); i++) {
        if (i == 2) {
            newWhileNode->condition = addExprTreeToAST(childrenNodes[i]);
        }
        else if (i == 4) {
            newWhileNode->body = addStmtSeqNodeToAST(childrenNodes[i]);
        }
    }

    return newWhileNode;
}


// converts CST selectStmt to AST ifNode
AstNode* addIfNodeToAST(CstNode* cstSelectStmt) {
    std::vector<CstNode*> childrenNodes = cstSelectStmt->childrenNodes;

    AstNode* newIfNode = new AstNode(IF_NODE);

    for (int i = 0; i < childrenNodes.size(); i++) {
        if (i == 2) {
            newIfNode->condition = addExprTreeToAST(childrenNodes[i]);
        }
        else if (i == 4) {
            newIfNode->ifBody = addStmtSeqNodeToAST(childrenNodes[i]);
        }
        else if (i == 6) {
            newIfNode->elseBody = addStmtSeqNodeToAST(childrenNodes[i]);
        }
    }

    return newIfNode;
}

// deals with a stmt grammar expression.
AstNode* addStmtNodeToAST(CstNode* stmtNodeAST) {
    CstNode* specificStmt = stmtNodeAST->childrenNodes[0];
    std::string stmtType = specificStmt->val;

    // switch doesn't work with strings :(
    if (stmtType == "expStmt") {
        return addExprNodeToAST(specificStmt);
    }
    else if (stmtType == "compoundStmt") {
        return addStmtSeqNodeToAST(specificStmt);

    }
    else if (stmtType == "selectStmt") {
        return addIfNodeToAST(specificStmt);

    }
    else if (stmtType == "iterStmt") {
        return addWhileNodeToAST(specificStmt);

    }
    else if (stmtType == "returnStmt") {
        // add later...
    }
    else if (stmtType == "breakStmt") {
        // add later...
    }
}

// can this logic be merged with the previous function?
std::vector<AstNode*> addStmtListToAST(CstNode* stmtList, std::vector<AstNode*> smtASTNodeVector) {
    std::cout << "stmt List" << std::endl;
    std::vector<CstNode*> childrenNodes = stmtList->childrenNodes;
    if (childrenNodes.size() == 1) { // at dead end...
        return {};
    }
    for (int i = 0; i < 2; i++) {
        if (i == 0) { // stmtDecl...
            
            smtASTNodeVector.push_back(addStmtNodeToAST(childrenNodes[i]));
            return smtASTNodeVector;

        }
        else { // further stmtList...
            
            std::vector<AstNode*> newStmts = addStmtListToAST(childrenNodes[i], smtASTNodeVector);
            smtASTNodeVector.insert(smtASTNodeVector.end(), newStmts.begin(), newStmts.end());
            return smtASTNodeVector;
        }

    }
}


AstNode* createAST(CstNode* cstRootNode) {
    // currently first node is always a compoundStmt node.
    return addStmtSeqNodeToAST(cstRootNode);
}

CstNode* createCstNode(std::string e) {
    CstNode* tmp = new CstNode;
    tmp->val = e;
    tmp->childrenNodes = {};
    return tmp;
}

void defineLanguageGrammar() {
    patternList["stmt"] = { {"expStmt"}, {"compoundStmt"}, {"selectStmt"}, {"iterStmt"}, {"returnStmt"}, {"breakStmt"} };

    patternList["expStmt"] = { {"exp", "_semi"}, {"_semi"} };

    patternList["compoundStmt"] = { {"_opencurly", "decList", "stmtList", "_closecurly"} };
    patternList["decList"] = { {"varDecl", "decList"}, {"E"} };

    patternList["varDecl"] = { {"typeSpec", "varDeclInit", "_semi"} };
    patternList["varDeclInit"] = { {"_ID", "_assign", "simpleExp"}, {"_ID"} };
    patternList["typeSpec"] = { {"_int"} };

    patternList["stmtList"] = { {"stmt", "stmtList"}, {"E"} };
    patternList["selectStmt"] = { {"_if", "_openbracket", "simpleExp", "_closebracket", "compoundStmt"}, {"_if", "_openbracket", "simpleExp", "_closebracket", "compoundStmt", "_else", "compoundStmt"} };

    patternList["iterStmt"] = { {"_while", "_openbracket", "simpleExp", "_closebracket", "compoundStmt"} };
    patternList["returnStmt"] = { {"_return", "_semi"}, {"_return", "exp", "_semi"} };
    patternList["breakStmt"] = { {"_break", "_semi"} };

    patternList["exp"] = { {"_ID", "_assign", "exp"}, {"simpleExp"} };

    patternList["simpleExp"] = { {"andExp", "simpleExp*"} };
    patternList["simpleExp*"] = { {"and", "andExp", "simpleExp*"}, {"E"} };

    patternList["andExp"] = { {"unaryRelExp", "andExp*"} };
    patternList["andExp*"] = { {"_or", "unaryRelExp", "andExp*"}, {"E"} };

    patternList["unaryRelExp"] = { {"relExp"}, {"_not", "unaryRelExp"} };

    patternList["relExp"] = { {"sumExp", "relOp", "sumExp"}, {"sumExp"} };
    patternList["relOp"] = { {"_lessthanequal"}, {"_lessthan"}, {"_greaterthan"}, {"_greaterthanequal"}, {"_equal"}, {"_notequal"} };

    patternList["sumExp"] = { {"mulExp", "sumExp*"} };
    patternList["sumExp*"] = { {"sumOp", "mulExp", "sumExp*"}, {"E"} };
    patternList["sumOp"] = { {"_plus"}, {"_subtract"} };

    patternList["mulExp"] = { {"factor", "mulExp*"} };
    patternList["mulExp*"] = { {"mulOp", "fatcor", "mulExp*"}, {"E"} };
    patternList["mulOp"] = { {"_multiply"}, {"_divide"} };

    patternList["factor"] = { {"_NUMCONST"}, {"_ID"} };
}

void displayVector(std::vector<std::string> vectorToDisplay) {
    std::string outputString;
    for (int i = 0; i < vectorToDisplay.size(); i++) {
        outputString += vectorToDisplay[i];
        outputString += " ";
    }
    std::cout << outputString << std::endl;
}

int countNodes(CstNode* t) {
    if (t != NULL) {
        std::vector<CstNode*> children = t->childrenNodes;
        int childrenNodes = 0;
        for (int i = 0; i < children.size(); i++) {
            childrenNodes += countNodes(children[i]);
        }
        return 1 + childrenNodes;
    }
}

void print_tree(CstNode* t) {
    if (t != NULL) {
        std::vector<CstNode*> children = t->childrenNodes;
        for (int i = 0; i < children.size(); i++) {
            print_tree(children[i]);
        }
        std::cout << t->val << std::endl;
    }
}

// converts the provided source code string into a vector of tokens.
std::vector<Token> tokenize(const std::string source_code, bool debugMode) {
    std::vector<Token> tokenList;
    std::string previousToken;
    std::string tokenType;

    std::string numConstVal; // this has to be a string so it can be appended to.
    std::string varName;
    
    for (int i = 0; i < source_code.length(); i++) {

        if (source_code[i] == ' ' || source_code[i] == '\n') {
            continue;
        }
        else if (isdigit(source_code[i])) {
            numConstVal += source_code[i];
            if (previousToken != "_NUMCONST") {
                tokenType = "_NUMCONST";
            }
            else {
                continue;
            }
        }

        else if (source_code.substr(i, 3) == "if(" && (previousToken == "_semi" || previousToken == "_closecurly" || previousToken == "")) {
            i += 1;
            tokenType = "_if";
        }

        else if (source_code.substr(i, 5) == "else{" && (previousToken == "_closecurly")) {
            i += 3;
            tokenType = "_else";
        }

        else if (source_code.substr(i, 6) == "while(" && (previousToken == "_semi" || previousToken == "_closecurly" || previousToken == "")) {
            i += 4;
            tokenType = "_while";
        }

        else if ((source_code.substr(i, 7) == "return;" || source_code.substr(i, 7) == "return ") && (previousToken == "_semi" || previousToken == "_closecurly" || previousToken == "")) {
            i += 5;
            tokenType = "_return";
        }

        else if ((source_code.substr(i, 6) == "break;") && (previousToken == "_semi" || previousToken == "_closecurly" || previousToken == "")) {
            i += 4;
            tokenType = "_break";
        }

        else if ((source_code.substr(i, 4) == "int ") && (previousToken == "_semi" || previousToken == "_closecurly" || previousToken == "" || previousToken == "_opencurly")) {
            i += 3;
            tokenType = "_int";
        }

        else if (source_code.substr(i, 2) == "&&") {
            i += 1;
            tokenType = "_and";
        }

        else if (source_code.substr(i, 2) == "||") {
            i += 1;
            tokenType = "_or";
        }

        else if (source_code[i] == '<') {
            if (source_code[i + 1] == '=') {
                i += 1;
                tokenType = "_lessthanequal";
            }
            else {
                tokenType = "_lessthan";
            }
        }

        else if (source_code[i] == '>') {
            if (source_code[i + 1] == '=') {
                i += 1;
                tokenType = "_greaterthanequal";
            }
            else {
                tokenType = "_greaterthan";
            }
        }

        else if (source_code[i] == '=') {
            if (source_code[i + 1] == '=') {
                i += 1;
                tokenType = "_equal";
            }
            else {
                tokenType = "_assign";
            }
        }

        else if (source_code[i] == '!') {
            if (source_code[i + 1] == '=') {
                i += 1;
                tokenType = "_notequal";
            }
            else {
                tokenType = "_not";
            }
        }

        else if (source_code[i] == '+') {
            tokenType = "_plus";
        }

        else if (source_code[i] == '-') {
            tokenType = "_subtract";
        }

        else if (source_code[i] == '*') {
            tokenType = "_multiply";
        }

        else if (source_code[i] == '/') {
            tokenType = "_divide";
        }

        else if (source_code[i] == ';') {
            tokenType = "_semi";
        }

        else if (source_code[i] == '{') {
            tokenType = "_opencurly";
        }

        else if (source_code[i] == '}') {
            tokenType = "_closecurly";
        }

        else if (source_code[i] == '(') {
            tokenType = "_openbracket";
        }

        else if (source_code[i] == ')') {
            tokenType = "_closebracket";
        }

        else {
            varName += source_code[i];
            if (previousToken != "_ID") {
                tokenType = "_ID";
            }
            else {
                continue;
            }
        }

        if (tokenType != "_ID" && varName != "") {
            tokenList.back().varName = varName;
            varName = "";
        }

        if (tokenType != "_NUMCONST" && numConstVal != "") {
            tokenList.back().numConstVal = stoi(numConstVal); // i think this casting is unavoidable.
            numConstVal = "";
        }

        Token tokenObj;
        tokenObj.type = tokenType;
        tokenList.push_back(tokenObj);
        previousToken = tokenType;

    }

    if (debugMode == true) {
        for (int i = 0; i < tokenList.size(); i++) {
            //std::cout << tokenList[i].type << " : "<< tokenList[i].val <<std::endl;
            // needs re-writing to take values into account
        }
    }

    return tokenList;
}

// recursive function to validate a provided grammar pattern for the current point in the token stream.
// TO-DO: Probably rewrite without pointers, just put the object itself inside the vector.
bool validPattern(std::vector<std::string> pattern, CstNode* rootTreeNode, bool debugMode) {
    rootTreeNode->childrenNodes.clear();

    for (std::string &component : pattern) { // by reference is more efficient.
        CstNode* newTreeNode = createCstNode(component);
        rootTreeNode->childrenNodes.push_back(newTreeNode);

        // if not a token...
        if (tokens.find(component) == tokens.end()) {
            bool valid = false;

            for (std::vector<std::string> &possiblePattern : patternList[component]) {
                int savedInputPtr = inputPtr;
                if (validPattern(possiblePattern, newTreeNode, debugMode)) {
                    valid = true;
                    break;
                }
                else {
                    inputPtr = savedInputPtr;
                }
            }
            if (valid == false) {
                return false;
            }
        }
        else {
            newTreeNode->token = tokenStream[inputPtr];
            newTreeNode->tokenPresent = true;

            if (component == tokenStream[inputPtr].type) {
                inputPtr++;
            }
            else if (component == "E") {
                continue;
            }
            else {
                return false;
            }
        }
    }
    return true;
}

// returns the root node of the Concrete Syntax Tree or NULL in the event of a syntax error.
CstNode* generateCST(bool debugMode) {
    CstNode* rootNode = createCstNode("compoundStmt");

    if (!validPattern(patternList["compoundStmt"][0], rootNode, debugMode)) {
        return NULL;
    }
    return rootNode;
}




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
    tokenStream = tokenize(contents, true);
    std::cout << "DONE]" << std::endl;

    std::cout << "Generating CST... [";
    CstNode* cstRootNode = generateCST(debugMode);
    if (cstRootNode != NULL) {
        std::cout << "DONE]" << std::endl;
    }
    else {
        std::cout << "SYNTAX ERROR]" << std::endl;
    }


    std::cout << "Generating AST..." << std::endl;
    AstNode* astRootNode = createAST(cstRootNode);
    std::cout << "DONE" << std::endl;




    

    //std::cout << countNodes(cstRootNode) << std::endl;
    return 0; 
}

