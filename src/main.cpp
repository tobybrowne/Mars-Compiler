#include <iostream>
#include <fstream>
#include <sstream>
#include <optional>
#include <vector>
#include <map>
#include <set>
#include <cstring>
#include <cassert>

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

std::vector<Token> tokenStream;

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
    STMT_NODE,
    STMTSEQ_NODE
};

enum class OpCode {
    ADD,
    SUB,
    DIV,
    MUL,
    LE,
    LT,
    GE,
    GT,
    EQ,
    NEQ,
    AND,
    OR,
    NOT
};

class VarNode {
    public:
        std::string name;
        VarNode();
        VarNode(const std::string& varName);
};

VarNode::VarNode(const std::string& varName)
{
    name = std::string(varName);
}

VarNode::VarNode()
{
}

class NumConstNode {
    public:
        int val;
        NumConstNode();
        NumConstNode(int val);
};

NumConstNode::NumConstNode(int val) : val(val)
{

}

NumConstNode::NumConstNode()
{
}

// only used as a medium to return data from specific functions.
// this way the classes stay type-safe, only storing one of the three operand types, but the code is much nicer.


class ExprNode {
public:
    OpCode opcode;
    Operand aType;
    Operand bType;
    union a {
        VarNode* var;
        ExprNode* expr;
        NumConstNode* num;
    } a;
    union b {
        VarNode* var;
        ExprNode* expr;
        NumConstNode* num;
    } b;
    ExprNode();
    ExprNode(OpCode opcode, Operand aType, Operand bType);
    ~ExprNode();
};


ExprNode::ExprNode(OpCode opcode, Operand aType, Operand bType) : opcode(opcode), aType(aType), bType(bType)
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

ExprNode::ExprNode() {

}

ExprNode::~ExprNode()
{
    switch (aType) {
    case Operand::VAR_NODE:
    {
        delete a.var;
        break;
    }
    case Operand::EXPR_NODE:
    {
        delete a.expr;
        break;
    }
    case Operand::NUMCONST_NODE:
    {
        delete a.num;
        break;
    }
    }
    switch (bType) {
    case Operand::VAR_NODE:
    {
        delete b.var;
        break;
    }
    case Operand::EXPR_NODE:
    {
        delete b.expr;
        break;
    }
    case Operand::NUMCONST_NODE:
    {
        delete b.num;
        break;
    }
    }
}

class NumVarExpr {
    public:
        Operand type;
        union {
            NumConstNode* numconst;
            VarNode* var;
            ExprNode* expr;
        } data;
};





// it would only be worth splitting up the statements if we had plans for some sort of type checking.
class Stmt {
    public:
        Statement type;
        union {
            struct {
                ExprNode* condition;
                Stmt* ifBody;
                Stmt* elseBody;
            } ifNode;

            struct {
                ExprNode condition;
                Stmt *whileBody;
            } whileNode;

            struct {
                VarNode* variable;
                Operand initType;
                union {
                    VarNode* var;
                    ExprNode* expr;
                    NumConstNode* num;
                } init;
            } declNode;

            struct {
                std::vector<int> numbers;
                std::vector<Stmt*> stmts;
            } seqNode;
            
        };
        Stmt(Statement stmtType);
        ~Stmt();
};

Stmt::Stmt(Statement stmtType): type(stmtType) {
    switch (stmtType) {
        case Statement::STMTSEQ_NODE: {
            new (&seqNode.stmts) std::vector<Stmt*>();
            break;
        }
    }
}

Stmt::~Stmt() {
    switch (type) {
        case Statement::STMTSEQ_NODE: {
            delete (&seqNode.stmts);
            break;
        }
    }
}

// tells you whether an expression in the expression grammar chain is actually in use (contains an operation).
// make sure this works with relExp.
bool expressionInUse(CstNode* cstExprNode) {
    std::string expressionType = cstExprNode->val;
    // checking the children of expression* (make sure this index is correct)
    if (((cstExprNode->childrenNodes)[1])->childrenNodes.size() == 1) {
        return false;
    }
    else {
        return true;
    }
}

// this is required because sometimes the opcode has a parent node like "relOp", sometimes it does not.
OpCode findOpcode(CstNode* cstExprNode) {

    if (cstExprNode->tokenPresent) {
        std::string opCodeString = cstExprNode->val;
        if (opCodeString == "_lessthan") {
            return OpCode::LT;
        }
        else if (opCodeString == "_greaterthan") {
            return OpCode::GT;
        }
        else if (opCodeString == "_lessthanequal") {
            return OpCode::LE;
        }
        else if (opCodeString == "_greaterthanequal") {
            return OpCode::GE;
        }
        else if (opCodeString == "_equal") {
            return OpCode::EQ;
        }
        else if (opCodeString == "_notequal") {
            return OpCode::NEQ;
        }
        else if (opCodeString == "_plus") {
            return OpCode::ADD;
        }
        else if (opCodeString == "_subtract") {
            return OpCode::SUB;
        }
        else if (opCodeString == "_multiply") {
            return OpCode::MUL;
        }
        else if (opCodeString == "_divide") {
            return OpCode::DIV;
        }
        else if (opCodeString == "_and") {
            return OpCode::AND;
        }
        else if (opCodeString == "_or") {
            return OpCode::OR;
        }
        else if (opCodeString == "_not") {
            return OpCode::NOT;
        }
    }
    else {
        return findOpcode(cstExprNode->childrenNodes[0]);
    }
}

NumVarExpr* addExprTreeToAST(CstNode* cstExpr) {
    std::string expressionType = cstExpr->val;
    std::cout << expressionType << std::endl;
    std::vector<CstNode*> childrenNodes = cstExpr->childrenNodes;

    if (expressionType == "factor") {
        Token factorToken = childrenNodes[0]->token;
        std::string factorType = factorToken.type;

        std::cout << "here: " << factorType << std::endl;
        std::cout << "gutentag" << std::endl;

        if (factorType == "_NUMCONST") {
            NumVarExpr* newNumVarExpr = new NumVarExpr;
            newNumVarExpr->type = Operand::NUMCONST_NODE;
            newNumVarExpr->data.numconst = new NumConstNode(factorToken.numConstVal);
            std::cout << "heya" << std::endl;
            return newNumVarExpr;
        }
        else if (factorType == "_ID") {

            NumVarExpr* newNumVarExpr = new NumVarExpr;
            newNumVarExpr->type = Operand::VAR_NODE;
            newNumVarExpr->data.var = new VarNode(factorToken.varName);

            return newNumVarExpr;
        }

    }

    // need to figure out how a unaryRelExp will actually be made.
    else if (expressionType == "unaryRelExp" || expressionInUse(cstExpr)) { // has a different structure than others.
        NumVarExpr* operand1;
        NumVarExpr* operand2;
        OpCode opcode;

        // unaryRelExp case...
        if (expressionType == "unaryRelExp") {
            // if not in use...
            std::cout << childrenNodes.size() << std::endl;
            if (childrenNodes.size() == 1) {
                return addExprTreeToAST(childrenNodes[0]);
            }
            else {
                std::cout << "in use" << std::endl;
                operand1 = addExprTreeToAST(childrenNodes[1]);
                std::cout << "operand 1 done" << std::endl;
                operand2 = operand1;
                std::cout << "operand 2 done" << std::endl;

                opcode = findOpcode(childrenNodes[0]);
                std::cout << "found opcode" << std::endl;
            }
        }

        // normal in use expression case...
        else {
            std::cout << "in use" << std::endl;
            operand1 = addExprTreeToAST(childrenNodes[0]);
            std::cout << "operand 1 done" << std::endl;
            operand2 = addExprTreeToAST(childrenNodes[1]->childrenNodes[1]);
            std::cout << "operand 2 done" << std::endl;

            opcode = findOpcode(childrenNodes[1]->childrenNodes[0]);
            std::cout << "found opcode" << std::endl;
        }

        ExprNode* newExprNode = new ExprNode(opcode, operand1->type, operand2->type);

        // nodes don't link to NumVarExpr types - just the actual operand type. 
        switch (operand1->type) {
        case Operand::VAR_NODE: {
            newExprNode->a.var = operand1->data.var;
        }
        case Operand::EXPR_NODE: {
            newExprNode->a.expr = operand1->data.expr;
        }
        case Operand::NUMCONST_NODE: {
            newExprNode->a.num = operand1->data.numconst;
        }
        }

        switch (operand2->type) {
        case Operand::VAR_NODE: {
            newExprNode->b.var = operand2->data.var;
        }
        case Operand::EXPR_NODE: {
            newExprNode->b.expr = operand2->data.expr;
        }
        case Operand::NUMCONST_NODE: {
            newExprNode->b.num = operand2->data.numconst;
        }
        }

        NumVarExpr* newNumVarExpr = new NumVarExpr;
        newNumVarExpr->type = Operand::EXPR_NODE;
        newNumVarExpr->data.expr = newExprNode;

        return newNumVarExpr;

    }
    

    else {
        std::cout << "not in use" << std::endl;
        return addExprTreeToAST(childrenNodes[0]);
    }
}

// creates an AST expression tree from a CST expression node.
//ExprNode* createExprTreeAST(CstNode* cstExpr) {
//    // we can assume the expression is in use (it will only be called in this case).
//    std::vector<CstNode*> childrenNodes = cstExpr->childrenNodes;
//    CstNode* cstRootOperands[2] = { childrenNodes[0], childrenNodes[1]->childrenNodes[1] };
//    Operand operandTypes[2];
//    OpCode opcode = findOpcode(cstExpr);
//
//    NumConstNode* nums[2];
//    VarNode* vars[2];
//    ExprNode* exprs[2];
//
//    bool validNodeReached = false;
//
//    ExprNode* newExprNode = new ExprNode();
//
//    for (int i = 0; i < 2; i++) {
//        CstNode* cstOperand = cstRootOperands[i];
//        validNodeReached = false;
//        while (validNodeReached == false) {
//            if (cstOperand->val == "factor") {
//                std::string type = cstOperand->childrenNodes[0]->token.type;
//                if (type == "_NUMCONST") {
//                    operandTypes[i] = Operand::NUMCONST_NODE;
//                    nums[i] = new NumConstNode(cstOperand->childrenNodes[0]->token.numConstVal);
//                }
//                else if (type == "_ID") {
//                    operandTypes[i] = Operand::VAR_NODE;
//                    vars[i] = new VarNode(cstOperand->childrenNodes[0]->token.varName);
//                }
//                validNodeReached = true;
//            }
//            else if (expressionInUse(cstOperand)) {
//                exprs[i] = createExprTreeAST(cstOperand);
//                operandTypes[i] = Operand::EXPR_NODE;
//                validNodeReached = true;
//            } 
//        }
//    }
//
//    newExprNode = new ExprNode(opcode, operandTypes[0], operandTypes[1]);
//
//    for (int i = 0; i < 2; i++) {
//        switch (operandTypes[i]) {
//            case Operand::VAR_NODE: {
//                newExprNode->a.var = vars[i];
//            }
//            case Operand::EXPR_NODE: {
//                newExprNode->a.expr = exprs[i];
//            }
//            case Operand::NUMCONST_NODE: {
//                newExprNode->a.num = nums[i];
//            }
//        }
//    }
//
//    return newExprNode;
//}


//AstNode* addWhileNodeToAST(CstNode* cstIterStmt) {
//    std::vector<CstNode*> childrenNodes = cstIterStmt->childrenNodes;
//
//    AstNode* newWhileNode = new AstNode(WHILE_NODE);
//
//    // can this be done without for loops.
//    for (int i = 0; i < childrenNodes.size(); i++) {
//        if (i == 2) {
//            newWhileNode->condition = addExprTreeToAST(childrenNodes[i]);
//        }
//        else if (i == 4) {
//            newWhileNode->body = addStmtSeqNodeToAST(childrenNodes[i]);
//        }
//    }
//
//    return newWhileNode;
//}


// converts CST selectStmt to AST ifNode


// converts CST expStmt to an expression node.
//AstNode* addExprNodeToAST(CstNode* cstExprStmt) {
//    // probably use a different name for childrenNodes.
//    std::vector<CstNode*> childrenNodes = cstExprStmt->childrenNodes;
//
//    if (childrenNodes.size() == 3) { // exp part of grammar...
//        AstNode* varNode = createVarNode(childrenNodes[0]->childrenNodes[0]->val);
//        AstNode* operand2 = addExprNodeToAST(childrenNodes[0]->childrenNodes[2]);
//        return createExprNode(varNode, operand2, "_assign");
//    }
//
//    else if (childrenNodes[0]->val == "simpleExp") {
//        return addExprTreeToAST(childrenNodes[0]);
//    }
//}


std::vector<Stmt*> addDecListToAST(CstNode* decListCSTNode, std::vector<Stmt*> decASTNodeVector) {
    std::cout << "making dec list" << std::endl;
    std::vector<CstNode*> childrenNodes = decListCSTNode->childrenNodes;
    if (childrenNodes.size() == 1) { // at dead end...
        return decASTNodeVector;
    }
    for (int i = 0; i < 2; i++) {
        if (i == 0) { // varDecl...
            std::cout << "============ making var decl ============" << std::endl;
            CstNode* varDeclNode = childrenNodes[0];
            Token idToken = (varDeclNode->childrenNodes)[1]->childrenNodes[0]->token;

            CstNode* simpleExp = (varDeclNode->childrenNodes)[1]->childrenNodes[2];

            Stmt* newDeclNode = new Stmt(Statement::DECL_NODE);
            newDeclNode->declNode.variable = new VarNode(idToken.varName);

            NumVarExpr* init = addExprTreeToAST(simpleExp);
            Operand initType = init->type;

            newDeclNode->declNode.initType = initType;

            switch (initType) {
                case Operand::VAR_NODE: {
                    newDeclNode->declNode.init.var = init->data.var;
                }
                case Operand::EXPR_NODE: {
                    newDeclNode->declNode.init.expr = init->data.expr;
                }
                case Operand::NUMCONST_NODE: {
                    newDeclNode->declNode.init.num = init->data.numconst;
                }
            }

            decASTNodeVector.push_back(newDeclNode);

        }
        else { // further decList...
            decASTNodeVector = addDecListToAST(childrenNodes[i], decASTNodeVector);
            return decASTNodeVector;
        }
    }
}


std::vector<Stmt*> addStmtListToAST(CstNode* stmtList, std::vector<Stmt*> smtASTNodeVector);

// converts CST cmpdStmt to AST stmtSeqNode
Stmt* createStmtSeqNodeAST(CstNode* cstCompoundStmt) {
    std::cout << "making statement sequence" << std::endl;
    std::vector<CstNode*> childrenNodes = cstCompoundStmt->childrenNodes;

    Stmt* newStmtSeqNode = new Stmt(Statement::STMTSEQ_NODE);
    
    std::vector<Stmt*> decList;
    std::vector<Stmt*> stmtList;
   
    for (int i = 0; i < 4; i++) {
        // decList
        if (i == 1) {
            decList = addDecListToAST(childrenNodes[1], {});
            std::cout << decList.size() << std::endl;
            std::cout << "finished declist stuff" << std::endl;
        }
        // stmtList
        else if (i == 2) {
            std::cout << "starting stmtList stuff" << std::endl;
            stmtList = addStmtListToAST(childrenNodes[i], {});
            std::cout << "finish stmtList stuff" << std::endl;
        }
    }

    decList.insert(decList.end(), stmtList.begin(), stmtList.end());
    std::cout << decList.size() << std::endl;
    std::cout << "heeey" << std::endl;
    newStmtSeqNode->seqNode.stmts = decList;
    std::cout << "heeey1" << std::endl;

    return newStmtSeqNode;
}

Stmt* createIfNodeAST(CstNode* cstSelectStmt) {
    std::cout << "============= creating if node =============" << std::endl;
    std::vector<CstNode*> childrenNodes = cstSelectStmt->childrenNodes;

    Stmt* newIfNode = new Stmt(Statement::IF_NODE);

    for (int i = 0; i < childrenNodes.size(); i++) {
        if (i == 2) {
            // is this always an expression?
            newIfNode->ifNode.condition = addExprTreeToAST(childrenNodes[i])->data.expr;
        }
        else if (i == 4) {
            newIfNode->ifNode.ifBody = createStmtSeqNodeAST(childrenNodes[i]);
        }
        else if (i == 6) {
            newIfNode->ifNode.elseBody = createStmtSeqNodeAST(childrenNodes[i]);
        }
    }

    return newIfNode;
}



// deals with a stmt grammar expression.
Stmt* createStmtNodeAST(CstNode* stmtNodeAST) {
    CstNode* specificStmt = stmtNodeAST->childrenNodes[0];
    std::string stmtType = specificStmt->val;

    // switch doesn't work with strings :(
    if (stmtType == "expStmt") {
        // add later...
        /*return addExprNodeToAST(specificStmt);*/
    }
    else if (stmtType == "compoundStmt") {
        return createStmtSeqNodeAST(specificStmt);

    }
    else if (stmtType == "selectStmt") {
        return createIfNodeAST(specificStmt);

    }
    else if (stmtType == "iterStmt") {
        // add later...
        //return addWhileNodeToAST(specificStmt);

    }
    else if (stmtType == "returnStmt") {
        // add later...
    }
    else if (stmtType == "breakStmt") {
        // add later...
    }
}

// can this logic be merged with the previous function?
std::vector<Stmt*> addStmtListToAST(CstNode* stmtList, std::vector<Stmt*> smtASTNodeVector) {
    std::cout << "stmt List" << std::endl;
    std::cout << stmtList->val << std::endl;
    std::vector<CstNode*> childrenNodes = stmtList->childrenNodes;
    if (childrenNodes.size() == 1) { // at dead end...
        std::cout << childrenNodes[0]->val << std::endl;
        std::cout << "termination" << std::endl;
        return smtASTNodeVector;
    }
    for (int i = 0; i < 2; i++) {
        if (i == 0) { // stmtDecl...
            
            smtASTNodeVector.push_back(createStmtNodeAST(childrenNodes[i]));

        }
        else { // further stmtList...
            
            smtASTNodeVector = addStmtListToAST(childrenNodes[i], smtASTNodeVector);
            return smtASTNodeVector;
        }

    }
}


Stmt* createAST(CstNode* cstRootNode) {
    // currently first node is always a compoundStmt node.
    Stmt* rootNode = createStmtSeqNodeAST(cstRootNode);
    return rootNode;
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
    patternList["selectStmt"] = { {"_if", "_openbracket", "simpleExp", "_closebracket", "compoundStmt", "_else", "compoundStmt"}, {"_if", "_openbracket", "simpleExp", "_closebracket", "compoundStmt"},};

    patternList["iterStmt"] = { {"_while", "_openbracket", "simpleExp", "_closebracket", "compoundStmt"} };
    patternList["returnStmt"] = { {"_return", "_semi"}, {"_return", "exp", "_semi"} };
    patternList["breakStmt"] = { {"_break", "_semi"} };

    patternList["exp"] = { {"_ID", "_assign", "exp"}, {"simpleExp"} };

    patternList["simpleExp"] = { {"andExp", "simpleExp*"} };
    patternList["simpleExp*"] = { {"and", "andExp", "simpleExp*"}, {"E"} };

    patternList["andExp"] = { {"unaryRelExp", "andExp*"} };
    patternList["andExp*"] = { {"_or", "unaryRelExp", "andExp*"}, {"E"} };

    patternList["unaryRelExp"] = { {"relExp"}, {"_not", "unaryRelExp"} };

    patternList["relExp"] = { {"sumExp", "relExp*"}};
    patternList["relExp*"] = { {"relOp", "sumExp"}, {"E"}};
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

// tokenizer can't tolerate indentation atm.
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
            std::cout << tokenList[i].type;
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

    std::cout << contents << std::endl;


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
    Stmt* astRootNode = createAST(cstRootNode);
    std::cout << "DONE" << std::endl;

    getchar();

    //std::cout << countNodes(cstRootNode) << std::endl;
    return 0; 
}

