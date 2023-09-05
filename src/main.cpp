#include <iostream>
#include <fstream>
#include <sstream>
#include <optional>
#include <vector>
#include <map>
#include <set>
#include <cstring>
#include <cassert>
#include <variant>



// for now we're defining our source code as a variable - later we can parse it as a parameter to the compiler.
std::string source_code = "C://Users//toby//Documents//Mars//test.clite";




enum class CstNonTerminal {
    STMT,
    EXP_STMT,
    COMPOUND_STMT,
    SELECT_STMT,
    ITER_STMT,
    RETURN_STMT,
    BREAK_STMT,
    EXP,
    DEC_LIST,
    STMT_LIST,
    VAR_DECL,
    TYPESPEC,
    VAR_DECL_INIT,
    SIMPLE_EXP,
    SIMPLE_EXP__,
    AND_EXP,
    AND_EXP__,
    UNARY_REL_EXP,
    REL_EXP,
    REL_EXP__,
    REL_OP,
    SUM_EXP,
    SUM_EXP__,
    SUM_OP,
    MUL_EXP,
    MUL_EXP__,
    MUL_OP,
    FACTOR,
    NULL_NONTERMINAL // to deal with undefined parameters.
};



// starting underscores indicate tokens, nothing else should have them.
enum class TokenType {
    _IF,
    _ELSE,
    _WHILE,
    _RETURN,
    _BREAK,
    _INT,
    _ID,
    _NUMCONST,
    _LE,
    _LT,
    _GT,
    _GE,
    _EQUAL,
    _NOTEQUAL,
    _ADD,
    _SUB,
    _MUL,
    _DIV,
    _SEMI,
    _OPENCURLY,
    _CLOSECURLY,
    _OPENBRACK,
    _CLOSEBRACK,
    _ASSIGN,
    _AND,
    _OR,
    _NOT,
    _E,
    NULL_TOKEN, // for undefined parameters
};


int inputPtr = 0;

// add position of the token in the code to the object
struct Token {
    TokenType type;

    // only one can be true at a time so use a union to save memory?!?
    // using a union throws some stupid constructor errors that need to be resolved.
    std::string varName;
    int numConstVal;
};

std::vector<Token> tokenStream;

class CstNode {
    public:
        union {
            CstNonTerminal nonTerm;
            TokenType token;
        } val;
        
        std::vector<CstNode*> childrenNodes;
        bool isToken = false;
        Token token;
};

std::map<CstNonTerminal, std::vector<std::vector<std::variant<TokenType, CstNonTerminal>>>> patternList;

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
    STMTSEQ_NODE,
    RET_NODE,
    BREAK_NODE,
};

// Don't conflate with token types, crossover but different.
enum class OpCode {
    ADD,
    SUB,
    DIV,
    MUL,
    LE,
    LT,
    GE,
    GT,
    EQUAL,
    NOTEQUAL,
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
    TokenType opcode; // use OpCode class instead for further validation?
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
    ExprNode(TokenType opcode, Operand aType, Operand bType);
    ~ExprNode();
};


ExprNode::ExprNode(TokenType opcode, Operand aType, Operand bType) : opcode(opcode), aType(aType), bType(bType)
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
                ExprNode* condition;
                Stmt* body;
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

            struct {
                bool operandPresent = false;
                union {
                    VarNode* var;
                    ExprNode* expr;
                    NumConstNode* num;
                } operand;
            } retNode;
            
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
    // checking the children of expression* (make sure this index is correct)
    if (((cstExprNode->childrenNodes)[1])->childrenNodes.size() == 1) {
        return false;
    }
    else {
        return true;
    }
}

// this is required because sometimes the opcode has a parent node like "relOp", sometimes it does not.
TokenType findOpcode(CstNode* cstExprNode) {

    if (cstExprNode->isToken) {
        TokenType opCodeToken = cstExprNode->val.token;
    }
    else {
        return findOpcode(cstExprNode->childrenNodes[0]);
    }
}

NumVarExpr* addExprTreeToAST(CstNode* cstExpr) {
    CstNonTerminal expressionType = cstExpr->val.nonTerm;
    std::vector<CstNode*> childrenNodes = cstExpr->childrenNodes;

    if (expressionType == CstNonTerminal::FACTOR) {
        Token factorToken = childrenNodes[0]->token;
        TokenType factorType = factorToken.type;


        if (factorType == TokenType::_NUMCONST) {
            NumVarExpr* newNumVarExpr = new NumVarExpr;
            newNumVarExpr->type = Operand::NUMCONST_NODE;
            newNumVarExpr->data.numconst = new NumConstNode(factorToken.numConstVal);
            std::cout << "heya" << std::endl;
            return newNumVarExpr;
        }
        else if (factorType == TokenType::_ID) {

            NumVarExpr* newNumVarExpr = new NumVarExpr;
            newNumVarExpr->type = Operand::VAR_NODE;
            newNumVarExpr->data.var = new VarNode(factorToken.varName);

            return newNumVarExpr;
        }

    }

    // need to figure out how a unaryRelExp will actually be made.
    else if (expressionType == CstNonTerminal::UNARY_REL_EXP || expressionInUse(cstExpr)) { // has a different structure than others.
        NumVarExpr* operand1;
        NumVarExpr* operand2;
        TokenType opcode;

        // unaryRelExp case...
        if (expressionType == CstNonTerminal::UNARY_REL_EXP) {
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

// creates AST if node from CST node.
Stmt* createIfNodeAST(CstNode* cstSelectStmt) {
    std::cout << "============= creating if node =============" << std::endl;
    std::vector<CstNode*> childrenNodes = cstSelectStmt->childrenNodes;

    Stmt* newIfNode = new Stmt(Statement::IF_NODE);


    // why is this in a for loop? just use the indexes directly.
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

// creates AST while node from CST node.
Stmt* createWhileNodeToAST(CstNode* cstIterStmt) {
    std::cout << "============= creating while node =============" << std::endl;
    std::vector<CstNode*> childrenNodes = cstIterStmt->childrenNodes;

    Stmt* newWhileNode = new Stmt(Statement::WHILE_NODE);

    newWhileNode->whileNode.condition = addExprTreeToAST(childrenNodes[2])->data.expr;
    newWhileNode->whileNode.body = createStmtSeqNodeAST(childrenNodes[4]);

    return newWhileNode;
}

// creates AST return node from CST node.
Stmt* createReturnNodeAST(CstNode* cstRetStmt) {
    std::cout << "============= creating return node =============" << std::endl;
    std::vector<CstNode*> childrenNodes = cstRetStmt->childrenNodes;

    Stmt* newRetNode = new Stmt(Statement::RET_NODE);

    if (childrenNodes.size() == 3) {
        newRetNode->retNode.operandPresent = true;

        NumVarExpr* operand = addExprTreeToAST(childrenNodes[1]);

        switch (operand->type) {
            case Operand::VAR_NODE: {
                newRetNode->retNode.operand.var = operand->data.var;
            }
            case Operand::EXPR_NODE: {
                newRetNode->retNode.operand.expr = operand->data.expr;
            }
            case Operand::NUMCONST_NODE: {
                newRetNode->retNode.operand.num = operand->data.numconst;
            }
        }

    }
    return newRetNode;
}

// creates AST break node from CST node.
Stmt* createBreakNodeAST(CstNode* cstIterStmt) {
    std::cout << "============= creating break node =============" << std::endl;
    Stmt* newBreakNode = new Stmt(Statement::BREAK_NODE);
    return newBreakNode;
}



// deals with a stmt grammar expression.
Stmt* createStmtNodeAST(CstNode* stmtNodeAST) {
    CstNode* specificStmt = stmtNodeAST->childrenNodes[0];
    CstNonTerminal stmtType = specificStmt->val.nonTerm;

    switch (stmtType) {
        case(CstNonTerminal::EXP_STMT):
            // add later...
            /*return addExprNodeToAST(specificStmt);*/

        case(CstNonTerminal::COMPOUND_STMT):
            return createStmtSeqNodeAST(specificStmt);

        case(CstNonTerminal::SELECT_STMT):
            return createIfNodeAST(specificStmt);

        case(CstNonTerminal::ITER_STMT):
            return createWhileNodeToAST(specificStmt);

        case(CstNonTerminal::RETURN_STMT):
            return createReturnNodeAST(specificStmt);

        case(CstNonTerminal::BREAK_STMT):
            return createBreakNodeAST(specificStmt);
    }
}

// can this logic be merged with the previous function?
std::vector<Stmt*> addStmtListToAST(CstNode* stmtList, std::vector<Stmt*> smtASTNodeVector) {
    std::cout << "stmt List" << std::endl;
    std::vector<CstNode*> childrenNodes = stmtList->childrenNodes;
    if (childrenNodes.size() == 1) { // at dead end...
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

CstNode* createCstNode(std::variant<TokenType, CstNonTerminal> e) {
    CstNode* tmp = new CstNode;

    // if token...
    if (std::holds_alternative<TokenType>(e)) {
        tmp->isToken = true;
        tmp->val.token = std::get<TokenType>(e);
    }
    // if non terminal..
    else {
        tmp->isToken = false;
        tmp->val.nonTerm = std::get<CstNonTerminal>(e);
    }


    tmp->childrenNodes = {};
    return tmp;
}



void defineLanguageGrammar() {
    patternList[CstNonTerminal::STMT] = { {CstNonTerminal::EXP_STMT}, {CstNonTerminal::COMPOUND_STMT}, {CstNonTerminal::SELECT_STMT}, {CstNonTerminal::ITER_STMT}, {CstNonTerminal::RETURN_STMT}, {CstNonTerminal::BREAK_STMT} };

    patternList[CstNonTerminal::EXP_STMT] = { {CstNonTerminal::EXP, TokenType::_SEMI}, {TokenType::_SEMI} };

    patternList[CstNonTerminal::COMPOUND_STMT] = { {TokenType::_OPENCURLY, CstNonTerminal::DEC_LIST, CstNonTerminal::STMT_LIST, TokenType::_CLOSECURLY} };
    patternList[CstNonTerminal::DEC_LIST] = { {CstNonTerminal::VAR_DECL, CstNonTerminal::DEC_LIST}, {TokenType::_E} };

    patternList[CstNonTerminal::VAR_DECL] = { {CstNonTerminal::TYPESPEC, CstNonTerminal::VAR_DECL_INIT, TokenType::_SEMI} };
    patternList[CstNonTerminal::VAR_DECL_INIT] = { {TokenType::_ID, TokenType::_ASSIGN, CstNonTerminal::SIMPLE_EXP}, {TokenType::_ID} };
    patternList[CstNonTerminal::TYPESPEC] = { {TokenType::_INT} };

    patternList[CstNonTerminal::STMT_LIST] = { {CstNonTerminal::STMT, CstNonTerminal::STMT_LIST}, {TokenType::_E} };
    patternList[CstNonTerminal::SELECT_STMT] = { {TokenType::_IF, TokenType::_OPENBRACK, CstNonTerminal::SIMPLE_EXP, TokenType::_CLOSEBRACK, CstNonTerminal::COMPOUND_STMT, TokenType::_ELSE, CstNonTerminal::COMPOUND_STMT}, {TokenType::_IF, TokenType::_OPENBRACK, CstNonTerminal::SIMPLE_EXP, TokenType::_CLOSEBRACK, CstNonTerminal::COMPOUND_STMT},};

    patternList[CstNonTerminal::ITER_STMT] = { {TokenType::_WHILE, TokenType::_OPENBRACK, CstNonTerminal::SIMPLE_EXP, TokenType::_CLOSEBRACK, CstNonTerminal::COMPOUND_STMT} };
    
    // original definitions had exp instead of simpleExp, idk why though.
    patternList[CstNonTerminal::RETURN_STMT] = { {TokenType::_RETURN, TokenType::_SEMI}, {TokenType::_RETURN, CstNonTerminal::SIMPLE_EXP, TokenType::_SEMI} };

    patternList[CstNonTerminal::BREAK_STMT] = { {TokenType::_BREAK, TokenType::_SEMI} };

    patternList[CstNonTerminal::EXP] = { {TokenType::_ID, TokenType::_ASSIGN, CstNonTerminal::EXP}, {CstNonTerminal::SIMPLE_EXP} };

    patternList[CstNonTerminal::SIMPLE_EXP] = { {CstNonTerminal::AND_EXP, CstNonTerminal::SIMPLE_EXP__} };
    patternList[CstNonTerminal::SIMPLE_EXP__] = { {TokenType::_AND, CstNonTerminal::AND_EXP, CstNonTerminal::SIMPLE_EXP__}, {TokenType::_E} };

    patternList[CstNonTerminal::AND_EXP] = { {CstNonTerminal::UNARY_REL_EXP, CstNonTerminal::AND_EXP__} };
    patternList[CstNonTerminal::AND_EXP__] = { {TokenType::_OR, CstNonTerminal::UNARY_REL_EXP, CstNonTerminal::AND_EXP__}, {TokenType::_E} };

    patternList[CstNonTerminal::UNARY_REL_EXP] = { {CstNonTerminal::REL_EXP}, {TokenType::_NOT, CstNonTerminal::UNARY_REL_EXP} };

    patternList[CstNonTerminal::REL_EXP] = { {CstNonTerminal::SUM_EXP, CstNonTerminal::REL_EXP__}};
    patternList[CstNonTerminal::REL_EXP__] = { {CstNonTerminal::REL_OP, CstNonTerminal::SUM_EXP}, {TokenType::_E}};
    patternList[CstNonTerminal::REL_OP] = { {TokenType::_LE}, {TokenType::_LT}, {TokenType::_GT}, {TokenType::_GE}, {TokenType::_EQUAL}, {TokenType::_NOTEQUAL} };

    patternList[CstNonTerminal::SUM_EXP] = { {CstNonTerminal::MUL_EXP, CstNonTerminal::SUM_EXP__} };
    patternList[CstNonTerminal::SUM_EXP__] = { {CstNonTerminal::SUM_OP, CstNonTerminal::MUL_EXP, CstNonTerminal::SUM_EXP__}, {TokenType::_E} };
    patternList[CstNonTerminal::SUM_OP] = { {TokenType::_ADD}, {TokenType::_SUB} };

    patternList[CstNonTerminal::MUL_EXP] = { {CstNonTerminal::FACTOR, CstNonTerminal::MUL_EXP__} };
    patternList[CstNonTerminal::MUL_EXP__] = { {CstNonTerminal::MUL_OP, CstNonTerminal::FACTOR, CstNonTerminal::MUL_EXP__}, {TokenType::_E} };
    patternList[CstNonTerminal::MUL_OP] = { {TokenType::_MUL}, {TokenType::_DIV} };

    patternList[CstNonTerminal::FACTOR] = { {TokenType::_NUMCONST}, {TokenType::_ID} };
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


// tokenizer can't tolerate indentation atm.
// converts the provided source code string into a vector of tokens.
std::vector<Token> tokenize(const std::string source_code, bool debugMode) {
    std::vector<Token> tokenList;
    TokenType prevType = TokenType::_E;
    TokenType type;

    std::string numConstVal; // this has to be a string so it can be appended to.
    std::string varName;
    
    for (int i = 0; i < source_code.length(); i++) {

        if (source_code[i] == ' ' || source_code[i] == '\n') {
            continue;
        }
        else if (isdigit(source_code[i])) {
            numConstVal += source_code[i];
            if (prevType != TokenType::_NUMCONST) {
                type = TokenType::_NUMCONST;
            }
            else {
                continue;
            }
        }

        else if (source_code.substr(i, 3) == "if(" && (prevType == TokenType::_SEMI || prevType == TokenType::_CLOSECURLY || prevType == TokenType::_E)) {
            i += 1;
            type = TokenType::_IF;
        }
        
        else if (source_code.substr(i, 5) == "else{" && (prevType == TokenType::_CLOSECURLY)) {
            i += 3;
            type = TokenType::_ELSE;
        }

        else if (source_code.substr(i, 6) == "while(" && (prevType == TokenType::_SEMI || prevType == TokenType::_CLOSECURLY || prevType == TokenType::_E)) {
            i += 4;
            type = TokenType::_WHILE;
        }

        else if ((source_code.substr(i, 7) == "return;" || source_code.substr(i, 7) == "return ") && (prevType == TokenType::_SEMI || prevType == TokenType::_CLOSECURLY || prevType == TokenType::_E)) {
            i += 5;
            type = TokenType::_RETURN;
        }

        else if ((source_code.substr(i, 6) == "break;") && (prevType == TokenType::_SEMI || prevType == TokenType::_CLOSECURLY || prevType == TokenType::_E)) {
            i += 4;
            type = TokenType::_BREAK;
        }

        else if ((source_code.substr(i, 4) == "int ") && (prevType == TokenType::_SEMI || prevType == TokenType::_CLOSECURLY || prevType == TokenType::_E || prevType == TokenType::_OPENCURLY)) {
            i += 3;
            type = TokenType::_INT;
        }

        else if (source_code.substr(i, 2) == "&&") {
            i += 1;
            type = TokenType::_AND;
        }

        else if (source_code.substr(i, 2) == "||") {
            i += 1;
            type = TokenType::_OR;
        }

        else if (source_code[i] == '<') {
            if (source_code[i + 1] == '=') {
                i += 1;
                type = TokenType::_LE;
            }
            else {
                type = TokenType::_LT;
            }
        }

        else if (source_code[i] == '>') {
            if (source_code[i + 1] == '=') {
                i += 1;
                type = TokenType::_GE;
            }
            else {
                type = TokenType::_GT;
            }
        }

        else if (source_code[i] == '=') {
            if (source_code[i + 1] == '=') {
                i += 1;
                type = TokenType::_EQUAL;
            }
            else {
                type = TokenType::_ASSIGN;
            }
        }

        else if (source_code[i] == '!') {
            if (source_code[i + 1] == '=') {
                i += 1;
                type = TokenType::_NOTEQUAL;
            }
            else {
                type = TokenType::_NOT;
            }
        }

        else if (source_code[i] == '+') {
            type = TokenType::_ADD;
        }

        else if (source_code[i] == '-') {
            type = TokenType::_SUB;
        }

        else if (source_code[i] == '*') {
            type = TokenType::_MUL;
        }

        else if (source_code[i] == '/') {
            type = TokenType::_DIV;
        }

        else if (source_code[i] == ';') {
            type = TokenType::_SEMI;
        }

        else if (source_code[i] == '{') {
            type = TokenType::_OPENCURLY;
        }

        else if (source_code[i] == '}') {
            type = TokenType::_CLOSECURLY;
        }

        else if (source_code[i] == '(') {
            type = TokenType::_OPENBRACK;
        }

        else if (source_code[i] == ')') {
            type = TokenType::_CLOSEBRACK;
        }

        else {
            varName += source_code[i];
            if (prevType != TokenType::_ID) {
                type = TokenType::_ID;
            }
            else {
                continue;
            }
        }

        if (type != TokenType::_ID && varName != "") {
            tokenList.back().varName = varName;
            varName = "";
        }

        if (type != TokenType::_NUMCONST && numConstVal != "") {
            tokenList.back().numConstVal = stoi(numConstVal); // i think this casting is unavoidable.
            numConstVal = "";
        }

        Token tokenObj;
        tokenObj.type = type;
        tokenList.push_back(tokenObj);
        prevType = type;

    }

    return tokenList;
}

// recursive function to validate a provided grammar pattern for the current point in the token stream.
// TO-DO: Probably rewrite without pointers, just put the object itself inside the vector.
bool validPattern(std::vector<std::variant<TokenType, CstNonTerminal>> pattern, CstNode* rootTreeNode, bool debugMode) {
    rootTreeNode->childrenNodes.clear();
    

    for (std::variant<TokenType,CstNonTerminal>&component : pattern) { // by reference is more efficient.
        
        CstNode* newTreeNode = createCstNode(component);
        rootTreeNode->childrenNodes.push_back(newTreeNode);

        // if a non terminal...
        if (!newTreeNode->isToken) {
            bool valid = false;

            for (std::vector<std::variant<TokenType, CstNonTerminal>>&possiblePattern : patternList[std::get<CstNonTerminal>(component)]) {
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

        // if a token...
        else {
            newTreeNode->token = tokenStream[inputPtr];
            newTreeNode->isToken = true;

            if (std::get<TokenType>(component) == tokenStream[inputPtr].type) {
                inputPtr++;
            }
            else if (std::get<TokenType>(component) == TokenType::_E) {
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
    CstNode* rootNode = createCstNode(CstNonTerminal::COMPOUND_STMT);

    if (!validPattern(patternList[CstNonTerminal::COMPOUND_STMT][0], rootNode, debugMode)) {
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

