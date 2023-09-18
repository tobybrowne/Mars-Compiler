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

// for now we're defining our source code as a variable - later we can parse it as a parameter to the compiler.
std::string source_code = "C:/Users/toby/Documents/Mars/test.clite";

enum class InstrType {
    LDR,
    STR,
    ADD,
    MUL,
    MOV,
    SUB,
    DIV
};

std::unordered_map<InstrType, std::string> instrString = {
    {InstrType::LDR, "LDR"},
    {InstrType::STR, "STR"},
    {InstrType::MOV, "MOV"},
    {InstrType::ADD, "ADD"},
    {InstrType::MUL, "MUL"},
    {InstrType::DIV, "DIV"},
    {InstrType::SUB, "SUB"},
};

enum class Register {
    RAX,
    RBX,
};

std::unordered_map<Register, std::string> regString = {
    {Register::RAX, "RAX"},
    {Register::RBX, "RBX"}
};

enum class x86OperandTypes {
    REGISTER,
    IMMEDIATE,
    STACK_OFFSET,
    EMPTY
};

class x86operand {
public:
    x86OperandTypes type;
    union {
        Register reg;
        int imm;
        int offset;
    };

    // default prevents linker errors.
    x86operand() = default;

    x86operand(x86OperandTypes inpType, std::variant<Register, int> inpData) {
        type = inpType;
        if (inpType == x86OperandTypes::REGISTER) {
            reg = std::get<Register>(inpData);
        }
        else if (inpType == x86OperandTypes::IMMEDIATE) {
            imm = std::get<int>(inpData);
        }
        else {
            offset = std::get<int>(inpData);
        }
    };

    x86operand(x86operand& t)  
    {
        type = t.type;
        if (type == x86OperandTypes::REGISTER) {
            reg = t.reg;
        }
        else if (type == x86OperandTypes::IMMEDIATE) {
            imm = t.imm;
        }
        else {
            offset = t.offset;
        }
    };
};


class Instr {
    public:
        InstrType type;
        x86operand op1;
        x86operand op2;
        std::optional<x86operand> op3; // can be null or operand

        Instr(InstrType inpType, x86operand inpOp1, x86operand inpOp2, std::optional<x86operand> inOp3 = std::nullopt) {
            type = inpType;
            op1 = inpOp1;
            op2 = inpOp2;
            if (inOp3) {
                op3 = inOp3.value();
            }
            
        };


};


enum class Datatype {
    INT
};

enum class ClassType {
    FUNCTION,
    VARIABLE
};

class TableEntry {
public:
    std::string name;
    ClassType classType;
    Datatype dataType; // holds the return type or var type.

    union {
        struct {
            int frameOffset;
        } varData;

        struct {
            int memRequired;
        } funcData;
    };

};

class SymbolTable {
    public:
        SymbolTable* upperTable;
        int currFrameOffset = 0;
        std::vector<TableEntry*> entries;
};





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
    FACTOR
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
    _E
};

struct Token{
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

        CstNode(std::variant<TokenType, CstNonTerminal> e);
        ~CstNode();
};

CstNode::~CstNode()
{
    // delete children...
    for(CstNode* childNode : childrenNodes){
        delete childNode;
    }
}

CstNode::CstNode(std::variant<TokenType, CstNonTerminal> e) {
    // if token...
    if (std::holds_alternative<TokenType>(e)) {
        isToken = true;
        val.token = std::get<TokenType>(e);
    }
    // if non terminal..
    else {
        isToken = false;
        val.nonTerm = std::get<CstNonTerminal>(e);
    }
}

std::map<CstNonTerminal, std::vector<std::vector<std::variant<TokenType, CstNonTerminal>>>> patternList;

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
    ASSIGN_NODE,
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
        TableEntry* tableEntry;
        VarNode();
        VarNode(const std::string& varName, TableEntry* tableEnt);
};

VarNode::VarNode(const std::string& varName, TableEntry* tableEnt)
{
    name = std::string(varName);
    tableEntry = tableEnt;
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

// forward reference required.
class ExprNode;

class NumVarExpr {
    public:
        Operand type;
        union {
            NumConstNode* numconst;
            VarNode* var;
            ExprNode* expr;
        } data;

        ~NumVarExpr();
};




class ExprNode {
    public:
        TokenType opcode; // use OpCode class instead for further validation?
        Operand aType;
        Operand bType;
        NumVarExpr* a;
        NumVarExpr* b;

        ExprNode();
        ExprNode(TokenType opcode, Operand aType, Operand bType);
        ~ExprNode();
};


ExprNode::ExprNode(TokenType opcode, Operand aType, Operand bType) : opcode(opcode), aType(aType), bType(bType)
{
    

}

ExprNode::ExprNode() {

}

ExprNode::~ExprNode()
{
    delete a;
    delete b;
}

NumVarExpr::~NumVarExpr() {
    switch (type) {
        case(Operand::VAR_NODE): {
            std::cout << "delete var" << std::endl;
            delete data.var;
            break;
        }
        case(Operand::EXPR_NODE): {
            std::cout << "delete expr" << std::endl;
            delete data.expr;
            break;
        }
        case(Operand::NUMCONST_NODE): {
            std::cout << "delete num" << std::endl;
            delete data.numconst;
            break;
        }
    }
}





// it would only be worth splitting up the statements if we had plans for some sort of type checking.
class Stmt {
    public:
        Statement type;
        union {
            struct {
                bool elsePresent = false;
                NumVarExpr* condition;
                Stmt* ifBody;
                Stmt* elseBody;
            } ifNode;

            struct {
                NumVarExpr* condition;
                Stmt* body;
            } whileNode;

            struct {
                VarNode* variable;
                NumVarExpr* init;
            } declNode;

            struct {
                VarNode* variable;
                bool furtherAssign;
                union { 
                    NumVarExpr* exprTree; 
                    Stmt* assignNode;
                } init;
            } assignNode;

            struct {
                std::vector<Stmt*> stmts;
            } seqNode;

            struct {
                bool operandPresent = false;
                Operand operandType;
                NumVarExpr* operand;
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
        case(Statement::IF_NODE): {
            std::cout << "if node destr." << std::endl;
            delete ifNode.condition;
            delete ifNode.ifBody;
            if (ifNode.elsePresent == true) {
                delete ifNode.elseBody;
            }
            break;
        }
        case(Statement::WHILE_NODE): {
            std::cout << "while node destr." << std::endl;
            delete whileNode.condition;
            delete whileNode.body;
            break;
        }
        case(Statement::DECL_NODE): {
            std::cout << "decl node destr." << std::endl;
            delete declNode.variable;
            delete declNode.init;
            break;
        }
        case(Statement::STMTSEQ_NODE): {
            std::cout << "stmt seq destr." << std::endl;
            std::vector<Stmt*> stmtVector = seqNode.stmts;
            std::cout << "VECTOR SIZE: " << seqNode.stmts.size() << std::endl;

            for (int i = 0; i < seqNode.stmts.size(); i++) {
                delete seqNode.stmts[i];
            }
            break;
        }
        case(Statement::RET_NODE): {
            std::cout << "return node destr." << std::endl;
            if (retNode.operandPresent) {
                delete retNode.operand;
            }
            break;
        }

    }
       
}

std::string displayMachineCode(std::vector<Instr*> program) {
    std::string programString;
    for (Instr* instruction : program) {
        std::string instructStr = "\n";

        instructStr += instrString[instruction->type];

        switch (instruction->op1.type) {
            case x86OperandTypes::REGISTER:
                instructStr += " " + regString[instruction->op1.reg];
                break;

            case x86OperandTypes::IMMEDIATE:
                // need to cast?
                instructStr += " " + std::to_string(instruction->op1.imm);
                break;

            case x86OperandTypes::STACK_OFFSET:
                instructStr += " sp+" + std::to_string(instruction->op1.offset);
                break;

        }

        switch (instruction->op2.type) {
            case x86OperandTypes::REGISTER:
                instructStr += ", " + regString[instruction->op2.reg];
                break;

            case x86OperandTypes::IMMEDIATE:
                // need to cast?
                instructStr += ", " + std::to_string(instruction->op2.imm);
                break;

            case x86OperandTypes::STACK_OFFSET:
                instructStr += ", sp+" + std::to_string(instruction->op2.offset);
                break;
        }

        // if operand 3 exists
        if (instruction->op3) {
            switch (instruction->op3.value().type) {
                case x86OperandTypes::REGISTER:
                    instructStr += ", " + regString[instruction->op3.value().reg];
                    break;

                case x86OperandTypes::IMMEDIATE:
                    // need to cast?
                    instructStr += ", " + std::to_string(instruction->op3.value().imm);
                    break;

                case x86OperandTypes::STACK_OFFSET:
                    instructStr += ", sp+" + std::to_string(instruction->op3.value().offset);
                    break;
                }
        }

        programString += instructStr;
        
    }

    return programString;
}

// generates code to calculate the result of an expression tree and store it in R2.
std::vector<Instr*> generateExprCode(ExprNode* exprTree, std::vector<Instr*> block, Register retReg) {
    Instr* newInstr1;
    Instr* newInstr2;

    bool instr1present = true;
    bool instr2present = true;

    // loads correct data into RAX and RBX
    switch (exprTree->aType) {
        case Operand::EXPR_NODE: {
            instr1present = false;
            std::vector<Instr*> newInstrs = generateExprCode(exprTree->a->data.expr, block, Register::RAX);
            block.insert(block.end(), newInstrs.begin(), newInstrs.end());
            break;
        }
            
        case Operand::VAR_NODE: {
            x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
            x86operand op2 = x86operand(x86OperandTypes::STACK_OFFSET, exprTree->a->data.var->tableEntry->varData.frameOffset);
            newInstr1 = new Instr(InstrType::LDR, op1, op2);
            break;
        }
            
        // should be able to get rid of this extra move and set the numconst as an operand
        case Operand::NUMCONST_NODE: {
            x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
            x86operand op2 = x86operand(x86OperandTypes::IMMEDIATE, exprTree->a->data.numconst->val);
            newInstr1 = new Instr(InstrType::MOV, op1, op2);
            break;
        }
            
    }

    switch (exprTree->bType) {
        case Operand::EXPR_NODE: {
            instr2present = false;
            std::vector<Instr*> newInstrs = generateExprCode(exprTree->b->data.expr, block, Register::RBX);
            block.insert(block.end(), newInstrs.begin(), newInstrs.end());
            break;
        }
            
        case Operand::VAR_NODE: {
            x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
            x86operand op2 = x86operand(x86OperandTypes::STACK_OFFSET, exprTree->b->data.var->tableEntry->varData.frameOffset);
            newInstr2 = new Instr(InstrType::LDR, op1, op2);
            break;
        }
            
        case Operand::NUMCONST_NODE: {
            x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
            x86operand op2 = x86operand(x86OperandTypes::IMMEDIATE, exprTree->b->data.numconst->val);
            newInstr2 = new Instr(InstrType::MOV, op1, op2);
            break;
        }
            
    }

    if (instr1present == true) {
        block.push_back(newInstr1);
    }
    if (instr2present == true) {
        block.push_back(newInstr2);
    }
    


    // actual operation


    // implementing multiplication is a pain - 64 bit x 64 bit == 128 bit
    TokenType opcode = exprTree->opcode;
    InstrType type;

    switch (opcode) {
        case TokenType::_ADD:
            type = InstrType::ADD;
            break;
        case TokenType::_SUB:
            type = InstrType::SUB;
            break;
        case TokenType::_MUL:
            type = InstrType::MUL;
            break;
        case TokenType::_DIV:
            type = InstrType::DIV;
            break;
    }

    x86operand op1 = x86operand(x86OperandTypes::REGISTER, retReg);
    x86operand op2 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
    x86operand op3 = x86operand(x86OperandTypes::REGISTER, Register::RBX);

    Instr* newInstr = new Instr(type, op1, op2, op3);
    block.push_back(newInstr);
    return block;
}



// tells you whether an expression in the expression grammar chain is actually in use (contains an operation).
bool expressionInUse(CstNode* cstExprNode) {
    // unaryRelExp...
    if (cstExprNode->val.nonTerm == CstNonTerminal::UNARY_REL_EXP) {
        if (cstExprNode->childrenNodes.size() == 1) {
            return false;
        }
        return true;
    }
    // everything else...
    else {
        if (((cstExprNode->childrenNodes)[1])->childrenNodes.size() == 1) {
            return false;
        }
        return true;
    }  
}

TableEntry* getTblEntry(SymbolTable* currentTable, std::string varName) {
    for (TableEntry* entry : currentTable->entries) {
        if (entry->classType == ClassType::VARIABLE && entry->name == varName) {
            return entry;
        }
    }

    // if not found in current table.
    if (currentTable->upperTable == NULL) {
        return NULL;
    }
    else {
        return getTblEntry(currentTable->upperTable, varName);
    }
}

// this is required because sometimes the opcode has a parent node like "relOp", sometimes it does not.
TokenType findOpcode(CstNode* cstExprNode) {
    if (cstExprNode->isToken) {
        return cstExprNode->val.token;
    }
    else {
        return findOpcode(cstExprNode->childrenNodes[0]);
    }
}

// converts CST expression tree into an AST expression tree.
NumVarExpr* createExprTreeAST(CstNode* cstExpr, SymbolTable* symbolTable) {
    CstNonTerminal expressionType = cstExpr->val.nonTerm;
    std::vector<CstNode*> childrenNodes = cstExpr->childrenNodes;

    if (expressionType == CstNonTerminal::FACTOR) {
        Token factorToken = childrenNodes[0]->token;
        TokenType factorType = factorToken.type;
        NumVarExpr* newNumVarExpr = new NumVarExpr;

        if (factorType == TokenType::_NUMCONST) {
            newNumVarExpr->type = Operand::NUMCONST_NODE;
            newNumVarExpr->data.numconst = new NumConstNode(factorToken.numConstVal);
        }
        else if (factorType == TokenType::_ID) {
            newNumVarExpr->type = Operand::VAR_NODE;
            TableEntry* tblEntry = getTblEntry(symbolTable, factorToken.varName);

            if (tblEntry == NULL) {
                std::cout << "VARIABLE " << factorToken.varName << " NOT DECLARED!]" << std::endl;
                getchar();
            }

            newNumVarExpr->data.var = new VarNode(factorToken.varName, tblEntry); // fix this.
        }
        return newNumVarExpr;
    }

    // if in use
    else if (expressionInUse(cstExpr)) {
        NumVarExpr* operand1;
        NumVarExpr* operand2;
        TokenType opcode;

        if (expressionType == CstNonTerminal::UNARY_REL_EXP) {
            operand1 = createExprTreeAST(childrenNodes[1], symbolTable);
            operand2 = operand1;
            opcode = findOpcode(childrenNodes[0]);
        }
        else {
            operand1 = createExprTreeAST(childrenNodes[0], symbolTable);
            operand2 = createExprTreeAST(childrenNodes[1]->childrenNodes[1], symbolTable);
            opcode = findOpcode(childrenNodes[1]->childrenNodes[0]);
        }

        ExprNode* newExprNode = new ExprNode(opcode, operand1->type, operand2->type);
        newExprNode->a = operand1;
        newExprNode->b = operand2;

        // creates expression node and returns it.
        NumVarExpr* newNumVarExpr = new NumVarExpr;
        newNumVarExpr->type = Operand::EXPR_NODE;
        newNumVarExpr->data.expr = newExprNode;

        return newNumVarExpr;
    }
    
    // not in use...
    else {
        return createExprTreeAST(childrenNodes[0], symbolTable);
    } 
}


// converts CST expStmt to an expression node.
Stmt* createAssignNodeAST(CstNode* exp, SymbolTable* symbolTable) { 
    // probably use a different name for childrenNodes.
    if (exp->childrenNodes.size() == 1) {
        return NULL;
    }

    Stmt* newAssignNode = new Stmt(Statement::ASSIGN_NODE);
    std::string varName = exp->childrenNodes[0]->token.varName;
    TableEntry* tblEntry = getTblEntry(symbolTable, varName);

    if (tblEntry == NULL) {
        std::cout << "VARIABLE " << varName << " NOT DECLARED!]" << std::endl;
        getchar();
    }

    newAssignNode->assignNode.variable = new VarNode(varName, tblEntry); // fix this
    CstNode* furtherExp = exp->childrenNodes[2];
    // further assign...
    int a = 3;
    if (furtherExp->childrenNodes.size() == 3) {
        int a = 3;
        newAssignNode->assignNode.furtherAssign = true;
        newAssignNode->assignNode.init.assignNode = createAssignNodeAST(furtherExp, symbolTable);
    }
    // simpleExp...
    else {
        std::cout << "create simple exp" << std::endl;
        newAssignNode->assignNode.furtherAssign = false;
        newAssignNode->assignNode.init.exprTree = createExprTreeAST(furtherExp->childrenNodes[0], symbolTable);
    }
    
    return newAssignNode;
}

bool varDeclaredInScope(SymbolTable* currentTable, std::string varName) {
    for (TableEntry* entry : currentTable->entries) {
        if (entry->classType == ClassType::VARIABLE && entry->name == varName) {
            return true;
        }
    }
    return false;
}


// creates a variable declaration node.
Stmt* createVarDeclNodeAST(CstNode* varDeclNode, SymbolTable* symbolTable) {
    Stmt* newDeclNode = new Stmt(Statement::DECL_NODE);

    // create varNode...
    std::string varName = (varDeclNode->childrenNodes)[1]->childrenNodes[0]->token.varName;

    if (varDeclaredInScope(symbolTable, varName)) {
        std::cout << "VARIABLE "<< varName << " DECLARED TWICE!]" << std::endl;
        getchar();
    }


    TokenType dataTypeToken = (varDeclNode->childrenNodes)[0]->childrenNodes[0]->token.type;
    Datatype dataType;

    switch (dataTypeToken) {
        case TokenType::_INT:
            dataType = Datatype::INT;
    }

    // this must be calculated before the table entry is made.
    NumVarExpr* init = createExprTreeAST((varDeclNode->childrenNodes)[1]->childrenNodes[2], symbolTable);

    // add to symbol table...
    TableEntry* newSymTblEntry = new TableEntry;
    newSymTblEntry->name = varName;
    newSymTblEntry->classType = ClassType::VARIABLE;
    newSymTblEntry->dataType = dataType;
    newSymTblEntry->varData.frameOffset = symbolTable->currFrameOffset + 1;
    symbolTable->entries.push_back(newSymTblEntry);

    // increment frame offset
    symbolTable->currFrameOffset++;

    newDeclNode->declNode.variable = new VarNode(varName, newSymTblEntry);
    
    newDeclNode->declNode.init = init;

    return newDeclNode;
}

// generates a vector of declerations from the recursive CST decl structure.
std::vector<Stmt*> createDeclListAST(CstNode* decListCSTNode, std::vector<Stmt*> decASTNodeVector, SymbolTable* symbolTable) {
    std::vector<CstNode*> childrenNodes = decListCSTNode->childrenNodes;

    if (childrenNodes.size() == 1) { // at dead end...
        return decASTNodeVector;
    }

    decASTNodeVector.push_back(createVarDeclNodeAST(childrenNodes[0], symbolTable)); // manages varDecl...

    decASTNodeVector = createDeclListAST(childrenNodes[1], decASTNodeVector, symbolTable); // manages further decList
    return decASTNodeVector;

}

// forward declaration required.
std::vector<Stmt*> createStmtListAST(CstNode* stmtList, std::vector<Stmt*> smtASTNodeVector, SymbolTable* symbolTable);

// converts CST cmpdStmt to AST stmtSeqNode
// this is the only time a new scope is entered (very nice).
Stmt* createStmtSeqNodeAST(CstNode* cstCompoundStmt, SymbolTable* symbolTable) {
    std::vector<Stmt*> decList;
    std::vector<Stmt*> stmtList;
    std::vector<CstNode*> childrenNodes = cstCompoundStmt->childrenNodes;

    // defines new symbol table for new scope
    SymbolTable* innerSymbolTable = new SymbolTable;
    innerSymbolTable->upperTable = symbolTable;
    innerSymbolTable->currFrameOffset = symbolTable->currFrameOffset;

    Stmt* newStmtSeqNode = new Stmt(Statement::STMTSEQ_NODE);
    
    // manages decList...
    decList = createDeclListAST(childrenNodes[1], {}, innerSymbolTable);

    // manages stmtList...
    stmtList = createStmtListAST(childrenNodes[2], {}, innerSymbolTable);
   
    decList.insert(decList.end(), stmtList.begin(), stmtList.end()); // merges decList and stmtList
    newStmtSeqNode->seqNode.stmts = decList;

    return newStmtSeqNode;
}

// creates AST if node from CST node.
Stmt* createIfNodeAST(CstNode* cstSelectStmt, SymbolTable* symbolTable) {
    std::vector<CstNode*> childrenNodes = cstSelectStmt->childrenNodes;

    Stmt* newIfNode = new Stmt(Statement::IF_NODE);

    newIfNode->ifNode.condition = createExprTreeAST(childrenNodes[2], symbolTable); // is this always an expression?

    newIfNode->ifNode.ifBody = createStmtSeqNodeAST(childrenNodes[4], symbolTable);

    // if else statement is present...
    if (childrenNodes.size() == 7) {
        newIfNode->ifNode.elsePresent = true;
        newIfNode->ifNode.elseBody = createStmtSeqNodeAST(childrenNodes[6], symbolTable);
    }

    return newIfNode;
}

// creates AST while node from CST node.
Stmt* createWhileNodeToAST(CstNode* cstIterStmt, SymbolTable* symbolTable) {
    std::vector<CstNode*> childrenNodes = cstIterStmt->childrenNodes;

    Stmt* newWhileNode = new Stmt(Statement::WHILE_NODE);
    newWhileNode->whileNode.condition = createExprTreeAST(childrenNodes[2], symbolTable);
    newWhileNode->whileNode.body = createStmtSeqNodeAST(childrenNodes[4], symbolTable);

    return newWhileNode;
}

// creates AST return node from CST node.
Stmt* createReturnNodeAST(CstNode* cstRetStmt, SymbolTable* symbolTable) {
    std::vector<CstNode*> childrenNodes = cstRetStmt->childrenNodes;

    Stmt* newRetNode = new Stmt(Statement::RET_NODE);

    if (childrenNodes.size() == 3) { // if return statement has operand.
        newRetNode->retNode.operandPresent = true;

        NumVarExpr* operand = createExprTreeAST(childrenNodes[1], symbolTable);
        newRetNode->retNode.operand = operand;
        newRetNode->retNode.operandType = operand->type;
    }
    return newRetNode;
}

// creates AST break node from CST node.
Stmt* createBreakNodeAST(CstNode* cstIterStmt){
    return new Stmt(Statement::BREAK_NODE);;
}

// takes a "stmt" non terminal and creates the correct stmt node.;
Stmt* createStmtNodeAST(CstNode* stmtNodeAST, SymbolTable* symbolTable) {
    CstNode* specificStmt = stmtNodeAST->childrenNodes[0];
    CstNonTerminal stmtType = specificStmt->val.nonTerm;

   
    switch (stmtType) {
        case(CstNonTerminal::EXP_STMT):
            if (specificStmt->childrenNodes.size() == 1) {
                std::cout << "hola" << std::endl;
                return NULL;
            }
            return createAssignNodeAST(specificStmt->childrenNodes[0], symbolTable);
            
        case(CstNonTerminal::COMPOUND_STMT):
            return createStmtSeqNodeAST(specificStmt, symbolTable);

        case(CstNonTerminal::SELECT_STMT):
            return createIfNodeAST(specificStmt, symbolTable);

        case(CstNonTerminal::ITER_STMT):
            return createWhileNodeToAST(specificStmt, symbolTable);

        case(CstNonTerminal::RETURN_STMT):
            return createReturnNodeAST(specificStmt, symbolTable);

        case(CstNonTerminal::BREAK_STMT):
            return createBreakNodeAST(specificStmt);
    }
}

// generates a vector of stmts from the recursive CST stmt structure.
std::vector<Stmt*> createStmtListAST(CstNode* stmtList, std::vector<Stmt*> smtASTNodeVector, SymbolTable* symbolTable) {
    std::vector<CstNode*> childrenNodes = stmtList->childrenNodes;

    if (childrenNodes.size() == 1) { // at dead end...
        return smtASTNodeVector;
    }
    else {
        Stmt* newStmtNode = createStmtNodeAST(childrenNodes[0], symbolTable);
        if (newStmtNode != NULL) {
            smtASTNodeVector.push_back(newStmtNode);
        }
        smtASTNodeVector = createStmtListAST(childrenNodes[1], smtASTNodeVector, symbolTable); // manages furtherstmtDecl
        return smtASTNodeVector;
    }
}

// generate AST and returns pointer to root node.
Stmt* createAST(CstNode* cstRootNode) {
    // currently first node is always a compoundStmt node.
    // starts in the global scope.
    SymbolTable* globalTable = new SymbolTable();
    Stmt* astRootNode = createStmtSeqNodeAST(cstRootNode, globalTable);

    std::cout << "finish" << std::endl;

    return astRootNode;
}

// eventually we can place this data structure into a separate header file.
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

bool isNumber(std::string s){
    for (char ch : s) {
        if (std::isdigit(ch) == 0)
            return false;
    }
    return true;
}


// tokenizer can't tolerate indentation atm.
// converts the provided source code string into a vector of tokens.
std::vector<Token> tokenize(const std::string source_code, bool debugMode) {
    std::vector<Token> tokenList;
    TokenType currentType;
    bool numConst;
    std::string alphanumeric;

    for (int i = 0; i < source_code.length(); i++) {
        
        // ignores whitespace, new lines and indentation.
        if (source_code[i] == ' ' || source_code[i] == '\n' || source_code[i] == '\t') {
            continue;
        }

        // tokenizing punctuation
        else if (source_code.substr(i, 2) == "&&") {
        i += 1;
        currentType = TokenType::_AND;
        }

        else if (source_code.substr(i, 2) == "||") {
        i += 1;
        currentType = TokenType::_OR;
        }

        else if (source_code[i] == '<') {
        if (source_code[i + 1] == '=') {
            i += 1;
            currentType = TokenType::_LE;
        }
        else {
            currentType = TokenType::_LT;
        }
        }

        else if (source_code[i] == '>') {
        if (source_code[i + 1] == '=') {
            i += 1;
            currentType = TokenType::_GE;
        }
        else {
            currentType = TokenType::_GT;
        }
        }

        else if (source_code[i] == '=') {
            if (source_code[i + 1] == '=') {
                i += 1;
                currentType = TokenType::_EQUAL;
            }
            else {
                currentType = TokenType::_ASSIGN;
            }
        }

        else if (source_code[i] == '!') {
            if (source_code[i + 1] == '=') {
                i += 1;
                currentType = TokenType::_NOTEQUAL;
            }
            else {
                currentType = TokenType::_NOT;
            }
        }

        else if (source_code[i] == '+') {
        currentType = TokenType::_ADD;
        }

        else if (source_code[i] == '-') {
        currentType = TokenType::_SUB;
        }

        else if (source_code[i] == '*') {
        currentType = TokenType::_MUL;
        }

        else if (source_code[i] == '/') {
        currentType = TokenType::_DIV;
        }

        else if (source_code[i] == ';') {
        currentType = TokenType::_SEMI;
        }

        else if (source_code[i] == '{') {
        currentType = TokenType::_OPENCURLY;
        }

        else if (source_code[i] == '}') {
        currentType = TokenType::_CLOSECURLY;
        }

        else if (source_code[i] == '(') {
        currentType = TokenType::_OPENBRACK;
        }

        else if (source_code[i] == ')') {
        currentType = TokenType::_CLOSEBRACK;
        }

        // if not punctuation...
        else if (iswalnum(source_code[i]) || source_code[i] == '_') {
            numConst = false;
            alphanumeric = "";
            
            if (std::isdigit(source_code[i])) {
                numConst = true;
            }
            
            // while still a var or numconst
            while (iswalnum(source_code[i]) || source_code[i] == '_') {
                // var starting with number not allowed.
                if (std::isdigit(source_code[i]) == false && numConst == true) {
                    // lexer error
                    return {};
                }
                alphanumeric += source_code[i];
                i++;
            }
            i--; // delimimter still needs to be processed.

            if (numConst == true) {
                currentType = TokenType::_NUMCONST;
            }

            else {
                // if keyword
                if (alphanumeric == "if") {
                    currentType = TokenType::_IF;
                }
                else if (alphanumeric == "else") {
                    currentType = TokenType::_ELSE;
                }
                else if (alphanumeric == "while") {
                    currentType = TokenType::_WHILE;
                }
                else if (alphanumeric == "return") {
                    currentType = TokenType::_RETURN;
                }
                else if (alphanumeric == "break") {
                    currentType = TokenType::_BREAK;
                }
                else if (alphanumeric == "int") {
                    currentType = TokenType::_INT;
                }
                // if id.
                else {
                    currentType = TokenType::_ID;
                }
            }
        }

        else {
            return {};
        }

        // creating token
        Token tokenObj;
        tokenObj.type = currentType;

        if (currentType == TokenType::_ID) {
            tokenObj.varName = alphanumeric;
        }
        else if (currentType == TokenType::_NUMCONST) {
            tokenObj.numConstVal = stoi(alphanumeric);
        }

        tokenList.push_back(tokenObj);
    }

    return tokenList;
}

// recursive function to validate a provided grammar pattern for the current point in the token stream.
// CONSIDER: Rewrite without pointers, just put the object itself inside the vector.
//inputPtr passed by reference gives a "global" effect.
bool validPattern(std::vector<std::variant<TokenType, CstNonTerminal>> pattern, CstNode* rootTreeNode, size_t& inputTokenPtr) {
    size_t savedPtr;
    bool valid;

    for (std::variant<TokenType,CstNonTerminal>&component : pattern) { // by reference is more efficient.
        CstNode* newTreeNode = new CstNode(component);
        rootTreeNode->childrenNodes.push_back(newTreeNode);

        // if a non terminal...
        if (!newTreeNode->isToken) {
            valid = false;

            //check if component is valid
            for (std::vector<std::variant<TokenType, CstNonTerminal>>&possiblePattern : patternList[std::get<CstNonTerminal>(component)]) {
                savedPtr = inputTokenPtr;
                if (validPattern(possiblePattern, newTreeNode, inputTokenPtr)) {
                    valid = true;
                    break;
                }
                else {
                    // reset pointer and deallocate the added children.
                    inputTokenPtr = savedPtr;
                    for (CstNode* childNode : newTreeNode->childrenNodes) {
                        delete childNode; // will recursively delete their childen too.
                    }
                    newTreeNode->childrenNodes.clear();
                }
            }

            // if component is invalid...
            if (valid == false) {
                return false;
            }
        }

        // if a token...
        else {
            newTreeNode->token = tokenStream[inputTokenPtr];
            
            if (std::get<TokenType>(component) == tokenStream[inputTokenPtr].type) { // correct token...
                inputTokenPtr++;
            }
            else if (std::get<TokenType>(component) == TokenType::_E) { // empty token...
                continue;
            }
            else { // incorrect token...
                return false;
            }
        }
    }
    return true;
}

// returns the root nwode of the Concrete Syntax Tree or NULL in the event of a syntax error.
CstNode* generateCST(bool debugMode) {
    CstNode* rootNode = new CstNode(CstNonTerminal::COMPOUND_STMT);
    size_t inputTokenPtr = 0; // points to the current token in the stream to be parsed.

    if (!validPattern(patternList[CstNonTerminal::COMPOUND_STMT][0], rootNode, inputTokenPtr)) {
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
    if (tokenStream.size() == 0) {
        std::cout << "LEXER ERROR]" << std::endl;
        getchar();
    }
    std::cout << "DONE]" << std::endl;

    
    std::cout << "Generating CST... [";
    CstNode* cstRootNode = generateCST(debugMode);
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


    ExprNode* rootExprNode = astRootNode->seqNode.stmts[0]->declNode.init->data.expr;

    std::vector<Instr*> block;
    std::vector<Instr*> machineCode = generateExprCode(rootExprNode, block, Register::RAX);

    std::cout << displayMachineCode(machineCode) << std::endl;;

    //std::cout << countNodes(cstRootNode) << std::endl;
    return 0; 
}