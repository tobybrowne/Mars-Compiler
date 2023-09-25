#pragma once   

enum class InstrType {
    LDR,
    STR,
    ADD,
    IMUL,
    MOV,
    SUB,
    DIV,
    AND,
    OR,
    CMP,
    JE,
    JNE,
    JNB,
    JNBE,
    JNA,
    JNAE,
    INT3,
    RET,
    JMP,
    LABEL,
    CALL
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

class programState {
    public:
        int whileCount = 0;
        int ifCount = 0;
};

// matches each relative operator to it's instruction type.
std::unordered_map<TokenType, InstrType> tokenInstrMatch = {
    {TokenType::_NOT, InstrType::JNE},
    {TokenType::_LT, InstrType::JNB},
    {TokenType::_LE, InstrType::JNBE},
    {TokenType::_GT, InstrType::JNA},
    {TokenType::_GE, InstrType::JNAE},
    {TokenType::_EQUAL, InstrType::JNE},
    {TokenType::_NOTEQUAL, InstrType::JE},
    {TokenType::_ADD, InstrType::ADD},
    {TokenType::_SUB, InstrType::SUB},
    {TokenType::_MUL, InstrType::IMUL},
    {TokenType::_DIV, InstrType::DIV},
    {TokenType::_AND, InstrType::AND},
    {TokenType::_OR, InstrType::OR},
};


std::vector<TokenType> relativeOps = {TokenType::_NOT , TokenType::_LT, TokenType::_LE, TokenType::_GT, TokenType::_GE, TokenType::_EQUAL, TokenType::_NOTEQUAL };

std::unordered_map<InstrType, std::string> instrString = {
    {InstrType::LDR, "LDR"},
    {InstrType::STR, "STR"},
    {InstrType::MOV, "MOV"},
    {InstrType::ADD, "ADD"},
    {InstrType::IMUL, "IMUL"},
    {InstrType::DIV, "DIV"},
    {InstrType::SUB, "SUB"},
    {InstrType::AND, "AND"},
    {InstrType::OR, "OR"},
    {InstrType::CMP, "CMP"},
    {InstrType::JE, "JE"},
    {InstrType::JNE, "JNE"},
    {InstrType::JNB, "JNB"},
    {InstrType::JNBE, "JNBE"},
    {InstrType::JNA, "JNA"},
    {InstrType::JNAE, "JNAE"},
    {InstrType::INT3, "INT3"},
    {InstrType::RET, "RET"},
    {InstrType::JMP, "JMP"},
    {InstrType::CALL, "CALL"},
};

enum class Register {
    RAX,
    RBX,
    RSP
};

std::unordered_map<Register, std::string> regString = {
    {Register::RAX, "RAX"},
    {Register::RBX, "RBX"},
    {Register::RSP, "RSP"},
};

enum class x86OperandTypes {
    REGISTER,
    IMMEDIATE,
    STACK_OFFSET,
    LABEL,
    EMPTY
};

class x86operand {
public:
    x86OperandTypes type;
    std::variant<Register, int, std::string> data;
    
    // sometimes needs a default constructor?
    x86operand() {
        type = x86OperandTypes::EMPTY;
        data = 0;
    }

    x86operand(x86OperandTypes inpType, std::variant<Register, int, std::string> inpData) {
        type = inpType;
        data = inpData;
    }
};


class Instr {
    public:
        InstrType type;
        int numArgs;
        x86operand op1;
        x86operand op2;
        x86operand op3; // can be null or operand
        std::optional<std::string> label;

        Instr(InstrType inpType, std::vector<x86operand> inpOps, std::optional<std::string> labelInp = std::nullopt) {
            type = inpType;
            numArgs = inpOps.size();

            if (labelInp) {
                label = labelInp.value();
            }

            for (int i = 0; i < inpOps.size(); i++) {
                if (i == 0) {
                    op1 = inpOps[0];
                }
                else if (i == 1) {
                    op2 = inpOps[1];
                }
                else {
                    op3 = inpOps[2];
                }
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



struct Token {
    TokenType type;
    // only one can be true at a time so use a union to save memory?!?
    // using a union throws some stupid constructor errors that need to be resolved.
    std::string varName;
    int numConstVal;
};



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
    for (CstNode* childNode : childrenNodes) {
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

    NumVarExpr(Operand inpType, std::variant<NumConstNode*, VarNode*, ExprNode*> inpData) {
        type = inpType;
        switch (type) {
        case Operand::VAR_NODE:
            data.var = std::get<VarNode*>(inpData);
            break;
        case Operand::NUMCONST_NODE:
            data.numconst = std::get<NumConstNode*>(inpData);
            break;
        case Operand::EXPR_NODE:
            data.expr = std::get<ExprNode*>(inpData);
            break;
        }
    };
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

Stmt::Stmt(Statement stmtType) : type(stmtType) {
    switch (stmtType) {
    case Statement::STMTSEQ_NODE: {
        new (&seqNode.stmts) std::vector<Stmt*>();
        break;
    }
    case Statement::IF_NODE: {
        ifNode.elsePresent = false;
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

std::map<CstNonTerminal, std::vector<std::vector<std::variant<TokenType, CstNonTerminal>>>> patternList;

void defineLanguageGrammar() {
    patternList[CstNonTerminal::STMT] = { {CstNonTerminal::EXP_STMT}, {CstNonTerminal::COMPOUND_STMT}, {CstNonTerminal::SELECT_STMT}, {CstNonTerminal::ITER_STMT}, {CstNonTerminal::RETURN_STMT}, {CstNonTerminal::BREAK_STMT} };

    patternList[CstNonTerminal::EXP_STMT] = { {CstNonTerminal::EXP, TokenType::_SEMI}, {TokenType::_SEMI} };

    patternList[CstNonTerminal::COMPOUND_STMT] = { {TokenType::_OPENCURLY, CstNonTerminal::DEC_LIST, CstNonTerminal::STMT_LIST, TokenType::_CLOSECURLY} };
    patternList[CstNonTerminal::DEC_LIST] = { {CstNonTerminal::VAR_DECL, CstNonTerminal::DEC_LIST}, {TokenType::_E} };

    patternList[CstNonTerminal::VAR_DECL] = { {CstNonTerminal::TYPESPEC, CstNonTerminal::VAR_DECL_INIT, TokenType::_SEMI} };
    patternList[CstNonTerminal::VAR_DECL_INIT] = { {TokenType::_ID, TokenType::_ASSIGN, CstNonTerminal::SIMPLE_EXP}, {TokenType::_ID} };
    patternList[CstNonTerminal::TYPESPEC] = { {TokenType::_INT} };

    patternList[CstNonTerminal::STMT_LIST] = { {CstNonTerminal::STMT, CstNonTerminal::STMT_LIST}, {TokenType::_E} };
    patternList[CstNonTerminal::SELECT_STMT] = { {TokenType::_IF, TokenType::_OPENBRACK, CstNonTerminal::SIMPLE_EXP, TokenType::_CLOSEBRACK, CstNonTerminal::COMPOUND_STMT, TokenType::_ELSE, CstNonTerminal::COMPOUND_STMT}, {TokenType::_IF, TokenType::_OPENBRACK, CstNonTerminal::SIMPLE_EXP, TokenType::_CLOSEBRACK, CstNonTerminal::COMPOUND_STMT}, };

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

    patternList[CstNonTerminal::REL_EXP] = { {CstNonTerminal::SUM_EXP, CstNonTerminal::REL_EXP__} };
    patternList[CstNonTerminal::REL_EXP__] = { {CstNonTerminal::REL_OP, CstNonTerminal::SUM_EXP, CstNonTerminal::REL_EXP__}, {TokenType::_E} };
    patternList[CstNonTerminal::REL_OP] = { {TokenType::_LE}, {TokenType::_LT}, {TokenType::_GT}, {TokenType::_GE}, {TokenType::_EQUAL}, {TokenType::_NOTEQUAL} };

    patternList[CstNonTerminal::SUM_EXP] = { {CstNonTerminal::MUL_EXP, CstNonTerminal::SUM_EXP__} };
    patternList[CstNonTerminal::SUM_EXP__] = { {CstNonTerminal::SUM_OP, CstNonTerminal::MUL_EXP, CstNonTerminal::SUM_EXP__}, {TokenType::_E} };
    patternList[CstNonTerminal::SUM_OP] = { {TokenType::_ADD}, {TokenType::_SUB} };

    patternList[CstNonTerminal::MUL_EXP] = { {CstNonTerminal::FACTOR, CstNonTerminal::MUL_EXP__} };
    patternList[CstNonTerminal::MUL_EXP__] = { {CstNonTerminal::MUL_OP, CstNonTerminal::FACTOR, CstNonTerminal::MUL_EXP__}, {TokenType::_E} };
    patternList[CstNonTerminal::MUL_OP] = { {TokenType::_MUL}, {TokenType::_DIV} };

    patternList[CstNonTerminal::FACTOR] = { {TokenType::_NUMCONST}, {TokenType::_ID} };
}