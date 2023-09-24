
#include "general.h"

int ifCount = 0;
int whileCount = 0;


std::string displayMachineCode(std::vector<Instr*> program) {
    std::string programString = "\nformat PE64 NX GUI 6.0\nentry start \nsection '.text' code readable executable \nstart : \n";

    for (Instr* instruction : program) {
        std::string instructStr = "\n";

        if (instruction->label) {
            instructStr = instructStr + instruction->label.value() + ": ";
        }
        instructStr+="\t";


        instructStr += instrString[instruction->type];

        std::vector<x86operand> operands = { instruction->op1 , instruction->op2 , instruction->op3 };
        std::vector<x86OperandTypes> operandTypes = { instruction->op1.type , instruction->op2.type , instruction->op3.type };

        for (int i = 0; i < instruction->numArgs; i++) {
            switch (operandTypes[i]) {
            case x86OperandTypes::REGISTER:
                instructStr += " " + regString[std::get<Register>(operands[i].data)];
                break;

            case x86OperandTypes::IMMEDIATE:
                instructStr += " " + std::to_string(std::get<int>(operands[i].data));
                break;

            case x86OperandTypes::STACK_OFFSET:
                instructStr += " sp+" + std::to_string(std::get<int>(operands[i].data));
                break;

            case x86OperandTypes::LABEL:
                instructStr += " "+std::get<std::string>(operands[i].data);
                break;
            }
            if (i != instruction->numArgs - 1) {
                instructStr += ",";
            }
        }
        programString += instructStr;
    }
    programString += "\n\tINT3\n\tRET";
    return programString;
}

// generates code to calculate the result of an expression tree and store it in R2.
// write this so it doesn't need an input block?
std::vector<Instr*> generateExprCode(ExprNode* exprTree, std::vector<Instr*> block, Register retReg) {
    Instr* newInstr1;
    Instr* newInstr2;

    bool instr1present = true;
    bool instr2present = true;

    TokenType opcode = exprTree->opcode;

    // manage the operands

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
        newInstr1 = new Instr(InstrType::LDR, { op1, op2 });
        break;
    }

                          // should be able to get rid of this extra move and set the numconst as an operand
    case Operand::NUMCONST_NODE: {
        x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
        x86operand op2 = x86operand(x86OperandTypes::IMMEDIATE, exprTree->a->data.numconst->val);
        newInstr1 = new Instr(InstrType::MOV, { op1, op2 });
        break;
    }
    }
    if (instr1present == true) {
        block.push_back(newInstr1);
    }

    // two operands
    if (opcode != TokenType::_NOT) {
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
            newInstr2 = new Instr(InstrType::LDR, { op1, op2 });
            break;
        }

        case Operand::NUMCONST_NODE: {
            x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
            x86operand op2 = x86operand(x86OperandTypes::IMMEDIATE, exprTree->b->data.numconst->val);
            newInstr2 = new Instr(InstrType::MOV, { op1, op2 });
            break;
        }
        }

        if (instr2present == true) {
            block.push_back(newInstr2);
        }
    }


    // performs operation

    if (opcode == TokenType::_NOT) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNE, { x86operand(x86OperandTypes::LABEL, "ENDOPf")})); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
        block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf")); // mov retReg #1
    }
    else if (opcode == TokenType::_LT) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNB, { x86operand(x86OperandTypes::LABEL, "ENDOPf") })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
        block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf"));
    }
    else if (opcode == TokenType::_LE) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNBE, { x86operand(x86OperandTypes::LABEL, "ENDOPf") })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
        block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf"));
    }
    else if (opcode == TokenType::_GT) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNA, { x86operand(x86OperandTypes::LABEL, "ENDOPf") })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
        block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf"));
    }
    else if (opcode == TokenType::_GE) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNAE, { x86operand(x86OperandTypes::LABEL, "ENDOPf") })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
        block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf"));
    }
    else if (opcode == TokenType::_EQUAL) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNE, { x86operand(x86OperandTypes::LABEL, "ENDOPf") })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
        block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf"));
    }
    else if (opcode == TokenType::_NOTEQUAL) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::LABEL, "ENDOPf") })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
        block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf"));
    }
    else {
        InstrType type;

        switch (opcode) {
        case TokenType::_ADD:
            type = InstrType::ADD;
            break;
        case TokenType::_SUB:
            type = InstrType::SUB;
            break;
        case TokenType::_MUL:
            type = InstrType::IMUL;
            break;
        case TokenType::_DIV:
            type = InstrType::DIV;
            break;
        case TokenType::_AND:
            type = InstrType::AND;
            break;
        case TokenType::_OR:
            type = InstrType::OR;
            break;

        }

        if (retReg == Register::RAX) {
            x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
            x86operand op2 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
            Instr* newInstr = new Instr(type, { op1, op2 });
            block.push_back(newInstr);
        }
        else {
            x86operand op1 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
            x86operand op2 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
            Instr* newInstr = new Instr(type, { op1, op2 });
            block.push_back(newInstr);
        }

        

    }
    return block;
}

// forward definition.
std::vector<Instr*> generateCodeBlock(Stmt* seqNode);

std::vector<Instr*> generateWhileCode(Stmt* whileNode) {
    std::cout << "hiya" << std::endl;
    std::vector<Instr*> block;
    NumVarExpr* condition = whileNode->whileNode.condition;
    std::string whileCountStr = std::to_string(whileCount);
    block.push_back(new Instr(InstrType::LABEL, {}, "STARTLOOP" + whileCountStr)); // cmp rax #0 
    std::vector<Instr*> evalExpr = generateExprCode(condition->data.expr, {}, Register::RAX);
    block.insert(block.end(), evalExpr.begin(), evalExpr.end()); 

    block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // cmp rax #0 
    block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::LABEL, "END"+whileCountStr)})); // cmp rax #0 
    
    std::vector<Instr*> whileBlock = generateCodeBlock(whileNode->whileNode.body);
    block.insert(block.end(), whileBlock.begin(), whileBlock.end()); // block += whileBlock
    block.push_back(new Instr(InstrType::JMP, { x86operand(x86OperandTypes::LABEL, "STARTLOOP" + whileCountStr) })); // cmp rax #0 
    block.push_back(new Instr(InstrType::LABEL, {}, "END"+whileCountStr)); // cmp rax #0 
    
    return block;
}

std::vector<Instr*> generateIfCode(Stmt* ifNode) {
    std::vector<Instr*> block;
    NumVarExpr* condition = ifNode->ifNode.condition;
    std::string ifCountStr = std::to_string(ifCount);
    block = generateExprCode(condition->data.expr, block, Register::RAX);
    block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // cmp rax #0 
    
    std::vector<Instr*> ifBlock = generateCodeBlock(ifNode->ifNode.ifBody); // = // deal with ifNode.ifBody

    if (ifNode->ifNode.elsePresent) {
        std::vector<Instr*> elseBlock = generateCodeBlock(ifNode->ifNode.elseBody); // deal with this ifNode.elseBody
        elseBlock[0]->label = "ELSE" + ifCountStr; // label else block
        block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::LABEL, "ELSE"+ ifCountStr)})); // cmp rax #0 
        ifBlock.push_back(new Instr(InstrType::JMP, { x86operand(x86OperandTypes::LABEL, "END" + ifCountStr) })); // cmp rax #0 )
        block.insert(block.end(), ifBlock.begin(), ifBlock.end()); // block += ifBlock
        block.insert(block.end(), elseBlock.begin(), elseBlock.end()); // block += elseBlock
        
    }
    else {
        block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::LABEL, "END" + ifCountStr) }));
        block.insert(block.end(), ifBlock.begin(), ifBlock.end()); // block += ifBlock
    }

    block.push_back(new Instr(InstrType::LABEL, {}, "END"+ ifCountStr)); // cmp rax #0 
   
    return block;
}



std::vector<Instr*> generateCodeBlock(Stmt* seqNode) {
    std::vector<Instr*> block;
    for (Stmt* statement : seqNode->seqNode.stmts) {
        std::vector<Instr*> newInstrs;
        switch (statement->type) {
            case Statement::IF_NODE:
                newInstrs = generateIfCode(statement);
                break;
            case Statement::WHILE_NODE:
                newInstrs = generateWhileCode(statement);
                break;
            case Statement::DECL_NODE:
                break;
            case Statement::ASSIGN_NODE:
                break;
            case Statement::STMTSEQ_NODE:
                break;
            case Statement::RET_NODE:
                break;
        }
        block.insert(block.end(), newInstrs.begin(), newInstrs.end()); // block += newInstr
    }
    return block;
}

