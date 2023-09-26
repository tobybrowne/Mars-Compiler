
#include "general.h"

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

            case x86OperandTypes::STACK_OFFSET: {
                int stackOffset = std::get<int>(operands[i].data);
                if (stackOffset == 0) {
                    instructStr += " [RSP]";
                }
                else {
                    instructStr += " [RSP+" + std::to_string(stackOffset) + "]";
                }
                break;
            }

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
    return programString;
}

void merge(std::vector<Instr*> &block1, std::vector<Instr*> &block2) {
    block1.insert(block1.end(), block2.begin(), block2.end());
}


// generates code to calculate the result of an expression tree and store it in R2.
// write this so it doesn't need an input block?
std::vector<Instr*> generateExprCode(NumVarExpr* numVarExpr, std::vector<Instr*> block, Register retReg) {
    switch (numVarExpr->type) {
        case Operand::VAR_NODE:
            block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::STACK_OFFSET, numVarExpr->data.var->tableEntry->varData.frameOffset) }));
            break;
        case Operand::NUMCONST_NODE:
            block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, numVarExpr->data.numconst->val) }));
            break;
        case Operand::EXPR_NODE:
            ExprNode* exprTree = numVarExpr->data.expr;
            TokenType opcode = exprTree->opcode;

            std::vector<Operand> operandTypes;
            std::vector<NumVarExpr*> operands;
            std::vector<Register> registers = {Register::RAX, Register::RBX};

            operandTypes.push_back(exprTree->aType);
            operands.push_back(exprTree->a);
            if (opcode != TokenType::_NOT) {
                operandTypes.push_back(exprTree->bType);
                operands.push_back(exprTree->b);
            }

            for (int i = 0; i < operands.size(); i++) {
                switch (operandTypes[i]) {
                    case Operand::EXPR_NODE: {
                        std::vector<Instr*> newInstrs = generateExprCode(operands[i], block, registers[i]);
                        merge(block, newInstrs);
                        break;
                    }
                    case Operand::VAR_NODE: {
                        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, registers[i]), x86operand(x86OperandTypes::STACK_OFFSET, operands[i]->data.var->tableEntry->varData.frameOffset) }));
                        break;
                    }                       // should be able to get rid of this extra move and set the numconst as an operand
                    case Operand::NUMCONST_NODE: {
                        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, registers[i]), x86operand(x86OperandTypes::IMMEDIATE, operands[i]->data.numconst->val) }));
                        break;
                    }
                }
            }

            // if opcode is a relative operator...
            if (std::find(relativeOps.begin(), relativeOps.end(), opcode) != relativeOps.end()) {
                if (opcode == TokenType::_NOT) {
                    block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // cmp rax #0
                }
                else {
                    block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
                }

                block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
                block.push_back(new Instr(tokenInstrMatch[opcode], {x86operand(x86OperandTypes::LABEL, "ENDOPf")})); // jne #2
                block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
                block.push_back(new Instr(InstrType::LABEL, {}, "ENDOPf")); // mov retReg #1
            }

            else {
                x86operand op1;
                x86operand op2;
                if (retReg == Register::RAX) {
                    op1 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
                    op2 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
                }
                else {
                    op1 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
                    op2 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
                }
                block.push_back(new Instr(tokenInstrMatch[opcode], { op1, op2 }));
            }
            break;
    }
    return block;
}

// forward definition.
std::vector<Instr*> generateCodeBlock(Stmt* seqNode, programState state);

std::vector<Instr*> generateWhileCode(Stmt* whileNode, programState state) {
    state.whileCount++;

    std::vector<Instr*> block;
    NumVarExpr* condition = whileNode->whileNode.condition;
    std::string whileCountStr = std::to_string(state.whileCount);
    block.push_back(new Instr(InstrType::LABEL, {}, "STARTLOOP" + whileCountStr)); // cmp rax #0 
    std::vector<Instr*> evalExpr = generateExprCode(condition, {}, Register::RAX);
    merge(block, evalExpr);

    block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // cmp rax #0 
    block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::LABEL, "ENDLOOP"+whileCountStr)})); // cmp rax #0 
    
    std::vector<Instr*> whileBlock = generateCodeBlock(whileNode->whileNode.body, state);
    merge(block, whileBlock);
    block.push_back(new Instr(InstrType::JMP, { x86operand(x86OperandTypes::LABEL, "STARTLOOP" + whileCountStr) })); // cmp rax #0 
    block.push_back(new Instr(InstrType::LABEL, {}, "ENDLOOP"+whileCountStr)); // cmp rax #0 
    
    return block;
}

std::vector<Instr*> generateIfCode(Stmt* ifNode, programState state) {
    state.ifCount++;
    std::vector<Instr*> block;
    NumVarExpr* condition = ifNode->ifNode.condition;
    std::string ifCountStr = std::to_string(state.ifCount);
    block = generateExprCode(condition, block, Register::RAX);
    block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // cmp rax #0 
    
    std::vector<Instr*> ifBlock = generateCodeBlock(ifNode->ifNode.ifBody, state); // = // deal with ifNode.ifBody

    if (ifNode->ifNode.elsePresent) {
        std::vector<Instr*> elseBlock = generateCodeBlock(ifNode->ifNode.elseBody, state); // deal with this ifNode.elseBody
        elseBlock[0]->label = "ELSE" + ifCountStr; // label else block
        block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::LABEL, "ELSE"+ ifCountStr)})); // cmp rax #0 
        ifBlock.push_back(new Instr(InstrType::JMP, { x86operand(x86OperandTypes::LABEL, "END" + ifCountStr) })); // cmp rax #0 )
        merge(block, ifBlock);
        merge(block, elseBlock);
    }
    else {
        block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::LABEL, "END" + ifCountStr) }));
        merge(block, ifBlock);
    }

    block.push_back(new Instr(InstrType::LABEL, {}, "END"+ ifCountStr)); // cmp rax #0 
   
    return block;
}

std::vector<Instr*> generateBreakCode(programState state) {
    std::vector<Instr*> block;
    std::string whileCountStr = std::to_string(state.whileCount);
    block.push_back(new Instr(InstrType::JMP, { x86operand(x86OperandTypes::LABEL, "ENDLOOP" + whileCountStr) })); 
    return block;
}

std::vector<Instr*> generateAssignCode(Stmt* assignNode, programState state) {
    std::vector<Instr*> block;

    // get var data...
    VarNode* varNode = assignNode->assignNode.variable;
    TableEntry* tableEntry = varNode->tableEntry;
    int stackOffset = tableEntry->varData.frameOffset;

    //get init data... [assuming no further assignment]
    // can function be edited so it doesn't take in an expression?
    block = generateExprCode(assignNode->assignNode.init.exprTree, {}, Register::RAX);
    block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::STACK_OFFSET, stackOffset), x86operand(x86OperandTypes::REGISTER, Register::RAX) })); // cmp rax #0
    return block;
}

std::vector<Instr*> generateReturnCode(Stmt* retNode, programState state) {
    std::vector<Instr*> block;

    // place operand in RAX (return register)
    if (retNode->retNode.operandPresent) {
        block = generateExprCode(retNode->retNode.operand, {}, Register::RAX);
    }
    block.push_back(new Instr(InstrType::JMP, {x86operand(x86OperandTypes::LABEL, "ENDFUNCf")}));
    return block;
}

std::vector<Instr*> generateCodeBlock(Stmt* seqNode, programState state) {
    std::vector<Instr*> block;
    for (Stmt* statement : seqNode->seqNode.stmts) {
        std::vector<Instr*> newInstrs;
        switch (statement->type) {
            case Statement::IF_NODE:
                newInstrs = generateIfCode(statement, state);
                break;
            case Statement::WHILE_NODE:
                newInstrs = generateWhileCode(statement, state);
                break;
            case Statement::ASSIGN_NODE:
                newInstrs = generateAssignCode(statement, state);
                break;
            case Statement::STMTSEQ_NODE:
                newInstrs = generateCodeBlock(statement, state);
                break;
            case Statement::BREAK_NODE:
                newInstrs = generateBreakCode(state);
                break;
            case Statement::RET_NODE:
                newInstrs = generateReturnCode(statement, state);
                break;
        }
        merge(block, newInstrs);
    }
    return block;
}

