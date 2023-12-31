
#include "general.h"

std::string displayMachineCode(std::vector<Instr*> program) {
    std::string programString = "\nformat PE64 NX GUI 6.0\nentry start \nsection '.text' code readable executable \nstart :";

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
std::vector<Instr*> generateExprCode(NumVarExpr* numVarExpr, std::vector<Instr*> block, Register retReg, bool root = false) {
    switch (numVarExpr->type) {
        
        // these top ones only run for "expressions" w no opcodes e.g: a = 3;
        case Operand::FUNCCALL_NODE:
            block.push_back(new Instr(InstrType::CALL, { x86operand(x86OperandTypes::LABEL, numVarExpr->data.funccall->tableEntry->name)}));
            // rax is  return register
            if (retReg != Register::RAX) {
                block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::REGISTER, Register::RAX) }));
            }
            break;
        case Operand::VAR_NODE:
            block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::STACK_OFFSET, numVarExpr->data.var->tableEntry->varData.frameOffset) }));
            break;
        case Operand::NUMCONST_NODE:
            block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, numVarExpr->data.numconst->val) }));
            break;

        // ran most of the time...
        case Operand::EXPR_NODE:
            ExprNode* exprTree = numVarExpr->data.expr;
            TokenType opcode = exprTree->opcode;

            std::vector<Operand> operandTypes;
            std::vector<NumVarExpr*> operands;
            std::vector<Register> loadRegisters = { Register::RCX, Register::RDX };
            std::vector<Register> retRegisters = { Register::RAX, Register::RBX };

            std::vector<Register> operandRegisters;


            operandTypes.push_back(exprTree->aType);
            operands.push_back(exprTree->a);
            if (opcode != TokenType::_NOT) {
                operandTypes.push_back(exprTree->bType);
                operands.push_back(exprTree->b);
            }

            std::vector<Instr*> newBlock;

            for (int i = 0; i < operands.size(); i++) {
                if (operandTypes[i] == Operand::EXPR_NODE) {
                    merge(block, generateExprCode(operands[i], {}, retRegisters[i]));
                    operandRegisters.push_back(retRegisters[i]);
                }
                else {
                    merge(newBlock, generateExprCode(operands[i], block, loadRegisters[i]));
                    operandRegisters.push_back(loadRegisters[i]);
                }
            }
            // ensures the order is preserved
            merge(block, newBlock);

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
                x86operand op1 = x86operand(x86OperandTypes::REGISTER, operandRegisters[0]);
                x86operand op2 = x86operand(x86OperandTypes::REGISTER, operandRegisters[1]);
                block.push_back(new Instr(tokenInstrMatch[opcode], { op1, op2}));

                if (retReg != operandRegisters[0]) {
                    op1 = x86operand(x86OperandTypes::REGISTER, retReg);
                    op2 = x86operand(x86OperandTypes::REGISTER, operandRegisters[0]);
                    block.push_back(new Instr(InstrType::MOV, { op1, op2 }));
                }
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
    std::vector<Instr*> evalExpr = generateExprCode(condition, {}, Register::RAX, true);
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
    block = generateExprCode(condition, block, Register::RAX, true);
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
    block = generateExprCode(assignNode->assignNode.init.exprTree, {}, Register::RAX, true);
    block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::STACK_OFFSET, stackOffset), x86operand(x86OperandTypes::REGISTER, Register::RAX) })); // cmp rax #0
    
    // further assignment...
    if (assignNode->assignNode.furtherAssign) {
        std::vector<Instr*> moreAssign = generateAssignCode(assignNode->assignNode.init.assignNode,state);
        merge(moreAssign, block);
        return moreAssign;
    }
    
    return block;
}

std::vector<Instr*> generateReturnCode(Stmt* retNode, programState state) {
    std::vector<Instr*> block;

    // place operand in RAX (return register)
    if (retNode->retNode.operandPresent) {
        block = generateExprCode(retNode->retNode.operand, {}, Register::RAX, true);
    }
    block.push_back(new Instr(InstrType::JMP, {x86operand(x86OperandTypes::LABEL, ".ENDFUNC")}));
    return block;
}

std::vector<Instr*> generateFunctionDecl(Stmt* funcNode, programState state) {
    std::vector<Instr*> block;
    TableEntry* symTblEntry = funcNode->funcDeclNode.tableEntry;
    std::string funcName = symTblEntry->name;
    block.push_back(new Instr(InstrType::LABEL, {}, funcName)); // cmp rax #0 
    block.push_back(new Instr(InstrType::SUB, { x86operand(x86OperandTypes::REGISTER, Register::RSP), x86operand(x86OperandTypes::IMMEDIATE, symTblEntry->funcData.memRequired) }));
    merge(block, generateCodeBlock(funcNode->funcDeclNode.innerCode, state));
    block.push_back(new Instr(InstrType::ADD, { x86operand(x86OperandTypes::REGISTER, Register::RSP), x86operand(x86OperandTypes::IMMEDIATE, symTblEntry->funcData.memRequired) }, ".ENDFUNC"));
    block.push_back(new Instr(InstrType::RET, {}));
    
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
            case Statement::FUNC_DECL_NODE:
                newInstrs = generateFunctionDecl(statement, state);
                break;
        }
        merge(block, newInstrs);
    }
    return block;
}


