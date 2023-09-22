
std::string displayMachineCode(std::vector<Instr*> program) {
    std::string programString = "\nformat PE64 NX GUI 6.0\nentry start \nsection '.text' code readable executable \nstart : \n";

    for (Instr* instruction : program) {
        std::string instructStr = "\n";

        instructStr += instrString[instruction->type];

        std::vector<x86operand> operands = { instruction->op1 , instruction->op2 , instruction->op3 };
        std::vector<x86OperandTypes> operandTypes = { instruction->op1.type , instruction->op2.type , instruction->op3.type };

        for (int i = 0; i < instruction->numArgs; i++) {
            switch (operandTypes[i]) {
            case x86OperandTypes::REGISTER:
                instructStr += " " + regString[operands[i].reg];
                break;

            case x86OperandTypes::IMMEDIATE:
                instructStr += " " + std::to_string(operands[i].imm);
                break;

            case x86OperandTypes::STACK_OFFSET:
                instructStr += " sp+" + std::to_string(operands[i].offset);
                break;
            }
            if (i != instruction->numArgs - 1) {
                instructStr += ",";
            }
        }
        programString += instructStr;
    }
    programString += "\nINT3\nRET";
    return programString;
}

// generates code to calculate the result of an expression tree and store it in R2.
std::vector<Instr*> generateExprCode(ExprNode* exprTree, std::vector<Instr*> block, Register retReg) {
    Instr* newInstr1;
    Instr* newInstr2;

    bool instr1present = true;
    bool instr2present = true;

    x86operand op1;
    x86operand op2;

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
        block.push_back(new Instr(InstrType::JNE, { x86operand(x86OperandTypes::IMMEDIATE, 2) })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
    }
    else if (opcode == TokenType::_LT) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNB, { x86operand(x86OperandTypes::IMMEDIATE, 2) })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
    }
    else if (opcode == TokenType::_LE) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNBE, { x86operand(x86OperandTypes::IMMEDIATE, 2) })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
    }
    else if (opcode == TokenType::_GT) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNA, { x86operand(x86OperandTypes::IMMEDIATE, 2) })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
    }
    else if (opcode == TokenType::_GE) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNAE, { x86operand(x86OperandTypes::IMMEDIATE, 2) })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
    }
    else if (opcode == TokenType::_EQUAL) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JNE, { x86operand(x86OperandTypes::IMMEDIATE, 2) })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
    }
    else if (opcode == TokenType::_NOTEQUAL) {
        block.push_back(new Instr(InstrType::CMP, { x86operand(x86OperandTypes::REGISTER, Register::RAX), x86operand(x86OperandTypes::REGISTER, Register::RBX) })); // cmp rax #0
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 0) })); // mov retReg #0
        block.push_back(new Instr(InstrType::JE, { x86operand(x86OperandTypes::IMMEDIATE, 2) })); // jne #2
        block.push_back(new Instr(InstrType::MOV, { x86operand(x86OperandTypes::REGISTER, retReg), x86operand(x86OperandTypes::IMMEDIATE, 1) })); // mov retReg #1
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
            op1 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
            op2 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
        }
        else {
            op1 = x86operand(x86OperandTypes::REGISTER, Register::RBX);
            op2 = x86operand(x86OperandTypes::REGISTER, Register::RAX);
        }

        Instr* newInstr = new Instr(type, { op1, op2 });
        block.push_back(newInstr);

    }
    return block;
}


