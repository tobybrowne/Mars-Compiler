
#include "general.h"

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
NumVarExpr* createExprTreeAST(CstNode* cstExpr, SymbolTable* symbolTable, bool negate = false) {
    CstNonTerminal expressionType = cstExpr->val.nonTerm;
    std::vector<CstNode*> childrenNodes = cstExpr->childrenNodes;

    if (expressionType == CstNonTerminal::FACTOR) {
        Token factorToken = childrenNodes[0]->token;
        TokenType factorType = factorToken.type;
        NumVarExpr* newNumVarExpr;

        if (factorType == TokenType::_NUMCONST) {
            newNumVarExpr = new NumVarExpr(Operand::NUMCONST_NODE, new NumConstNode(factorToken.numConstVal));
        }
        else if (factorType == TokenType::_ID) {
            TableEntry* tblEntry = getTblEntry(symbolTable, factorToken.varName);

            if (tblEntry == NULL) {
                std::cout << "VARIABLE " << factorToken.varName << " NOT DECLARED!]" << std::endl;
                getchar();
            }
            newNumVarExpr = new NumVarExpr(Operand::VAR_NODE, new VarNode(factorToken.varName, tblEntry));
        }
        return newNumVarExpr;
    }

    // if in use
    else if (expressionInUse(cstExpr)) {
        NumVarExpr* operand1;
        NumVarExpr* operand2;
        TokenType opcode;
        ExprNode* rootExprNode;

        if (expressionType == CstNonTerminal::UNARY_REL_EXP) {
            operand1 = createExprTreeAST(childrenNodes[1], symbolTable);
            operand2 = operand1;
            opcode = findOpcode(childrenNodes[0]);

            rootExprNode = new ExprNode(opcode, operand1->type, operand2->type);
            rootExprNode->a = operand1;
            rootExprNode->b = operand2;
        }

        else {

            opcode = findOpcode(childrenNodes[1]->childrenNodes[0]);
            CstNode* interNode = childrenNodes[1];

            operand2 = createExprTreeAST(interNode->childrenNodes[1], symbolTable);
            operand1 = createExprTreeAST(childrenNodes[0], symbolTable);

            rootExprNode = new ExprNode(opcode, operand1->type, operand2->type);
            rootExprNode->a = operand1;
            rootExprNode->b = operand2;

            // manages when multiple "same-level" operations are stacked "+" and "-" for example.
            ExprNode* prevExprNode = rootExprNode;
            while (interNode->childrenNodes[2]->childrenNodes.size() != 1) {
                opcode = findOpcode(interNode->childrenNodes[2]->childrenNodes[0]);

                operand1 = operand2;
                operand2 = createExprTreeAST(interNode->childrenNodes[2]->childrenNodes[1], symbolTable);

                ExprNode* newExprNode = new ExprNode(opcode, operand1->type, operand2->type);
                newExprNode->a = operand1;
                newExprNode->b = operand2;

                NumVarExpr* newNumVarExpr = new NumVarExpr(Operand::EXPR_NODE, newExprNode);

                prevExprNode->bType = Operand::EXPR_NODE;
                prevExprNode->b = newNumVarExpr;

                prevExprNode = newExprNode;

                interNode = interNode->childrenNodes[2];
            }


        }

        // creates expression node and returns it.
        NumVarExpr* newNumVarExpr = new NumVarExpr(Operand::EXPR_NODE, rootExprNode);
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


// generates a vector of declerations from the recursive CST decl structure.
std::vector<Stmt*> createDeclListAST(CstNode* decListCSTNode, std::vector<Stmt*> decASTNodeVector, SymbolTable* symbolTable) {
    std::vector<CstNode*> childrenNodes = decListCSTNode->childrenNodes;

    if (childrenNodes.size() == 1) { // at dead end...
        return decASTNodeVector;
    }

    CstNode* varDeclNode = childrenNodes[0];
    std::string varName = (varDeclNode->childrenNodes)[1]->childrenNodes[0]->token.varName;

    if (varDeclaredInScope(symbolTable, varName)) {
        std::cout << "VARIABLE " << varName << " DECLARED TWICE!]" << std::endl;
        getchar();
    }

    TokenType dataTypeToken = (varDeclNode->childrenNodes)[0]->childrenNodes[0]->token.type;
    Datatype dataType;
    switch (dataTypeToken) {
        case TokenType::_INT:
            dataType = Datatype::INT;
    }

    // add to symbol table...
    TableEntry* newSymTblEntry = new TableEntry(varName, ClassType::VARIABLE, dataType, symbolTable->currFrameOffset);
    symbolTable->entries.push_back(newSymTblEntry);

    // increment frame offset
    symbolTable->currFrameOffset += 8; // for an 64 bit integer

    // if declaration has an assignment add an assign node to AST.
    if (varDeclNode->childrenNodes[1]->childrenNodes.size() != 1) {
        Stmt* newAssignNode = new Stmt(Statement::ASSIGN_NODE);

        newAssignNode->assignNode.variable = new VarNode(varName, newSymTblEntry);
        newAssignNode->assignNode.furtherAssign = false; // not possible with declarations atm.

        NumVarExpr* init = createExprTreeAST((varDeclNode->childrenNodes)[1]->childrenNodes[2], symbolTable);
        newAssignNode->assignNode.init.exprTree = init;
        decASTNodeVector.push_back(newAssignNode);
    }

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
Stmt* createBreakNodeAST(CstNode* cstIterStmt) {
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