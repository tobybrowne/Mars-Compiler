#include "general.h"
// recursive function to validate a provided grammar pattern for the current point in the token stream.
// CONSIDER: Rewrite without pointers, just put the object itself inside the vector.
//inputPtr passed by reference gives a "global" effect.


// TODO: FIX EOF Parsing ISSUE - shouldn't require a + to end.
bool validPattern(std::vector<std::variant<TokenType, CstNonTerminal>> pattern, CstNode* rootTreeNode, size_t& inputTokenPtr, const std::vector<Token>& tokenStream, CstNode* &rooot) {
    size_t savedPtr;
    bool valid;

    for (std::variant<TokenType, CstNonTerminal>& component : pattern) { // by reference is more efficient.
        
        
        
        
        CstNode* newTreeNode = new CstNode(component);
        rootTreeNode->childrenNodes.push_back(newTreeNode);

        // if a non terminal...
        if (!newTreeNode->isToken) {

            valid = false;

            //check if component is valid
            for (std::vector<std::variant<TokenType, CstNonTerminal>>& possiblePattern : patternList[std::get<CstNonTerminal>(component)]) {
                savedPtr = inputTokenPtr;
                if (validPattern(possiblePattern, newTreeNode, inputTokenPtr, tokenStream, rooot)) {
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
                // curly bracket gets checked, then pointer incremented
                // then has to see if declist is fulfilled (doesnt get to _E yet).

                
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

// TO:DO {} in the global scope is valid cos it just reads it as empty therefore anything can be put in the global scope?!?
// it literally just counts every token as an empty - but why would it keep incrementing the input pointer?


// returns the root node of the Concrete Syntax Tree or NULL in the event of a syntax error.
CstNode* generateCST(bool debugMode, std::vector<Token> tokenStream) {
    CstNode* rootNode = new CstNode(CstNonTerminal::PROGRAM);
    size_t inputTokenPtr = 0; // points to the current token in the stream to be parsed.

    if (!validPattern(patternList[CstNonTerminal::PROGRAM][0], rootNode, inputTokenPtr, tokenStream, rootNode)) {
        return NULL;
    }
    return rootNode;
}
