
#include "general.h"
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
