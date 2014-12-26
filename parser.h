#ifndef PARSE_HDR
#define PARSE_HDR
#include <setjmp.h>

#include "lexer.h"
#include "ast.h"

namespace glsl {

struct stage {
    stage()
        : flags(0)
        , interpolation(-1)
        , precision(-1)
        , memory(-1)
        , type(0)
        , arraySize(0)
        , isArray(false)
    {
    }
    int flags;
    int interpolation;
    int precision;
    int memory;
    std::vector<astLayoutQualifier*> layoutQualifiers;
    astType *type;
    astConstantExpression *arraySize;
    bool isArray;
    std::string name;
};


struct parser {
    ~parser();
    parser(const std::string &source);
    astTU *parse();

protected:
    enum {
        kEndConditionSemicolon = 1 << 0,
        kEndConditionParanthesis = 1 << 1,
        kEndConditionBracket = 1 << 2,
        kEndConditionColon = 1 << 3,
        kEndConditionComma = 1 << 4
    };

    typedef int endCondition;

    void next();

    stage parseGlobalItem(stage *continuation = 0);
    std::vector<stage> parseGlobalScope();

    bool isType(int type) const;
    bool isKeyword(int keyword) const;
    bool isOperator(int oper) const;
    bool isEndCondition(endCondition condition) const;
    bool isBuiltin() const;

    void parseLayout(std::vector<astLayoutQualifier*> &layoutQualifiers);

    void fatal(const char *fmt, ...);

    // Type parsers
    astBuiltin *parseBuiltin();
    astStruct *parseStruct();
    astFunction *parseFunction(const stage &parse);

    // Call parsers
    astConstructorCall *parseConstructorCall();
    astFunctionCall *parseFunctionCall();

    // Expression parsers
    astExpression *parseExpression(endCondition end);
    astExpression *parseUnary(endCondition end);
    astExpression *parseBinary(int lhsPrecedence, astExpression *lhs, endCondition condition);
    astExpression *parseUnaryPrefix(endCondition end);
    astConstantExpression *parseArraySize();

    // Statement parsers
    astStatement *parseStatement();
    astSwitchStatement *parseSwitchStatement();
    astCaseLabelStatement *parseCaseLabelStatement();
    astForStatement *parseForStatement();
    astCompoundStatement *parseCompoundStatement();
    astIfStatement *parseIfStatement();
    astSimpleStatement *parseDeclarationOrExpressionStatement(endCondition condition);
    astDeclarationStatement *parseDeclarationStatement(endCondition condition);
    astExpressionStatement *parseExpressionStatement(endCondition condition);
    astContinueStatement *parseContinueStatement();
    astBreakStatement *parseBreakStatement();
    astDiscardStatement *parseDiscardStatement();
    astReturnStatement *parseReturnStatement();
    astDoStatement *parseDoStatement();
    astWhileStatement *parseWhileStatement();

    astBinaryExpression *createExpression();

    astType *findType(const std::string &identifier);
    astVariable *findVariable(const std::string &identifier);

private:
    typedef std::vector<astVariable *> scope;

    template <typename T>
    astCollect<T> *gc();

    astTU *m_ast;
    lexer m_lexer;
    token m_token;
    std::vector<scope> m_scopes;
    std::vector<astBuiltin*> m_builtins;

    jmp_buf m_exit;
    std::string m_error;
};

template <typename T>
inline astCollect<T> *parser::gc() {
    return (astCollect<T>*)m_ast;
}

}

#endif
