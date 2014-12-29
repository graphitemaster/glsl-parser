#ifndef PARSE_HDR
#define PARSE_HDR
#include "lexer.h"
#include "ast.h"

namespace glsl {

#if __GNUC__ >= 4
#   define CHECK_RETURN __attribute__((warn_unused_result))
#elif _MSC_VER >= 1700
#   define CHECK_RETURN _Check_return_
#else
#   define CHECK_RETURN
#endif

struct topLevel {
    topLevel()
        : storage(-1)
        , auxiliary(-1)
        , memory(0)
        , precision(-1)
        , interpolation(-1)
        , type(0)
        , initialValue(0)
        , isInvariant(false)
        , isPrecise(false)
        , isArray(false)
    {
    }
    int storage;
    int auxiliary;
    int memory;
    int precision;
    int interpolation;
    std::vector<astLayoutQualifier*> layoutQualifiers;
    astType *type;
    astConstantExpression *initialValue;
    std::vector<astConstantExpression*> arraySizes;
    bool isInvariant;
    bool isPrecise;
    bool isArray;
    std::string name;
};


struct parser {
    ~parser();
    parser(const std::string &source);
    CHECK_RETURN astTU *parse(int type);

    const char *error() const;

protected:
    void cleanup();

    enum {
        kEndConditionSemicolon = 1 << 0,
        kEndConditionParanthesis = 1 << 1,
        kEndConditionBracket = 1 << 2,
        kEndConditionColon = 1 << 3,
        kEndConditionComma = 1 << 4
    };

    typedef int endCondition;

    CHECK_RETURN bool next();

    CHECK_RETURN bool parseStorage(topLevel &current); // const, in, out, attribute, uniform, varying, buffer, shared
    CHECK_RETURN bool parseAuxiliary(topLevel &current); // centroid, sample, patch
    CHECK_RETURN bool parseInterpolation(topLevel &current); // smooth, flat, noperspective
    CHECK_RETURN bool parsePrecision(topLevel &current); // highp, mediump, lowp
    CHECK_RETURN bool parseInvariant(topLevel &current); // invariant
    CHECK_RETURN bool parsePrecise(topLevel &current); // precise
    CHECK_RETURN bool parseMemory(topLevel &current); // coherent, volatile, restrict, readonly, writeonly

    CHECK_RETURN bool parseTopLevelItem(topLevel &level, topLevel *continuation = 0);
    CHECK_RETURN bool parseTopLevel(std::vector<topLevel> &top);

    CHECK_RETURN bool isType(int type) const;
    CHECK_RETURN bool isKeyword(int keyword) const;
    CHECK_RETURN bool isOperator(int oper) const;
    CHECK_RETURN bool isEndCondition(endCondition condition) const;
    CHECK_RETURN bool isBuiltin() const;

    CHECK_RETURN bool isConstantValue(astExpression *expression) const;
    CHECK_RETURN bool isConstant(astExpression *expression) const;

    void parseLayout(std::vector<astLayoutQualifier*> &layoutQualifiers);

    void fatal(const char *fmt, ...);

    CHECK_RETURN astConstantExpression *evaluate(astExpression *expression);

    // Type parsers
    astBuiltin *parseBuiltin();
    astStruct *parseStruct();

    CHECK_RETURN astFunction *parseFunction(const topLevel &parse);

    // Call parsers
    CHECK_RETURN astConstructorCall *parseConstructorCall();
    CHECK_RETURN astFunctionCall *parseFunctionCall();

    // Expression parsers
    CHECK_RETURN astExpression *parseExpression(endCondition end);
    CHECK_RETURN astExpression *parseUnary(endCondition end);
    CHECK_RETURN astExpression *parseBinary(int lhsPrecedence, astExpression *lhs, endCondition condition);
    CHECK_RETURN astExpression *parseUnaryPrefix(endCondition end);
    CHECK_RETURN astConstantExpression *parseArraySize();

    // Statement parsers
    CHECK_RETURN astStatement *parseStatement();
    CHECK_RETURN astSwitchStatement *parseSwitchStatement();
    CHECK_RETURN astCaseLabelStatement *parseCaseLabelStatement();
    CHECK_RETURN astForStatement *parseForStatement();
    CHECK_RETURN astCompoundStatement *parseCompoundStatement();
    CHECK_RETURN astIfStatement *parseIfStatement();
    CHECK_RETURN astSimpleStatement *parseDeclarationOrExpressionStatement(endCondition condition);
    CHECK_RETURN astDeclarationStatement *parseDeclarationStatement(endCondition condition);
    CHECK_RETURN astExpressionStatement *parseExpressionStatement(endCondition condition);
    CHECK_RETURN astContinueStatement *parseContinueStatement();
    CHECK_RETURN astBreakStatement *parseBreakStatement();
    CHECK_RETURN astDiscardStatement *parseDiscardStatement();
    CHECK_RETURN astReturnStatement *parseReturnStatement();
    CHECK_RETURN astDoStatement *parseDoStatement();
    CHECK_RETURN astWhileStatement *parseWhileStatement();

    astBinaryExpression *createExpression();

    astType *findType(const std::string &identifier);
    astVariable *findVariable(const std::string &identifier);

private:
    typedef std::vector<astVariable *> scope;

    astTU *m_ast;
    lexer m_lexer;
    token m_token;
    std::vector<scope> m_scopes;
    std::vector<astBuiltin*> m_builtins;
    std::string m_error;

    std::vector<astMemory> m_memory; // Memory of AST held here
};

}

#endif
