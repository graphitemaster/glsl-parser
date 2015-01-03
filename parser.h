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
    astType *type;
    astConstantExpression *initialValue;
    std::vector<astConstantExpression*> arraySizes;
    std::vector<astLayoutQualifier*> layoutQualifiers;
    bool isInvariant;
    bool isPrecise;
    bool isArray;
    char *name;
};

struct parser {
    ~parser();
    parser(const char *source, const char *fileName);
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
    CHECK_RETURN bool parseLayout(topLevel &current);

    CHECK_RETURN bool parseTopLevelItem(topLevel &level, topLevel *continuation = 0);
    CHECK_RETURN bool parseTopLevel(std::vector<topLevel> &top);

    CHECK_RETURN bool isType(int type) const;
    CHECK_RETURN bool isKeyword(int keyword) const;
    CHECK_RETURN bool isOperator(int oper) const;
    CHECK_RETURN bool isEndCondition(endCondition condition) const;
    CHECK_RETURN bool isBuiltin() const;

    CHECK_RETURN bool isConstantValue(astExpression *expression) const;
    CHECK_RETURN bool isConstant(astExpression *expression) const;

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

    astType *findType(const char *identifier);
    astVariable *findVariable(const char *identifier);

private:
    typedef std::vector<astVariable *> scope;

    astTU *m_ast;
    lexer m_lexer;
    token m_token;
    std::vector<scope> m_scopes;
    std::vector<astBuiltin*> m_builtins;
    char *m_error;
    char *m_oom;
    const char *m_fileName;

    void strdel(char **what) {
        if (!*what)
            return;
        free(*what);
        *what = 0;
    }

    char *strnew(const char *what) {
        if (!what)
            return 0;
        size_t length = strlen(what) + 1;
        char *copy = (char*)malloc(length);
        memcpy(copy, what, length);
        m_strings.push_back(copy);
        return copy;
    }

    bool strnil(const char *what) {
        return !what || !*what;
    }

    std::vector<astMemory> m_memory; // Memory of AST held here
    std::vector<char *> m_strings; // Memory of strings held here
};

}

#endif
