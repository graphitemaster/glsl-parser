#include <stdio.h> // TODO: proper error handling
#include "parser.h"

static inline void fatal(const char *error) {
    fprintf(stderr, "%s\n", error);
    abort();
}

namespace glsl {

parser::parser(const std::string &source)
    : m_lexer(source)
{
}

parser::~parser() {
    // TODO: reference count m_ast
}

#define IS_TYPE(TOKEN, TYPE) \
    ((TOKEN).m_type == (TYPE))
#define IS_KEYWORD(TOKEN, KEYWORD) \
    (IS_TYPE((TOKEN), kType_keyword) && (TOKEN).m_keyword == (KEYWORD))
#define IS_OPERATOR(TOKEN, OPERATOR) \
    (IS_TYPE((TOKEN), kType_operator) && (TOKEN).m_operator == (OPERATOR))

bool parser::isType(int type) const {
    return IS_TYPE(m_token, type);
}

bool parser::isKeyword(int keyword) const {
    return IS_KEYWORD(m_token, keyword);
}

bool parser::isOperator(int oper) const {
    return IS_OPERATOR(m_token, oper);
}

bool parser::isEndCondition(endCondition condition) const {
    return ((condition & kEndConditionSemicolon)    && isType(kType_semicolon))
        || ((condition & kEndConditionParanthesis)  && isOperator(kOperator_paranthesis_end))
        || ((condition & kEndConditionBracket)      && isOperator(kOperator_bracket_end))
        || ((condition & kEndConditionColon)        && isOperator(kOperator_colon))
        || ((condition & kEndConditionComma)        && isOperator(kOperator_comma));
}

#undef TYPENAME
#define TYPENAME(X) case kKeyword_##X:
bool parser::isBuiltin() const {
    if (!isType(kType_keyword))
        return false;
    switch (m_token.m_keyword) {
        #include "lexemes.h"
            return true;
        default:
            break;
    }
    return false;
}
#undef TYPENAME
#define TYPENAME(...)

/// The parser entry point
astTU *parser::parse() {
    m_ast = new astTU;

    m_scopes.push_back(scope());
    for (;;) {
        m_lexer.read(m_token, true);
        if (isType(kType_eof))
            break;

        std::vector<stage> stages = parseGlobalScope();

        if (isType(kType_semicolon)) {
            for (size_t i = 0; i < stages.size(); i++) {
                stage &parse = stages[i];
                astGlobalVariable *global = new(gc<astVariable>()) astGlobalVariable();
                global->flags = parse.flags;
                global->interpolation = parse.interpolation;
                global->precision = parse.precision;
                global->type = parse.type;
                global->name = parse.name;
                global->isArray = parse.isArray;
                global->arraySize = parse.arraySize;
                m_ast->globals.push_back(global);
                m_scopes.back().push_back(global);
            }
        } else if (isOperator(kOperator_paranthesis_begin)) {
            m_ast->functions.push_back(parseFunction(stages.front()));
        } else if (isType(kType_whitespace)) {
            continue; // whitespace tokens will be used later for the preprocessor
        } else {
            fatal("syntax error (top level)");
        }
    }

    return m_ast;
}

stage parser::parseGlobalItem(stage *continuation) {
    stage parse;

    // Inherit the previous type if this is a continuation "T a, b, c;"
    if (continuation)
        parse = *continuation;

    if (isKeyword(kKeyword_const)) {
        parse.flags |= kConst;
        next(); // skip 'const'
    }
    if (isKeyword(kKeyword_in)) {
        parse.flags |= kIn;
        next(); // skip 'in'
    }
    if (isKeyword(kKeyword_out)) {
        parse.flags |= kOut;
        next(); // skip 'out'
    }
    if (isKeyword(kKeyword_inout)) {
        parse.flags |= kIn;
        parse.flags |= kOut;
        next(); // skip 'inout'
    }
    if (isKeyword(kKeyword_centroid)) {
        parse.flags |= kCentroid;
        next(); // skip 'centroid'
    }
    if (isKeyword(kKeyword_patch)) {
        parse.flags |= kPatch;
        next(); // skip 'patch'
    }
    if (isKeyword(kKeyword_sample)) {
        parse.flags |= kSample;
        next(); // skip 'sample'
    }
    if (isKeyword(kKeyword_uniform)) {
        parse.flags |= kUniform;
        next(); // skip 'uniform'
    }
    if (isKeyword(kKeyword_layout)) {
        parseLayout(parse.layoutQualifiers);
        next();
    }
    if (isKeyword(kKeyword_invariant)) {
        parse.flags |= kInvariant;
        next(); // skip 'invariant'
    }

    // smooth/flat
    if (isKeyword(kKeyword_flat)) {
        parse.interpolation = kFlat;
        next(); // skip 'flat'
    } else if (isKeyword(kKeyword_smooth)) {
        parse.interpolation = kSmooth;
        next(); // skip 'smooth'
    }

    // lowp/mediump/highp
    if (isKeyword(kKeyword_lowp)) {
        parse.precision = kLowp;
        next(); // skip 'lowp'
    } else if (isKeyword(kKeyword_mediump)) {
        parse.precision = kMediump;
        next(); // skip 'mediump'
    } else if (isKeyword(kKeyword_highp)) {
        parse.precision = kHighp;
        next(); // skip 'highp'
    }

    if (!continuation) {
        if (isKeyword(kKeyword_struct)) {
            parse.type = parseStruct();
            next();
        } else if (isType(kType_identifier)) {
            parse.type = findType(m_token.m_identifier);
            next();
        } else {
            parse.type = parseBuiltin();
            next();
        }
    }

    if (isType(kType_identifier)) {
        parse.name = m_token.identifier();
        next(); // skip identifier
    }
    if (isOperator(kOperator_bracket_begin)) {
        parse.isArray = true;
        parse.arraySize = parseArraySize();
        next(); // skip ']' (parseArraySize skips '[')
    }
    return parse;
}

std::vector<stage> parser::parseGlobalScope() {
    std::vector<stage> stages;
    stages.push_back(parseGlobalItem());
    while (isOperator(kOperator_comma)) {
        next(); // skip ','
        stages.push_back(parseGlobalItem(&stages.front()));
    }
    return stages;
}

void parser::parseLayout(std::vector<astLayoutQualifier*> &layoutQualifiers) {
    fatal("not implemented: layout qualifier parsing");
}

astStruct *parser::parseStruct() {
    fatal("not implemented: structure parsing");
    return 0;
}

astExpression *parser::parseBinary(int lhsPrecedence, astExpression *lhs, endCondition end) {
    // Precedence climbing
    while (!isEndCondition(end)) {
        int binaryPrecedence = m_token.precedence();
        if (binaryPrecedence < lhsPrecedence)
            break;

        astBinaryExpression *expression = createExpression();
        next();
        astExpression *rhs = parseUnary(end);
        next();

        int rhsPrecedence = m_token.precedence();
        // climb
        if (binaryPrecedence < rhsPrecedence)
            rhs = parseBinary(binaryPrecedence + 1, rhs, end);

        expression->operand1 = lhs;
        expression->operand2 = rhs;
        lhs = expression;
    }
    return lhs;
}

astExpression *parser::parseUnaryPrefix(endCondition condition) {
    if (isOperator(kOperator_paranthesis_begin)) {
        next(); // skip '('
        return parseExpression(kEndConditionParanthesis);
    } else if (isOperator(kOperator_logical_not)) {
        next(); // skip '!'
        return new(gc<astExpression>()) astUnaryLogicalNotExpression(parseUnary(condition));
    } else if (isOperator(kOperator_bit_not)) {
        next(); // skip '~'
        return new(gc<astExpression>()) astUnaryBitNotExpression(parseUnary(condition));
    } else if (isOperator(kOperator_plus)) {
        next(); // skip '+'
        return new(gc<astExpression>()) astUnaryPlusExpression(parseUnary(condition));
    } else if (isOperator(kOperator_minus)) {
        next(); // skip '-'
        return new(gc<astExpression>()) astUnaryMinusExpression(parseUnary(condition));
    } else if (isOperator(kOperator_increment)) {
        next(); // skip '++'
        return new(gc<astExpression>()) astPrefixIncrementExpression(parseUnary(condition));
    } else if (isOperator(kOperator_decrement)) {
        next(); // skip '--'
        return new(gc<astExpression>()) astPrefixDecrementExpression(parseUnary(condition));
    } else if (isBuiltin()) {
        return parseConstructorCall();
    } else if (isType(kType_identifier)) {
        token peek = m_lexer.peek();
        if (IS_OPERATOR(peek, kOperator_paranthesis_begin)) {
            astType *type = findType(m_token.m_identifier);
            if (type)
                return parseConstructorCall();
            else
                return parseFunctionCall();
        } else {
            return new(gc<astExpression>()) astVariableIdentifier(findVariable(m_token.m_identifier));
        }
    } else if (isKeyword(kKeyword_true))
        return new(gc<astExpression>()) astBoolConstant(true);
    else if (isKeyword(kKeyword_false))
        return new(gc<astExpression>()) astBoolConstant(false);
    else if (isType(kType_constant_int))
        return new(gc<astExpression>()) astIntConstant(m_token.asInt);
    else if (isType(kType_constant_uint))
        return new(gc<astExpression>()) astUIntConstant(m_token.asUnsigned);
    else if (isType(kType_constant_float))
        return new(gc<astExpression>()) astFloatConstant(m_token.asFloat);
    else if (isType(kType_constant_double))
        return new(gc<astExpression>()) astDoubleConstant(m_token.asDouble);
    fatal("syntax error");
    return 0;
}

astExpression *parser::parseUnary(endCondition end) {
    astExpression *operand = parseUnaryPrefix(end);
    for (;;) {
        token peek = m_lexer.peek();
        if (IS_OPERATOR(peek, kOperator_dot)) {
            next(); // skip last
            next(); // skip '.'
            if (!isType(kType_identifier))
                fatal("expected field identifier or swizzle after `.'");
            astFieldOrSwizzle *expression = new(gc<astExpression>()) astFieldOrSwizzle();
            expression->operand = operand;
            expression->name = peek.m_identifier;
            operand = expression;
        } else if (IS_OPERATOR(peek, kOperator_increment)) {
            next(); // skip last
            operand = new(gc<astExpression>()) astPostIncrementExpression(operand);
        } else if (IS_OPERATOR(peek, kOperator_decrement)) {
            next(); // skip last
            operand = new(gc<astExpression>()) astPostDecrementExpression(operand);
        } else if (IS_OPERATOR(peek, kOperator_bracket_begin)) {
            next(); // skip last
            next(); // skip '['
            astArraySubscript *expression = new(gc<astExpression>()) astArraySubscript();
            expression->operand = operand;
            expression->index = parseExpression(kEndConditionBracket);
            operand = expression;
        } else {
            break;
        }
    }
    return operand;
}

astExpression *parser::parseExpression(endCondition condition) {
    astExpression *lhs = parseUnary(condition);
    next(); // skip last
    return parseBinary(0, lhs, condition);
}

astExpressionStatement *parser::parseExpressionStatement(endCondition condition) {
    return new(gc<astStatement>()) astExpressionStatement(parseExpression(condition));
}

astConstantExpression *parser::parseArraySize() {
    next(); // skip '['
    return parseExpression(kEndConditionBracket);
}

astCompoundStatement *parser::parseCompoundStatement() {
    astCompoundStatement *statement = new(gc<astStatement>()) astCompoundStatement();
    next(); // skip '{'
    while (!isType(kType_scope_end)) {
        statement->statements.push_back(parseStatement());
        next(); // skip ';'
    }
    return statement;
}

astIfStatement *parser::parseIfStatement() {
    astIfStatement *statement = new(gc<astStatement>()) astIfStatement();
    next(); // skip 'if'
    if (!isOperator(kOperator_paranthesis_begin))
        fatal("expected `(' after `if' in if statement");
    next(); // skip '('
    statement->condition = parseExpression(kEndConditionParanthesis);
    next(); // skip ')'
    statement->thenStatement = parseStatement();
    token peek = m_lexer.peek();
    if (IS_KEYWORD(peek, kKeyword_else)) {
        next(); // skip ';' or '}'
        next(); // skip 'else'
        statement->elseStatement = parseStatement();
    }
    return statement;
}

astSwitchStatement *parser::parseSwitchStatement() {
    astSwitchStatement *statement = new(gc<astStatement>()) astSwitchStatement();
    next(); // skip 'switch'
    if (!isOperator(kOperator_paranthesis_begin))
        fatal("expected `(' after `switch' in switch statement");
    next(); // skip '('
    statement->expression = parseExpression(kEndConditionParanthesis);
    next(); // skip next
    if (!isType(kType_scope_begin))
        fatal("expected `{' after `)' in switch statement");
    next(); // skip '{'
    while (!isType(kType_scope_end)) {
        statement->statements.push_back(parseStatement());
        next();
    }
    // TODO: verify scope of where enum's are found
    return statement;
}

astCaseLabelStatement *parser::parseCaseLabelStatement() {
    astCaseLabelStatement *statement = new(gc<astStatement>()) astCaseLabelStatement();
    if (isKeyword(kKeyword_default)) {
        statement->isDefault = true;
        next(); // skip 'default'
        if (!isOperator(kOperator_colon))
            fatal("expected `:' after `default' in case label");
    } else {
        next(); // skip 'case'
        statement->condition = parseExpression(kEndConditionColon);
    }
    return statement;
}

astForStatement *parser::parseForStatement() {
    astForStatement *statement = new(gc<astStatement>()) astForStatement();
    next(); // skip 'for'
    if (!isOperator(kOperator_paranthesis_begin))
        fatal("expected `(' after `for' in for statement");
    next(); // skip '('
    if (!isType(kType_semicolon))
        statement->init = parseDeclarationOrExpressionStatement(kEndConditionSemicolon);
    next(); // skip ';'
    if (!isType(kType_semicolon))
        statement->condition = parseExpression(kEndConditionSemicolon);
    next(); // skip ';'
    if (!isOperator(kOperator_paranthesis_end))
        statement->loop = parseExpression(kEndConditionParanthesis);
    next(); // skip ')'
    statement->body = parseStatement();
    return statement;
}

astContinueStatement *parser::parseContinueStatement() {
    astContinueStatement *statement = new(gc<astStatement>()) astContinueStatement();
    next(); // skip 'continue'
    return statement;
}

astBreakStatement *parser::parseBreakStatement() {
    astBreakStatement *statement = new(gc<astStatement>()) astBreakStatement();
    next(); // skip 'break'
    return statement;
}

astDiscardStatement *parser::parseDiscardStatement() {
    astDiscardStatement *statement = new(gc<astStatement>()) astDiscardStatement();
    next(); // skip 'discard'
    return statement;
}

astReturnStatement *parser::parseReturnStatement() {
    astReturnStatement *statement = new(gc<astStatement>()) astReturnStatement();
    next(); // skip 'return'
    if (!isType(kType_semicolon))
        statement->expression = parseExpression(kEndConditionSemicolon);
    return statement;
}

astDoStatement *parser::parseDoStatement() {
    astDoStatement *statement = new(gc<astStatement>()) astDoStatement();
    next(); // skip 'do'
    statement->body = parseStatement();
    next();
    if (!isKeyword(kKeyword_while))
        fatal("expected `while' after `do' in do-while loop");
    next(); // skip 'while'
    if (!isOperator(kOperator_paranthesis_begin))
        fatal("expected `(' after `while' in do-while loop");
    next(); // skip '('
    statement->condition = parseExpression(kEndConditionParanthesis);
    next();
    return statement;
}

astWhileStatement *parser::parseWhileStatement() {
    astWhileStatement *statement = new(gc<astStatement>()) astWhileStatement();
    next(); // skip 'while'
    if (!isOperator(kOperator_paranthesis_begin))
        fatal("expected `(' after `while' in while-loop");
    next(); // skip '('
    statement->condition = parseDeclarationOrExpressionStatement(kEndConditionParanthesis);
    next();
    statement->body = parseStatement();
    return statement;
}

astDeclarationStatement *parser::parseDeclarationStatement(endCondition condition) {
    m_lexer.backup();

    bool isConst = false;
    if (isKeyword(kKeyword_const)) {
        isConst = true;
        next(); // skip 'const'
    }

    astType *type = 0;
    if (isBuiltin())
        type = parseBuiltin();
    else if (isType(kType_identifier))
        type = findType(m_token.m_identifier);

    if (!type) {
        m_lexer.restore();
        return 0;
    }

    next();

    astDeclarationStatement *statement = new(gc<astStatement>()) astDeclarationStatement();
    for (;;) {
        size_t paranthesisCount = 0;
        while (isOperator(kOperator_paranthesis_begin)) {
            paranthesisCount++;
            next(); // skip ','
        }
        if (!isType(kType_identifier)) {
            m_lexer.restore();
            return 0;
        }

        std::string name = m_token.m_identifier;
        next(); // skip identifier

        for (size_t i = 0; i < paranthesisCount; i++) {
            if (!isOperator(kOperator_paranthesis_end)) {
                m_lexer.restore();
                return 0;
            }
            next();
        }

        if (statement->variables.empty() && !isOperator(kOperator_assign)
            && !isOperator(kOperator_comma) && !isEndCondition(condition))
        {
            m_lexer.restore();
            return 0;
        }

        astExpression *initialValue = 0;
        if (isOperator(kOperator_assign)) {
            next(); // skip '='
            initialValue = parseExpression(kEndConditionComma | condition);
        }

        astFunctionVariable *variable = new(gc<astVariable>()) astFunctionVariable();
        variable->isConst = isConst;
        variable->type = type;
        variable->name = name;
        variable->initialValue = initialValue;
        variable->isArray = false;
        variable->arraySize = 0;
        statement->variables.push_back(variable);
        m_scopes.back().push_back(variable);

        if (isEndCondition(condition))
            break;
        else if (isOperator(kOperator_comma))
            next(); // skip ','
        else if (isOperator(kOperator_bracket_begin)){
            next(); // skip '['
            variable->isArray = true;
            variable->arraySize = parseExpression(kEndConditionBracket);
            next(); // skip ']'
        } else {
            fatal("syntax error (declaration)");
        }
    }

    return statement;
}

astSimpleStatement *parser::parseDeclarationOrExpressionStatement(endCondition condition) {
    astSimpleStatement *declaration = parseDeclarationStatement(condition);
    if (declaration)
        return declaration;
    else
        return parseExpressionStatement(condition);
}

astStatement *parser::parseStatement() {
    if (isType(kType_scope_begin))
        return parseCompoundStatement();
    else if (isKeyword(kKeyword_if))
        return parseIfStatement();
    else if (isKeyword(kKeyword_switch))
        return parseSwitchStatement();
    else if (isKeyword(kKeyword_case) || isKeyword(kKeyword_default))
        return parseCaseLabelStatement();
    else if (isKeyword(kKeyword_for))
        return parseForStatement();
    else if (isKeyword(kKeyword_do))
        return parseDoStatement();
    else if (isKeyword(kKeyword_while))
        return parseWhileStatement();
    else if (isKeyword(kKeyword_continue))
        return parseContinueStatement();
    else if (isKeyword(kKeyword_break))
        return parseBreakStatement();
    else if (isKeyword(kKeyword_discard))
        return parseDiscardStatement();
    else if (isKeyword(kKeyword_return))
        return parseReturnStatement();
    else if (isType(kType_semicolon))
        return new(gc<astStatement>()) astEmptyStatement();
    else
        return parseDeclarationOrExpressionStatement(kEndConditionSemicolon);
}

astFunction *parser::parseFunction(const stage &parse) {
    astFunction *function = new(gc<astFunction>()) astFunction();
    function->returnType = parse.type;
    function->name = parse.name;

    next(); // skip '('
    while (!isOperator(kOperator_paranthesis_end)) {
        astFunctionParameter *parameter = new(gc<astVariable>()) astFunctionParameter();
        parameter->precision = kHighp; // TODO: memory qualifier

        while (!isOperator(kOperator_comma) && !isOperator(kOperator_paranthesis_end)) {
            if (isKeyword(kKeyword_in))
                parameter->flags |= kIn;
            else if (isKeyword(kKeyword_out))
                parameter->flags |= kOut;
            else if (isKeyword(kKeyword_inout))
                parameter->flags |= (kIn | kOut);
            else if (isKeyword(kKeyword_highp))
                parameter->precision = kHighp;
            else if (isKeyword(kKeyword_mediump))
                parameter->precision = kMediump;
            else if (isKeyword(kKeyword_lowp))
                parameter->precision = kLowp;
            else if (isType(kType_identifier)) {
                //TODO: user types
                //if (!parameter->type && !(parameter->type = findType(m_token.m_identifier)))
                parameter->name = m_token.m_identifier;
            } else if (isOperator(kOperator_bracket_begin)) {
                parameter->arraySize = parseArraySize();
            } else {
                parameter->type = parseBuiltin();
            }
            next();
        }

        if (!parameter->type)
            fatal("expected type");
        function->parameters.push_back(parameter);
        if (isOperator(kOperator_comma))
            next(); // skip ','
    }
    next(); // skip ')'

    if (isType(kType_scope_begin)) {
        function->isPrototype = false;
        next(); // skip '{'

        // Create a new lexical scope and push back the function parameters
        m_scopes.push_back(scope());
        for (size_t i = 0; i < function->parameters.size(); i++)
            m_scopes.back().push_back(function->parameters[i]);

        // Parse the statements
        while (!isType(kType_scope_end)) {
            function->statements.push_back(parseStatement());
            next(); // skip ';'
        }

        // Leave the lexical scope
        m_scopes.pop_back();
    } else if (isType(kType_semicolon)) {
        function->isPrototype = true;
    } else {
        fatal("expected `{' or `;'");
    }
    return function;
}

// TODO: clenaup
#undef TYPENAME
#define TYPENAME(X) case kKeyword_##X:
astBuiltin *parser::parseBuiltin() {
    if (!isType(kType_keyword))
        fatal("expected keyword");

    switch (m_token.m_keyword) {
        #include "lexemes.h"
            for (size_t i = 0; i < m_builtins.size(); i++)
                if (m_builtins[i]->type == m_token.m_keyword)
                    return m_builtins[i];
            m_builtins.push_back(new(gc<astType>()) astBuiltin(m_token.m_keyword));
            return m_builtins.back();
            break;
        default:
            break;
    }
    fatal("internal compiler error: attempted to parse as builtin type");
    return 0;
}
#undef TYPENAME

astConstructorCall *parser::parseConstructorCall() {
    astConstructorCall *expression = new(gc<astExpression>()) astConstructorCall();
    expression->type = parseBuiltin();
    next();
    if (!isOperator(kOperator_paranthesis_begin))
        fatal("expected `(' in constructor call");
    next(); // skip '('
    while (!isOperator(kOperator_paranthesis_end)) {
        astExpression *parameter = parseExpression(kEndConditionComma | kEndConditionParanthesis);
        expression->parameters.push_back(parameter);
        if (isOperator(kOperator_comma))
            next(); // skip ','
    }
    return expression;
}

astFunctionCall *parser::parseFunctionCall() {
    astFunctionCall *expression = new(gc<astExpression>()) astFunctionCall();
    expression->name = m_token.m_identifier;
    next(); // skip identifier
    if (!isOperator(kOperator_paranthesis_begin))
        fatal("expected `(' in function call");
    next(); // skip '('
    while (!isOperator(kOperator_paranthesis_end)) {
        astExpression *parameter = parseExpression(kEndConditionComma | kEndConditionParanthesis);
        expression->parameters.push_back(parameter);
        if (isOperator(kOperator_comma))
            next(); // skip ','
    }
    return expression;
}

void parser::next() {
    m_lexer.read(m_token, true);
    if (isType(kType_eof))
        fatal("premature end of file");
}

astBinaryExpression *parser::createExpression() {
    if (!isType(kType_operator))
        fatal("internal compiler error: attempted to create binary expression in wrong context");

    switch (m_token.m_operator) {
        case kOperator_comma:
            return new(gc<astExpression>()) astSequenceExpression();
        case kOperator_minus:
            return new(gc<astExpression>()) astMinusExpression();
        case kOperator_plus:
            return new(gc<astExpression>()) astPlusExpression();
        default:
            return 0;
    }
}

astType *parser::findType(const std::string &identifier) {
    //fatal("not implemented yet: custom types");
    return 0;
}

astVariable *parser::findVariable(const std::string &identifier) {
    for (size_t scopeIndex = m_scopes.size(); scopeIndex > 0; scopeIndex--) {
        scope &s = m_scopes[scopeIndex - 1];
        for (size_t variableIndex = 0; variableIndex < s.size(); variableIndex++)
            if (s[variableIndex]->name == identifier)
                return s[variableIndex];
    }
    return 0;
}

}
