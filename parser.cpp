#include <algorithm> // std::find

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "parser.h"

namespace glsl {

parser::parser(const std::string &source)
    : m_lexer(source)
{
}

parser::~parser() {
    delete m_ast;
    for (size_t i = 0; i < m_memory.size(); i++)
        m_memory[i].destroy();
}

#define IS_TYPE(TOKEN, TYPE) \
    ((TOKEN).m_type == (TYPE))
#define IS_KEYWORD(TOKEN, KEYWORD) \
    (IS_TYPE((TOKEN), kType_keyword) && (TOKEN).m_keyword == (KEYWORD))
#define IS_OPERATOR(TOKEN, OPERATOR) \
    (IS_TYPE((TOKEN), kType_operator) && (TOKEN).m_operator == (OPERATOR))

#define GC_NEW(X) new(&m_memory)

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

// Constant expression evaluator
bool parser::isConstant(astExpression *expression) const {
    if (isConstantValue(expression))
        return true;
    else if (expression->type == astExpression::kVariableIdentifier) {
        astVariable *reference = ((astVariableIdentifier*)expression)->variable;
        if (reference->type != astVariable::kGlobal)
            return false;
        astExpression *initialValue = ((astGlobalVariable*)reference)->initialValue;
        if (!initialValue)
            return false;
        return isConstant(initialValue);
    } else if (expression->type == astExpression::kUnaryMinus)
        return isConstant(((astUnaryExpression*)expression)->operand);
    else if (expression->type == astExpression::kUnaryPlus)
        return isConstant(((astUnaryExpression*)expression)->operand);
    else if (expression->type == astExpression::kOperation) {
        astOperationExpression *operation = (astOperationExpression*)expression;
        return isConstant(operation->operand1) && isConstant(operation->operand2);
    }
    return false;
}

bool parser::isConstantValue(astExpression *expression) const {
    return expression->type == astExpression::kIntConstant ||
           expression->type == astExpression::kUIntConstant ||
           expression->type == astExpression::kFloatConstant ||
           expression->type == astExpression::kDoubleConstant ||
           expression->type == astExpression::kBoolConstant;
}

#define ICONST(X) ((astIntConstant*)(X))
#define UCONST(X) ((astUIntConstant*)(X))
#define FCONST(X) ((astFloatConstant*)(X))
#define DCONST(X) ((astDoubleConstant*)(X))
#define BCONST(X) ((astBoolConstant*)(X))

#define ICONST_NEW(X) GC_NEW(astConstantExpression) astIntConstant(X)
#define UCONST_NEW(X) GC_NEW(astConstantExpression) astUIntConstant(X)
#define FCONST_NEW(X) GC_NEW(astConstantExpression) astFloatConstant(X)
#define DCONST_NEW(X) GC_NEW(astConstantExpression) astDoubleConstant(X)
#define BCONST_NEW(X) GC_NEW(astConstantExpression) astBoolConstant(X)

#define IVAL(X) (ICONST(X)->value)
#define UVAL(X) (UCONST(X)->value)
#define FVAL(X) (FCONST(X)->value)
#define DVAL(X) (DCONST(X)->value)
#define BVAL(X) (BCONST(X)->value)

astConstantExpression *parser::evaluate(astExpression *expression) {
    if (!expression) return 0;
    else if (isConstantValue(expression))
        return expression;
    else if (expression->type == astExpression::kVariableIdentifier)
        return evaluate(((astGlobalVariable*)((astVariableIdentifier*)expression)->variable)->initialValue);
    else if (expression->type == astExpression::kUnaryMinus) {
        astExpression *operand = evaluate(((astUnaryExpression*)expression)->operand);
        if (!operand) return 0;
        switch (operand->type) {
            case astExpression::kIntConstant:    return ICONST_NEW(-IVAL(operand));
            case astExpression::kFloatConstant:  return FCONST_NEW(-FVAL(operand));
            case astExpression::kDoubleConstant: return DCONST_NEW(-DVAL(operand));
            default:
                fatal("invalid operation in constant expression");
                return 0;
        }
    } else if (expression->type == astExpression::kUnaryPlus) {
        astExpression *operand = evaluate(((astUnaryExpression*)expression)->operand);
        if (!operand) return 0;
        switch (operand->type) {
            case astExpression::kIntConstant:
            case astExpression::kUIntConstant:
            case astExpression::kFloatConstant:
            case astExpression::kDoubleConstant:
                return operand;
            default:
                fatal("invalid operation in constant expression");
                return 0;
        }
    } else if (expression->type == astExpression::kOperation) {
        int operation = ((astOperationExpression*)expression)->operation;
        astExpression *lhs = evaluate(((astBinaryExpression*)expression)->operand1);
        astExpression *rhs = evaluate(((astBinaryExpression*)expression)->operand2);
        if (!lhs) return 0;
        if (!rhs) return 0;
        switch (lhs->type) {
        case astExpression::kIntConstant:
            switch (operation) {
            case kOperator_multiply:       return ICONST_NEW(IVAL(lhs) * IVAL(rhs));
            case kOperator_divide:         return ICONST_NEW(IVAL(lhs) / IVAL(rhs));
            case kOperator_modulus:        return ICONST_NEW(IVAL(lhs) % IVAL(rhs));
            case kOperator_plus:           return ICONST_NEW(IVAL(lhs) + IVAL(rhs));
            case kOperator_minus:          return ICONST_NEW(IVAL(lhs) - IVAL(rhs));
            case kOperator_shift_left:     return ICONST_NEW(IVAL(lhs) << IVAL(rhs));
            case kOperator_shift_right:    return ICONST_NEW(IVAL(lhs) >> IVAL(rhs));
            case kOperator_less:           return BCONST_NEW(IVAL(lhs) < IVAL(rhs));
            case kOperator_greater:        return BCONST_NEW(IVAL(lhs) > IVAL(rhs));
            case kOperator_less_equal:     return BCONST_NEW(IVAL(lhs) <= IVAL(rhs));
            case kOperator_greater_equal:  return BCONST_NEW(IVAL(lhs) >= IVAL(rhs));
            case kOperator_equal:          return BCONST_NEW(IVAL(lhs) == IVAL(rhs));
            case kOperator_not_equal:      return BCONST_NEW(IVAL(lhs) != IVAL(rhs));
            case kOperator_bit_and:        return ICONST_NEW(IVAL(lhs) & IVAL(rhs));
            case kOperator_bit_xor:        return ICONST_NEW(IVAL(lhs) ^ IVAL(rhs));
            case kOperator_logical_and:    return BCONST_NEW(IVAL(lhs) && IVAL(rhs));
            case kOperator_logical_xor:    return BCONST_NEW(!IVAL(lhs) != !IVAL(rhs));
            case kOperator_logical_or:     return BCONST_NEW(IVAL(lhs) || IVAL(rhs));
            default:
                fatal("invalid operation in constant expression");
                return 0;
            }
            break;
        case astExpression::kUIntConstant:
            switch (operation) {
            case kOperator_multiply:       return UCONST_NEW(UVAL(lhs) * UVAL(rhs));
            case kOperator_divide:         return UCONST_NEW(UVAL(lhs) / UVAL(rhs));
            case kOperator_modulus:        return UCONST_NEW(UVAL(lhs) % UVAL(rhs));
            case kOperator_plus:           return UCONST_NEW(UVAL(lhs) + UVAL(rhs));
            case kOperator_minus:          return UCONST_NEW(UVAL(lhs) - UVAL(rhs));
            case kOperator_shift_left:     return UCONST_NEW(UVAL(lhs) << UVAL(rhs));
            case kOperator_shift_right:    return UCONST_NEW(UVAL(lhs) >> UVAL(rhs));
            case kOperator_less:           return BCONST_NEW(UVAL(lhs) < UVAL(rhs));
            case kOperator_greater:        return BCONST_NEW(UVAL(lhs) > UVAL(rhs));
            case kOperator_less_equal:     return BCONST_NEW(UVAL(lhs) <= UVAL(rhs));
            case kOperator_greater_equal:  return BCONST_NEW(UVAL(lhs) >= UVAL(rhs));
            case kOperator_equal:          return BCONST_NEW(UVAL(lhs) == UVAL(rhs));
            case kOperator_not_equal:      return BCONST_NEW(UVAL(lhs) != UVAL(rhs));
            case kOperator_bit_and:        return UCONST_NEW(UVAL(lhs) & UVAL(rhs));
            case kOperator_bit_xor:        return UCONST_NEW(UVAL(lhs) ^ UVAL(rhs));
            case kOperator_logical_and:    return BCONST_NEW(UVAL(lhs) && UVAL(rhs));
            case kOperator_logical_xor:    return BCONST_NEW(!UVAL(lhs) != !UVAL(rhs));
            case kOperator_logical_or:     return BCONST_NEW(UVAL(lhs) || UVAL(rhs));
            default:
                fatal("invalid operation in constant expression");
                return 0;
            }
            break;
        case astExpression::kFloatConstant:
            switch (operation) {
            case kOperator_multiply:       return FCONST_NEW(FVAL(lhs) * FVAL(rhs));
            case kOperator_divide:         return FCONST_NEW(FVAL(lhs) / FVAL(rhs));
            case kOperator_plus:           return FCONST_NEW(FVAL(lhs) + FVAL(rhs));
            case kOperator_minus:          return FCONST_NEW(FVAL(lhs) - FVAL(rhs));
            case kOperator_less:           return BCONST_NEW(FVAL(lhs) < FVAL(rhs));
            case kOperator_greater:        return BCONST_NEW(FVAL(lhs) > FVAL(rhs));
            case kOperator_less_equal:     return BCONST_NEW(FVAL(lhs) <= FVAL(rhs));
            case kOperator_greater_equal:  return BCONST_NEW(FVAL(lhs) >= FVAL(rhs));
            case kOperator_equal:          return BCONST_NEW(FVAL(lhs) == FVAL(rhs));
            case kOperator_not_equal:      return BCONST_NEW(FVAL(lhs) != FVAL(rhs));
            case kOperator_logical_and:    return BCONST_NEW(FVAL(lhs) && FVAL(rhs));
            case kOperator_logical_xor:    return BCONST_NEW(!FVAL(lhs) != !FVAL(rhs));
            case kOperator_logical_or:     return BCONST_NEW(FVAL(lhs) || FVAL(rhs));
            default:
                fatal("invalid operation in constant expression");
                return 0;
            }
            break;
        case astExpression::kDoubleConstant:
            switch (operation) {
            case kOperator_multiply:       return DCONST_NEW(DVAL(lhs) * DVAL(rhs));
            case kOperator_divide:         return DCONST_NEW(DVAL(lhs) / DVAL(rhs));
            case kOperator_plus:           return DCONST_NEW(DVAL(lhs) + DVAL(rhs));
            case kOperator_minus:          return DCONST_NEW(DVAL(lhs) - DVAL(rhs));
            case kOperator_less:           return BCONST_NEW(DVAL(lhs) < DVAL(rhs));
            case kOperator_greater:        return BCONST_NEW(DVAL(lhs) > DVAL(rhs));
            case kOperator_less_equal:     return BCONST_NEW(DVAL(lhs) <= DVAL(rhs));
            case kOperator_greater_equal:  return BCONST_NEW(DVAL(lhs) >= DVAL(rhs));
            case kOperator_equal:          return BCONST_NEW(DVAL(lhs) == DVAL(rhs));
            case kOperator_not_equal:      return BCONST_NEW(DVAL(lhs) != DVAL(rhs));
            case kOperator_logical_and:    return BCONST_NEW(DVAL(lhs) && DVAL(rhs));
            case kOperator_logical_xor:    return BCONST_NEW(!DVAL(lhs) != !DVAL(rhs));
            case kOperator_logical_or:     return BCONST_NEW(DVAL(lhs) || DVAL(rhs));
            default:
                fatal("invalid operation in constant expression");
                return 0;
            }
            break;
        case astExpression::kBoolConstant:
            switch (operation) {
            case kOperator_equal:          return BCONST_NEW(BVAL(lhs) == BVAL(rhs));
            case kOperator_not_equal:      return BCONST_NEW(BVAL(lhs) != BVAL(rhs));
            case kOperator_logical_and:    return BCONST_NEW(BVAL(lhs) && BVAL(rhs));
            case kOperator_logical_xor:    return BCONST_NEW(!BVAL(lhs) != !BVAL(rhs));
            case kOperator_logical_or:     return BCONST_NEW(BVAL(lhs) || BVAL(rhs));
            default:
                fatal("invalid operation in constant expression");
                return 0;
            }
            break;
        }
    } else {
        return evaluate(expression);
    }
    return 0;
}

void parser::fatal(const char *fmt, ...) {
    char buffer[1024];
    snprintf(buffer, sizeof(buffer), "<string>:%zu:%zu: error: ", m_lexer.line(), m_lexer.column());
    m_error = buffer;
    va_list va;
    va_start(va, fmt);
    vsnprintf(buffer, sizeof(buffer), fmt, va);
    va_end(va);
    m_error += buffer;
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
astTU *parser::parse(int type) {
    m_ast = new astTU(type);
    m_scopes.push_back(scope());
    for (;;) {
        m_lexer.read(m_token, true);
        if (isType(kType_eof))
            break;

        std::vector<topLevel> items;
        if (!parseTopLevel(items))
            return 0;

        if (isType(kType_semicolon)) {
            for (size_t i = 0; i < items.size(); i++) {
                topLevel &parse = items[i];
                astGlobalVariable *global = GC_NEW(astVariable) astGlobalVariable();
                global->storage = parse.storage;
                global->auxiliary = parse.auxiliary;
                global->memory = parse.memory;
                global->precision = parse.precision;
                global->interpolation = parse.interpolation;
                global->baseType = parse.type;
                global->name = parse.name;
                if (parse.initialValue) {
                    if (!(global->initialValue = evaluate(parse.initialValue)))
                        return 0;
                }
                global->isArray = parse.isArray;
                global->arraySizes = parse.arraySizes;
                m_ast->globals.push_back(global);
                m_scopes.back().push_back(global);
            }
        } else if (isOperator(kOperator_paranthesis_begin)) {
            astFunction *function = parseFunction(items.front());
            if (!function)
                return 0;
            m_ast->functions.push_back(function);
        } else if (isType(kType_whitespace)) {
            continue; // whitespace tokens will be used later for the preprocessor
        } else {
            fatal("syntax error");
            return 0;
        }
    }
    return m_ast;
}

bool parser::parseStorage(topLevel &current) {
    // const, in, out, attribute, uniform, varying, buffer, shared
    if (isKeyword(kKeyword_const)) {
        current.storage = kConst;
        if (!next()) return false; // skip 'const'
    } else if (isKeyword(kKeyword_in)) {
        current.storage = kIn;
        if (!next()) return false; // skip 'in'
    } else if (isKeyword(kKeyword_out)) {
        current.storage = kOut;
        if (!next()) return false; // skip 'out'
    } else if (isKeyword(kKeyword_attribute)) {
        current.storage = kAttribute;
        if (!next()) return false; // skip 'attribute'
    } else if (isKeyword(kKeyword_uniform)) {
        current.storage = kUniform;
        if (!next()) return false; // skip 'uniform'
    } else if (isKeyword(kKeyword_varying)) {
        current.storage = kVarying;
        if (!next()) return false; // skip 'varying'
    } else if (isKeyword(kKeyword_buffer)) {
        current.storage = kBuffer;
        if (!next()) return false; // skip 'buffer'
    } else if (isKeyword(kKeyword_shared)) {
        current.storage = kShared;
        if (!next()) return false; // skip 'shared'
    }
    return true;
}

bool parser::parseAuxiliary(topLevel &current) {
    // centroid, sample, patch
    if (isKeyword(kKeyword_centroid)) {
        current.auxiliary = kCentroid;
        if (!next()) return false; // skip 'centroid'
    } else if (isKeyword(kKeyword_sample)) {
        current.auxiliary = kSample;
        if (!next()) return false; // skip 'sample'
    } else if (isKeyword(kKeyword_patch)) {
        current.auxiliary = kPatch;
        if (!next()) return false; // skip 'patch'
    }
    return true;
}

bool parser::parseInterpolation(topLevel &current) {
    // smooth, flat, noperspective
    if (isKeyword(kKeyword_smooth)) {
        current.interpolation = kSmooth;
        if (!next()) return false; // skip 'smooth'
    } else if (isKeyword(kKeyword_flat)) {
        current.interpolation = kFlat;
        if (!next()) return false; // skip 'flat'
    } else if (isKeyword(kKeyword_noperspective)) {
        current.interpolation = kNoPerspective;
        if (!next()) return false; // skip 'noperspective'
    }
    return true;
}

bool parser::parsePrecision(topLevel &current) {
    // highp, mediump, lowp
    if (isKeyword(kKeyword_highp)) {
        current.precision = kHighp;
        if (!next()) return false; // skip 'highp'
    } else if (isKeyword(kKeyword_mediump)) {
        current.precision = kMediump;
        if (!next()) return false; // skip 'mediump'
    } else if (isKeyword(kKeyword_lowp)) {
        current.precision = kLowp;
        if (!next()) return false; // skip 'lowp'
    }
    return true;
}

bool parser::parseInvariant(topLevel &) {
    // invariant
    if (isKeyword(kKeyword_invariant)) {
        // TODO:
        if (!next()) return false; // skip 'invariant'
    }
    return true;
}

bool parser::parsePrecise(topLevel &) {
    // precise
    if (isKeyword(kKeyword_precise)) {
        // TODO:
        if (!next()) return false; // skip 'precise'
    }
    return true;
}

bool parser::parseMemory(topLevel &current) {
    // coherent, volatile, restrict, readonly, writeonly
    if (isKeyword(kKeyword_coherent)) {
        current.memory |= kCoherent;
        if (!next()) return false; // skip 'coherent'
    } else if (isKeyword(kKeyword_volatile)) {
        current.memory |= kVolatile;
        if (!next()) return false; // skip 'volatile'
    } else if (isKeyword(kKeyword_restrict)) {
        current.memory |= kRestrict;
        if (!next()) return false; // skip 'restrict'
    } else if (isKeyword(kKeyword_readonly)) {
        current.memory |= kReadOnly;
        if (!next()) return false; // skip 'readonly'
    } else if (isKeyword(kKeyword_writeonly)) {
        current.memory |= kWriteOnly;
        if (!next()) return false; // skip 'writeonly;
    }
    return true;
}

bool parser::parseTopLevelItem(topLevel &level, topLevel *continuation) {
    std::vector<topLevel> items;
    while (!isBuiltin() && !isType(kType_identifier)) {
        topLevel next;
        if (continuation)
            next = *continuation;
        if (!parseStorage(next))       return false;
        if (!parseAuxiliary(next))     return false;
        if (!parseInterpolation(next)) return false;
        if (!parsePrecision(next))     return false;
        if (!parseInvariant(next))     return false;
        if (!parsePrecise(next))       return false;
        if (!parseMemory(next))        return false;
        items.push_back(next);
    }

    if (continuation)
        level = *continuation;
    for (size_t i = 0; i < items.size(); i++) {
        topLevel &next = items[i];
        const int storage = level.storage != -1 ? level.storage : next.storage;
        if (m_ast->type == astTU::kVertex && storage == kIn) {
            // "It's a compile-time error to use any auxiliary or interpolation
            //  qualifiers on a vertex shader input"
            if (level.auxiliary != -1 || next.auxiliary != -1) {
                fatal("cannot use auxiliary storage qualifier on vertex shader input");
                return false;
            } else if (level.interpolation != -1 || next.interpolation != -1) {
                fatal("cannot use interpolation qualifier on vertex shader input");
                return false;
            }
        }
        if (m_ast->type == astTU::kFragment && storage == kOut) {
            // "It's a compile-time error to use auxiliary storage qualifiers or
            //  interpolation qualifiers on an output in a fragment shader."
            if (level.auxiliary != -1 || next.auxiliary != -1) {
                fatal("cannot use auxiliary storage qualifier on fragment shader output");
                return false;
            } else if (level.interpolation != -1 || next.interpolation != -1) {
                fatal("cannot use interpolation qualifier on fragment shader output");
                return false;
            }
        }
        if (m_ast->type != astTU::kTessEvaluation && storage == kIn) {
            // "Applying the patch qualifier to inputs can only be done in tessellation
            //  evaluation shaders. It is a compile-time error to use patch with inputs
            //  in any other stage."
            if (level.auxiliary == kPatch || next.auxiliary == kPatch) {
                fatal("applying `patch' qualifier to input can only be done in tessellation evaluation shaders");
                return false;
            }
        }
        if (m_ast->type != astTU::kTessControl && storage == kOut) {
            // "Applying patch to an output can only be done in a tessellation control
            //  shader. It is a compile-time errot to use patch on outputs in any
            //  other stage."
            if (level.auxiliary == kPatch || next.auxiliary == kPatch) {
                fatal("applying `patch' qualifier to output can only be done in tessellation control shaders");
                return false;
            }
        }
        if (next.storage != -1 && level.storage != -1) {
            fatal("multiple storage qualifiers in declaration");
            return false;
        } else if (next.auxiliary != -1 && level.auxiliary != -1) {
            fatal("multiple auxiliary storage qualifiers in declaration");
            return false;
        } else if (next.interpolation != -1 && level.interpolation != -1) {
            fatal("multiple interpolation qualifiers in declaration");
            return false;
        } if (next.precision != -1 && level.precision != -1) {
            fatal("multiple precision qualifiers in declaration");
            return false;
        }
        level.storage = next.storage;
        level.auxiliary = next.auxiliary;
        level.interpolation = next.interpolation;
        level.precision = next.precision;
        level.memory |= next.memory;
    }

    // "It's a compile-time error to use interpolation qualifiers with patch"
    if (level.auxiliary == kPatch && level.interpolation != -1) {
        fatal("cannot use interpolation qualifier with auxiliary storage qualifier `patch'");
        return false;
    }

    if (!continuation) {
        if (isType(kType_identifier)) {
            level.type = findType(m_token.m_identifier);
            if (!next()) // skip identifier
                return false;
        } else {
            level.type = parseBuiltin();
            if (!next()) // skip typename
                return false;
        }

        if (level.type) {
            // Could be an array
            while (isOperator(kOperator_bracket_begin)) {
                level.isArray = true;
                astConstantExpression *arraySize = parseArraySize();
                if (!arraySize)
                    return false;
                level.arraySizes.insert(level.arraySizes.begin(), arraySize);
                if (!next()) // skip ']'
                    return false;
            }
        }
    }

    if (!level.type) {
        fatal("expected typename");
        return false;
    }

    if (isType(kType_identifier)) {
        level.name = m_token.m_identifier;
        if (!next())// skip identifier
            return false;
    }

    while (isOperator(kOperator_bracket_begin)) {
        level.isArray = true;
        level.arraySizes.push_back(parseArraySize());
        if (!next()) // skip ']'
            return false;
    }

    if (level.storage == kConst) {
        // Can have a constant expression assignment
        if (isOperator(kOperator_assign)) {
            if (!next()) // skip '='
                return false;
            if (!(level.initialValue = parseExpression(kEndConditionSemicolon)))
                return false;
            if (!isConstant(level.initialValue)) {
                fatal("not a valid constant expression");
                return false;
            }
        } else {
            fatal("const-qualified variable declared but not initialized");
            return false;
        }
    }

    // If it isn't a function or prototype than the use of void is not legal
    if (!isOperator(kOperator_paranthesis_begin)) {
        if (level.type->builtin && ((astBuiltin*)level.type)->type == kKeyword_void) {
            fatal("`void' cannot be used in declaration");
            return false;
        }
    }

    // if it doesn't have a name than it's illegal
    if (level.name.empty()) {
        fatal("expected name for declaration");
        return false;
    }

    return true;
}

bool parser::parseTopLevel(std::vector<topLevel> &items) {
    topLevel item;
    if (!parseTopLevelItem(item))
        return false;
    items.push_back(item);
    while (isOperator(kOperator_comma)) {
        if (!next())
            return false; // skip ','
        topLevel nextItem;
        if (!parseTopLevelItem(nextItem, &items.front()))
            return false;
        items.push_back(nextItem);
    }
    return true;
}

void parser::parseLayout(std::vector<astLayoutQualifier*> &) {
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
        if (!next())
            return 0;
        astExpression *rhs = parseUnary(end);
        if (!rhs)
            return 0;
        if (!next())
            return 0;

        if (((astExpression*)expression)->type == astExpression::kAssign) {
            astExpression *find = lhs;
            while (find->type == astExpression::kArraySubscript)
                find = ((astArraySubscript*)find)->operand;
            if (find->type != astExpression::kVariableIdentifier) {
                fatal("not a valid lvalue");
                return 0;
            }
            astVariable *variable = ((astVariableIdentifier*)lhs)->variable;
            if (variable->type == astVariable::kGlobal) {
                astGlobalVariable *global = (astGlobalVariable*)variable;
                // "It's a compile-time error to write to a variable declared as an input"
                if (global->storage == kIn) {
                    fatal("cannot write to a variable declared as input");
                    return 0;
                }
                // "It's a compile-time error to write to a const variable outside of its declaration."
                if (global->storage == kConst) {
                    fatal("cannot write to a const variable outside of its declaration");
                    return 0;
                }
            }
        }

        int rhsPrecedence = m_token.precedence();

        // climb
        if (binaryPrecedence < rhsPrecedence) {
            if (!(rhs = parseBinary(binaryPrecedence + 1, rhs, end)))
                return 0;
        }

        expression->operand1 = lhs;
        expression->operand2 = rhs;
        lhs = expression;
    }
    return lhs;
}

astExpression *parser::parseUnaryPrefix(endCondition condition) {
    if (isOperator(kOperator_paranthesis_begin)) {
        if (!next()) return 0; // skip '('
        return parseExpression(kEndConditionParanthesis);
    } else if (isOperator(kOperator_logical_not)) {
        if (!next()) return 0; // skip '!'
        return GC_NEW(astExpression) astUnaryLogicalNotExpression(parseUnary(condition));
    } else if (isOperator(kOperator_bit_not)) {
        if (!next()) return 0; // skip '~'
        return GC_NEW(astExpression) astUnaryBitNotExpression(parseUnary(condition));
    } else if (isOperator(kOperator_plus)) {
        if (!next()) return 0; // skip '+'
        return GC_NEW(astExpression) astUnaryPlusExpression(parseUnary(condition));
    } else if (isOperator(kOperator_minus)) {
        if (!next()) return 0; // skip '-'
        return GC_NEW(astExpression) astUnaryMinusExpression(parseUnary(condition));
    } else if (isOperator(kOperator_increment)) {
        if (!next()) return 0; // skip '++'
        return GC_NEW(astExpression) astPrefixIncrementExpression(parseUnary(condition));
    } else if (isOperator(kOperator_decrement)) {
        if (!next()) return 0; // skip '--'
        return GC_NEW(astExpression) astPrefixDecrementExpression(parseUnary(condition));
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
            astVariable *find = findVariable(m_token.m_identifier);
            if (find)
                return GC_NEW(astExpression) astVariableIdentifier(find);
            fatal("`%s' was not declared in this scope", m_token.m_identifier.c_str());
            return 0;
        }
    } else if (isKeyword(kKeyword_true)) {
        return BCONST_NEW(true);
    } else if (isKeyword(kKeyword_false)) {
        return BCONST_NEW(false);
    } else if (isType(kType_constant_int)) {
        return ICONST_NEW(m_token.asInt);
    } else if (isType(kType_constant_uint)) {
        return UCONST_NEW(m_token.asUnsigned);
    } else if (isType(kType_constant_float)) {
        return FCONST_NEW(m_token.asFloat);
    } else if (isType(kType_constant_double)) {
        return DCONST_NEW(m_token.asDouble);
    } else if (condition == kEndConditionBracket) {
        return 0;
    }
    fatal("syntax error");
    return 0;
}

astExpression *parser::parseUnary(endCondition end) {
    astExpression *operand = parseUnaryPrefix(end);
    if (!operand)
        return 0;
    for (;;) {
        token peek = m_lexer.peek();
        if (IS_OPERATOR(peek, kOperator_dot)) {
            if (!next()) return 0; // skip last
            if (!next()) return 0; // skip '.'
            if (!isType(kType_identifier)) {
                fatal("expected field identifier or swizzle after `.'");
                return 0;
            }
            astFieldOrSwizzle *expression = GC_NEW(astExpression) astFieldOrSwizzle();
            expression->operand = operand;
            expression->name = peek.m_identifier;
            operand = expression;
        } else if (IS_OPERATOR(peek, kOperator_increment)) {
            if (!next()) return 0; // skip last
            operand = GC_NEW(astExpression) astPostIncrementExpression(operand);
        } else if (IS_OPERATOR(peek, kOperator_decrement)) {
            if (!next()) return 0; // skip last
            operand = GC_NEW(astExpression) astPostDecrementExpression(operand);
        } else if (IS_OPERATOR(peek, kOperator_bracket_begin)) {
            if (!next()) return 0; // skip last
            if (!next()) return 0; // skip '['
            astArraySubscript *expression = GC_NEW(astExpression) astArraySubscript();
            astExpression *find = operand;
            while (find->type == astExpression::kArraySubscript)
                find = ((astArraySubscript*)find)->operand;
            if (find->type != astExpression::kVariableIdentifier) {
                fatal("cannot be subscripted");
                return 0;
            }
            expression->operand = operand;
            if (!(expression->index = parseExpression(kEndConditionBracket)))
                return 0;
            if (isConstant(expression->index)) {
                if (!(expression->index = evaluate(expression->index)))
                    return 0;
            }
            operand = expression;
        } else {
            break;
        }
    }
    return operand;
}

astExpression *parser::parseExpression(endCondition condition) {
    astExpression *lhs = parseUnary(condition);
    if (!lhs)
        return 0;
    if (!next()) // skip last
        return 0;
    return parseBinary(0, lhs, condition);
}

astExpressionStatement *parser::parseExpressionStatement(endCondition condition) {
    astExpression *expression = parseExpression(condition);
    return expression ? GC_NEW(astStatement) astExpressionStatement(expression) : 0;
}

astConstantExpression *parser::parseArraySize() {
    if (!next()) // skip '['
        return 0;
    return parseExpression(kEndConditionBracket);
}

astCompoundStatement *parser::parseCompoundStatement() {
    astCompoundStatement *statement = GC_NEW(astStatement) astCompoundStatement();
    if (!next()) // skip '{'
        return 0;
    while (!isType(kType_scope_end)) {
        astStatement *nextStatement = parseStatement();
        if (!nextStatement) return 0;
        statement->statements.push_back(nextStatement);
        if (!next()) // skip ';'
            return 0;
    }
    return statement;
}

astIfStatement *parser::parseIfStatement() {
    astIfStatement *statement = GC_NEW(astStatement) astIfStatement();
    if (!next()) // skip 'if'
        return 0;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `if'");
        return 0;
    }
    if (!next()) // skip '('
        return 0;
    if (!(statement->condition = parseExpression(kEndConditionParanthesis)))
        return 0;
    if (!next()) // skip ')'
        return 0;
    statement->thenStatement = parseStatement();
    token peek = m_lexer.peek();
    if (IS_KEYWORD(peek, kKeyword_else)) {
        if (!next()) // skip ';' or '}'
            return 0;
        if (!next()) // skip 'else'
            return 0;
        if (!(statement->elseStatement = parseStatement()))
            return 0;
    }
    return statement;
}

astSwitchStatement *parser::parseSwitchStatement() {
    astSwitchStatement *statement = GC_NEW(astStatement) astSwitchStatement();
    if (!next()) // skip 'switch'
        return 0;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `switch'");
        return 0;
    }
    if (!next()) // skip '('
        return 0;
    if (!(statement->expression = parseExpression(kEndConditionParanthesis)))
        return 0;
    if (!next()) // skip next
        return 0;
    if (!isType(kType_scope_begin)) {
        fatal("expected `{' after `)'");
        return 0;
    }
    if (!next()) // skip '{'
        return 0;

    std::vector<int> seenInts;
    std::vector<unsigned int> seenUInts;
    bool hadDefault = false;
    while (!isType(kType_scope_end)) {
        astStatement *nextStatement = parseStatement();
        if (!nextStatement) return 0;
        if (nextStatement->type == astStatement::kCaseLabel) {
            astCaseLabelStatement *caseLabel = (astCaseLabelStatement*)nextStatement;
            if (!caseLabel->isDefault) {
                if (!isConstant(caseLabel->condition)) {
                    fatal("case label is not a valid constant expression");
                    return 0;
                }
                astConstantExpression *value = evaluate(caseLabel->condition);
                // "It is a compile-time error to have two case label constant-expression of equal value"
                if (value->type == astExpression::kIntConstant) {
                    const int val = IVAL(value);
                    if (std::find(seenInts.begin(), seenInts.end(), val) != seenInts.end()) {
                        fatal("duplicate case label `%d'", val);
                        return 0;
                    }
                    seenInts.push_back(val);
                } else if (value->type == astExpression::kUIntConstant) {
                    const unsigned int val = UVAL(value);
                    if (std::find(seenUInts.begin(), seenUInts.end(), val) != seenUInts.end()) {
                        fatal("duplicate case label `%u'", val);
                        return 0;
                    }
                    seenUInts.push_back(val);
                } else {
                    fatal("case label must be scalar `int' or `uint'");
                    return 0;
                }
            } else {
                // "It's a compile-time error to have more than one default"
                if (hadDefault) {
                    fatal("duplicate `default' case label");
                    return 0;
                }
                hadDefault = true;
            }
        }
        statement->statements.push_back(nextStatement);
        if (!next())
            return 0;
    }

    // TODO: verify scope of where switches are found
    return statement;
}

astCaseLabelStatement *parser::parseCaseLabelStatement() {
    astCaseLabelStatement *statement = GC_NEW(astStatement) astCaseLabelStatement();
    if (isKeyword(kKeyword_default)) {
        statement->isDefault = true;
        if (!next()) // skip 'default'
            return 0;
        if (!isOperator(kOperator_colon)) {
            fatal("expected `:' after `default' in case label");
            return 0;
        }
    } else {
        if (!next()) // skip 'case'
            return 0;
        statement->condition = parseExpression(kEndConditionColon);
    }
    return statement;
}

astForStatement *parser::parseForStatement() {
    astForStatement *statement = GC_NEW(astStatement) astForStatement();
    if (!next()) // skip 'for'
        return 0;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `for'");
        return 0;
    }
    if (!next()) // skip '('
        return 0;
    if (!isType(kType_semicolon))
        if (!(statement->init = parseDeclarationOrExpressionStatement(kEndConditionSemicolon)))
            return 0;
    if (!next()) // skip ';'
        return 0;
    if (!isType(kType_semicolon))
        if (!(statement->condition = parseExpression(kEndConditionSemicolon)))
            return 0;
    if (!next()) // skip ';'
        return 0;
    if (!isOperator(kOperator_paranthesis_end)) {
        if (!(statement->loop = parseExpression(kEndConditionParanthesis)))
            return 0;
    }
    if (!next()) // skip ')'
        return 0;
    statement->body = parseStatement();
    return statement;
}

astContinueStatement *parser::parseContinueStatement() {
    astContinueStatement *statement = GC_NEW(astStatement) astContinueStatement();
    if (!next()) // skip 'continue'
        return 0;
    return statement;
}

astBreakStatement *parser::parseBreakStatement() {
    astBreakStatement *statement = GC_NEW(astStatement) astBreakStatement();
    if (!next())
        return 0; // skip 'break'
    return statement;
}

astDiscardStatement *parser::parseDiscardStatement() {
    astDiscardStatement *statement = GC_NEW(astStatement) astDiscardStatement();
    if (!next()) // skip 'discard'
        return 0;
    return statement;
}

astReturnStatement *parser::parseReturnStatement() {
    astReturnStatement *statement = GC_NEW(astStatement) astReturnStatement();
    if (!next()) // skip 'return'
        return 0;
    if (!isType(kType_semicolon)) {
        if (!(statement->expression = parseExpression(kEndConditionSemicolon)))
            return 0;
    }
    return statement;
}

astDoStatement *parser::parseDoStatement() {
    astDoStatement *statement = GC_NEW(astStatement) astDoStatement();
    if (!next()) // skip 'do'
        return 0;
    if (!(statement->body = parseStatement()))
        return 0;
    if (!next())
        return 0;
    if (!isKeyword(kKeyword_while)) {
        fatal("expected `while' after `do'");
        return 0;
    }
    if (!next()) // skip 'while'
        return 0;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `while'");
        return 0;
    }
    if (!next()) // skip '('
        return 0;
    if (!(statement->condition = parseExpression(kEndConditionParanthesis)))
        return 0;
    if (!next())
        return 0;
    return statement;
}

astWhileStatement *parser::parseWhileStatement() {
    astWhileStatement *statement = GC_NEW(astStatement) astWhileStatement();
    if (!next()) // skip 'while'
        return 0;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `while'");
        return 0;
    }
    if (!next()) // skip '('
        return 0;
    if (!(statement->condition = parseDeclarationOrExpressionStatement(kEndConditionParanthesis)))
        return 0;
    if (!next())
        return 0;
    if (!(statement->body = parseStatement()))
        return 0;
    return statement;
}

astDeclarationStatement *parser::parseDeclarationStatement(endCondition condition) {
    m_lexer.backup();

    bool isConst = false;
    if (isKeyword(kKeyword_const)) {
        isConst = true;
        if (!next()) // skip 'const'
            return 0;
    }

    astType *type = 0;
    if (isBuiltin()) {
        type = parseBuiltin();
    } else if (isType(kType_identifier)) {
        type = findType(m_token.m_identifier);
    }

    if (!type) {
        m_lexer.restore();
        return 0;
    }

    if (!next())
        return 0;

    astDeclarationStatement *statement = GC_NEW(astStatement) astDeclarationStatement();
    for (;;) {
        size_t paranthesisCount = 0;
        while (isOperator(kOperator_paranthesis_begin)) {
            paranthesisCount++;
            if (!next()) // skip ','
                return 0;
        }
        if (!isType(kType_identifier)) {
            m_lexer.restore();
            return 0;
        }

        std::string name = m_token.m_identifier;
        if (!next()) // skip identifier
            return 0;

        for (size_t i = 0; i < paranthesisCount; i++) {
            if (!isOperator(kOperator_paranthesis_end)) {
                m_lexer.restore();
                return 0;
            }
            if (!next())
                return 0;
        }

        if (statement->variables.empty() && !isOperator(kOperator_assign)
            && !isOperator(kOperator_comma) && !isEndCondition(condition))
        {
            m_lexer.restore();
            return 0;
        }

        astExpression *initialValue = 0;
        if (isOperator(kOperator_assign)) {
            if (!next()) // skip '='
                return 0;
            if (!(initialValue = parseExpression(kEndConditionComma | condition)))
                return 0;
        }

        astFunctionVariable *variable = GC_NEW(astVariable) astFunctionVariable();
        variable->isConst = isConst;
        variable->baseType = type;
        variable->name = name;
        variable->initialValue = initialValue;
        statement->variables.push_back(variable);
        m_scopes.back().push_back(variable);

        if (isEndCondition(condition)) {
            break;
        } else if (isOperator(kOperator_comma)) {
            if (!next()) // skip ','
                return 0;
        } else if (isOperator(kOperator_bracket_begin)) {
            while (isOperator(kOperator_bracket_begin)) {
                variable->isArray = true;
                astConstantExpression *arraySize = parseArraySize();
                if (!arraySize)
                    return 0;
                variable->arraySizes.push_back(arraySize);
                if (!next()) // skip ']'
                    return 0;
            }
        } else {
            fatal("syntax error");
            return 0;
        }
    }

    return statement;
}

astSimpleStatement *parser::parseDeclarationOrExpressionStatement(endCondition condition) {
    astSimpleStatement *declaration = parseDeclarationStatement(condition);
    if (declaration) {
        return declaration;
    } else {
        return parseExpressionStatement(condition);
    }
}

astStatement *parser::parseStatement() {
    if (isType(kType_scope_begin)) {
        return parseCompoundStatement();
    } else if (isKeyword(kKeyword_if)) {
        return parseIfStatement();
    } else if (isKeyword(kKeyword_switch)) {
        return parseSwitchStatement();
    } else if (isKeyword(kKeyword_case) || isKeyword(kKeyword_default)) {
        return parseCaseLabelStatement();
    } else if (isKeyword(kKeyword_for)) {
        return parseForStatement();
    } else if (isKeyword(kKeyword_do)) {
        return parseDoStatement();
    } else if (isKeyword(kKeyword_while)) {
        return parseWhileStatement();
    } else if (isKeyword(kKeyword_continue)) {
        return parseContinueStatement();
    } else if (isKeyword(kKeyword_break)) {
        return parseBreakStatement();
    } else if (isKeyword(kKeyword_discard)) {
        return parseDiscardStatement();
    } else if (isKeyword(kKeyword_return)) {
        return parseReturnStatement();
    } else if (isType(kType_semicolon)) {
        return GC_NEW(astStatement) astEmptyStatement();
    } else {
        return parseDeclarationOrExpressionStatement(kEndConditionSemicolon);
    }
}

astFunction *parser::parseFunction(const topLevel &parse) {
    astFunction *function = GC_NEW(astFunction) astFunction();
    function->returnType = parse.type;
    function->name = parse.name;

    if (!next()) // skip '('
        return 0;
    while (!isOperator(kOperator_paranthesis_end)) {
        astFunctionParameter *parameter = GC_NEW(astVariable) astFunctionParameter();
        while (!isOperator(kOperator_comma) && !isOperator(kOperator_paranthesis_end)) {
            if (isKeyword(kKeyword_in)) {
                parameter->storage = kIn;
            } else if (isKeyword(kKeyword_out)) {
                parameter->storage = kOut;
            } else if (isKeyword(kKeyword_inout)) {
                parameter->storage = kInOut;
            } else if (isKeyword(kKeyword_highp)) {
                parameter->precision = kHighp;
            } else if (isKeyword(kKeyword_mediump)) {
                parameter->precision = kMediump;
            } else if (isKeyword(kKeyword_lowp)) {
                parameter->precision = kLowp;
            } else if (isKeyword(kKeyword_coherent)) {
                parameter->memory = kCoherent;
            } else if (isKeyword(kKeyword_volatile)) {
                parameter->memory = kVolatile;
            } else if (isKeyword(kKeyword_restrict)) {
                parameter->memory = kRestrict;
            } else if (isKeyword(kKeyword_readonly)) {
                parameter->memory = kReadOnly;
            } else if (isKeyword(kKeyword_writeonly)) {
                parameter->memory = kWriteOnly;
            } else if (isType(kType_identifier)) {
                // TODO: user defined types
                parameter->name = m_token.m_identifier;
            } else if (isOperator(kOperator_bracket_begin)) {
                while (isOperator(kOperator_bracket_begin)) {
                    parameter->isArray = true;
                    astConstantExpression *arraySize = parseArraySize();
                    if (!arraySize)
                        return 0;
                    parameter->arraySizes.push_back(arraySize);
                }
            } else {
                parameter->baseType = parseBuiltin();
                if (parameter->baseType && parameter->baseType->builtin) {
                    astBuiltin *builtin = (astBuiltin*)parameter->baseType;
                    if (builtin->type == kKeyword_void && parameter->name.size()) {
                        fatal("`void' parameter cannot be named");
                        return 0;
                    }
                }
            }
            if (!next())
                return 0;
        }

        if (!parameter->baseType) {
            fatal("expected type");
            return 0;
        }
        function->parameters.push_back(parameter);
        if (isOperator(kOperator_comma)) {
            if (!next())// skip ','
                return 0;
        }
    }
    if (!next()) // skip ')'
        return 0;

    // If there is just one 'void' than silently drop it
    if (function->parameters.size() == 1) {
        if (function->parameters[0]->baseType->builtin) {
            astBuiltin *builtin = (astBuiltin*)function->parameters[0]->baseType;
            if (builtin->type == kKeyword_void)
                function->parameters.pop_back();
        }
    }

    // "It is a compile-time or link-time error to declare or define a function main with any other parameters or
    //  return type."
    if (function->name == "main") {
        if (!function->parameters.empty()) {
            fatal("`main' cannot have parameters");
            return 0;
        }
        if (!function->returnType->builtin || ((astBuiltin*)function->returnType)->type != kKeyword_void) {
            fatal("`main' must be declared to return void");
            return 0;
        }
    }

    if (isType(kType_scope_begin)) {
        function->isPrototype = false;
        if (!next()) // skip '{'
            return 0;

        m_scopes.push_back(scope());
        for (size_t i = 0; i < function->parameters.size(); i++)
            m_scopes.back().push_back(function->parameters[i]);
        while (!isType(kType_scope_end)) {
            astStatement *statement = parseStatement();
            if (!statement)
                return 0;
            function->statements.push_back(statement);
            if (!next())// skip ';'
                return 0;
        }

        m_scopes.pop_back();
    } else if (isType(kType_semicolon)) {
        function->isPrototype = true;
    } else {
        fatal("expected `{' or `;'");
        return 0;
    }
    return function;
}

// TODO: cleanup
#undef TYPENAME
#define TYPENAME(X) case kKeyword_##X:
astBuiltin *parser::parseBuiltin() {
    if (!isType(kType_keyword)) {
        fatal("expected keyword");
        return 0;
    }

    switch (m_token.m_keyword) {
        #include "lexemes.h"
            for (size_t i = 0; i < m_builtins.size(); i++) {
                if (m_builtins[i]->type == m_token.m_keyword) {
                    return m_builtins[i];
                }
            }
            m_builtins.push_back(GC_NEW(astType) astBuiltin(m_token.m_keyword));
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
    astConstructorCall *expression = GC_NEW(astExpression) astConstructorCall();
    if (!(expression->type = parseBuiltin()))
        return 0;
    if (!next())
        return 0;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' for constructor call");
        return 0;
    }
    if (!next()) // skip '('
        return 0;
    while (!isOperator(kOperator_paranthesis_end)) {
        astExpression *parameter = parseExpression(kEndConditionComma | kEndConditionParanthesis);
        if (!parameter)
            return 0;
        expression->parameters.push_back(parameter);
        if (isOperator(kOperator_comma)) {
            if (!next()) // skip ','
                return 0;
        }
    }
    return expression;
}

astFunctionCall *parser::parseFunctionCall() {
    astFunctionCall *expression = GC_NEW(astExpression) astFunctionCall();
    expression->name = m_token.m_identifier;
    if (!next()) // skip identifier
        return 0;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' for function call");
        return 0;
    }
    if (!next()) return 0; // skip '('
    while (!isOperator(kOperator_paranthesis_end)) {
        astExpression *parameter = parseExpression(kEndConditionComma | kEndConditionParanthesis);
        if (!parameter)
            return 0;
        expression->parameters.push_back(parameter);
        if (isOperator(kOperator_comma)) {
            if (!next()) // skip ','
                return 0;
        }
    }
    return expression;
}

bool parser::next() {
    m_lexer.read(m_token, true);
    if (isType(kType_eof)) {
        fatal("premature end of file");
        return false;
    }
    return m_lexer.error() ? false : true;
}

astBinaryExpression *parser::createExpression() {
    if (!isType(kType_operator)) {
        fatal("internal compiler error: attempted to create binary expression in wrong context");
        return 0;
    }

    switch (m_token.m_operator) {
    case kOperator_multiply:
    case kOperator_divide:
    case kOperator_modulus:
    case kOperator_plus:
    case kOperator_minus:
    case kOperator_shift_left:
    case kOperator_shift_right:
    case kOperator_less:
    case kOperator_greater:
    case kOperator_less_equal:
    case kOperator_greater_equal:
    case kOperator_equal:
    case kOperator_not_equal:
    case kOperator_bit_and:
    case kOperator_bit_xor:
    case kOperator_logical_and:
    case kOperator_logical_xor:
    case kOperator_logical_or:
        return GC_NEW(astExpression) astOperationExpression(m_token.m_operator);
    case kOperator_assign:
    case kOperator_add_assign:
    case kOperator_sub_assign:
    case kOperator_multiply_assign:
    case kOperator_divide_assign:
    case kOperator_modulus_assign:
    case kOperator_shift_left_assign:
    case kOperator_shift_right_assign:
    case kOperator_bit_and_assign:
    case kOperator_bit_xor_assign:
    case kOperator_bit_or_assign:
        return GC_NEW(astExpression) astAssignmentExpression(m_token.m_operator);
    case kOperator_comma:
        return GC_NEW(astExpression) astSequenceExpression();
    default:
        return 0;
    }
}

astType *parser::findType(const std::string &) {
    return 0;
}

astVariable *parser::findVariable(const std::string &identifier) {
    for (size_t scopeIndex = m_scopes.size(); scopeIndex > 0; scopeIndex--) {
        scope &s = m_scopes[scopeIndex - 1];
        for (size_t variableIndex = 0; variableIndex < s.size(); variableIndex++) {
            if (s[variableIndex]->name == identifier)
                return s[variableIndex];
        }
    }
    return 0;
}

const char *parser::error() const {
    return m_error.c_str();
}

}
