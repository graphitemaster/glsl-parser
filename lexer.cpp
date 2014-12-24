#include <string.h>
#include "lexer.h"

namespace glsl {

// Lookup table of keywords
#define TYPE(...)
#define OPERATOR(...)
#define KEYWORD(X) { #X, kKeyword_##X },
static const keywordInfo kKeywords[] = {
    #include "lexemes.h"
};
#undef TYPE
#undef OPERATOR
#undef KEYWORD

// Lookup table of operators
#define TYPE(...)
#define KEYWORD(...)
#define OPERATOR(X, S) { #X, S },
static const operatorInfo kOperators[] = {
    #include "lexemes.h"
};
#undef OPERATOR
#undef TYPE
#undef KEYWORD

// Lookup table of token names
#define TYPE(X) #X,
#define KEYWORD(...)
#define OPERATOR(...)
static const char *kTokenNames[] = {
    #include "lexemes.h"
};
#undef TYPE
#undef KEYWORD
#undef OPERATOR

// Utilities
#define is_digit(X) (((unsigned)(X)-'0') < 10)
#define is_char(X) ((((unsigned)(X)|32)-'a') < 26)
#define is_octal(X) ((X) >= '0' && (X) <= '7')
#define is_hex(X) (((X) >= 'a' && (X) <= 'f') && ((X) >= 'A' && (X) <= 'F') && is_digit(X))
#define is_space(X) (((X) >= '\t' && (X) <= '\r') || (X) == ' ')

lexer::lexer(const std::string &string)
    : m_position(0)
    , m_data(string)
{
}

void lexer::read(token &out) {
    // TODO: Line continuation (backslash `\'.)
    if (m_position == m_data.length()) {
        out.m_type = kType_eof;
        return;
    }

    int ch1 = (m_position + 1 < m_data.length()) ? m_data[m_position + 1] : 0;
    int ch2 = (m_position + 2 < m_data.length()) ? m_data[m_position + 2] : 0;

    // Lex numerics
    if (is_digit(m_data[m_position]) || (m_data[m_position] == '.' && is_digit(ch1)))
    {
        bool isFloat = false;
        bool isDouble = false;
        bool isUnsigned = false;
        bool isOctal = false;
        bool isHex = false;

        if (m_data[m_position] == '0') {
            if (ch1 && (ch1 == 'x' || ch1 == 'X')) {
                isHex = true;
                m_position += 2;
            }
            // TODO: Octal
        }

        std::string numeric = readNumeric(isOctal, isHex);
        if (m_position != m_data.length() && m_data[m_position] == '.') {
            isFloat = true;
            numeric.push_back('.');
            m_position++;
            numeric += readNumeric(isOctal, isHex);
        }

        if (m_position != m_data.length() && (m_data[m_position] == 'e' || m_data[m_position] == 'E')) {
            ch1 = (m_position + 1 < m_data.length()) ? m_data[m_position + 1] : 0;
            ch2 = (m_position + 2 < m_data.length()) ? m_data[m_position + 2] : 0;
            if ((ch1 == '+' || ch1 == '-') && (ch2 >= '0' && ch2 <= '9')) {
                numeric.push_back(ch1);
                numeric.push_back(ch2);
                m_position += 2;
                numeric += readNumeric(isOctal, isHex);
                isFloat = true;
            } else {
                m_error = "invalid numeric literal";
                return;
            }
        }

        if (m_position != m_data.length() && is_char(m_data[m_position])) {
            ch1 = (m_position + 1 < m_data.length()) ? m_data[m_position + 1] : 0;
            if (m_data[m_position] == 'f' || m_data[m_position] == 'F') {
                isFloat = true;
            } else if ((m_data[m_position] == 'l' && ch1 == 'f') || (m_data[m_position] == 'L' && ch1 == 'F')) {
                isFloat = false;
                isDouble = true;
            } else if (m_data[m_position] == 'u' | m_data[m_position] == 'U') {
                if (isFloat) {
                    m_error = "invalid use of suffix on literal";
                    return;
                }
                isUnsigned = true;
            } else {
                m_error = "invalid numeric literal";
                return;
            }
            m_position++;
        }

        if (isHex && (isFloat || isDouble)) {
            m_error = "invalid numeric literal";
            return;
        }

        int base = isHex ? 16 : (isOctal ? 8 : 10);
        if (isFloat) {
            out.m_type = kType_constant_float;
            out.asFloat = 0.0f; // TODO:
        } else if (isDouble) {
            out.m_type = kType_constant_double;
            out.asDouble = 0.0; // TODO:
        } else if (isUnsigned) {
            out.m_type = kType_constant_uint;
            out.asUnsigned = 0; // TODO:
        } else {
            out.m_type = kType_constant_int;
            out.asInt = 0; // TODO:
        }
    } else if (is_char(m_data[m_position]) || m_data[m_position] == '_') {
        // Identifiers
        out.m_type = kType_identifier;
        while (m_position != m_data.length() &&
            (is_char(m_data[m_position]) || is_digit(m_data[m_position]) || m_data[m_position] == '_'))
        {
            out.m_identifier.push_back(m_data[m_position]);
            m_position++;
        }

        for (int i = 0; i < sizeof(kKeywords)/sizeof(kKeywords[0]); i++) {
            if (strcmp(kKeywords[i].name, out.m_identifier.c_str()))
                continue;
            out.m_type = kType_keyword;
            out.m_keyword = i;
            out.m_identifier.clear();
        }
    } else {
        switch (m_data[m_position]) {
        // Non operators
        case ' ':
        case '\t':
        case '\r':
        case '\n':
        case '\f':
        case '\v':
            while (m_position < m_data.length() && is_space(m_data[m_position]))
                m_position++;
            out.m_type = kType_whitespace; // Whitespace already skippped
            break;
        case ';':
            out.m_type = kType_semicolon;
            m_position++;
            break;
        case '{':
            out.m_type = kType_scope_begin;
            m_position++;
            break;
        case '}':
            out.m_type = kType_scope_end;
            m_position++;
            break;

        // Operators
        case '.':
            out.m_type = kType_operator;
            out.m_op = kOperator_dot;
            break;
        case '+':
            out.m_type = kType_operator;
            if (ch1 == '+')
                out.m_op = kOperator_increment;
            else if (ch1 == '=')
                out.m_op = kOperator_add_assign;
            else
                out.m_op = kOperator_plus;
            break;
        case '-':
            out.m_type = kType_operator;
            if (ch1 == '-')
                out.m_op = kOperator_decrement;
            else if (ch1 == '=')
                out.m_op = kOperator_sub_assign;
            else
                out.m_op = kOperator_minus;
            break;
        case '/':
            if (ch1 == '/') {
                // Skip line comments
                while (m_position != m_data.length()) {
                    if (m_data[m_position] == '\n') {
                        m_position++;
                        break;
                    }
                    m_position++;
                }
                out.m_type = kType_comment;
            } else if (ch1 == '*') {
                // Skip block comments
                while (m_position != m_data.length()) {
                    if (m_data[m_position] == '*' && m_position + 1 < m_data.length() && m_data[m_position + 1] == '/') {
                        m_position += 2;
                        break;
                    }
                    m_position++;
                }
                out.m_type = kType_comment;
            } else if (ch1 == '=') {
                out.m_type = kType_operator;
                out.m_op = kOperator_divide_assign;
            } else {
                out.m_type = kType_operator;
                out.m_op = kOperator_divide;
            }
            break;
        case '*':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_op = kOperator_multiply_assign;
            else
                out.m_op = kOperator_multiply;
            break;
        case '%':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_op = kOperator_modulus_assign;
            else
                out.m_op = kOperator_modulus;
            break;
        case '<':
            out.m_type = kType_operator;
            if (ch1 == '<' && ch2 == '=')
                out.m_op = kOperator_shift_left_assign;
            else if (ch1 == '<')
                out.m_op = kOperator_shift_left;
            else if (ch1 == '=')
                out.m_op = kOperator_less_equal;
            else
                out.m_op = kOperator_less;
            break;
        case '>':
            out.m_type = kType_operator;
            if (ch1 == '>' && ch2 == '=')
                out.m_op = kOperator_shift_right_assign;
            else if (ch1 == '>')
                out.m_op == kOperator_shift_right;
            else if (ch1 == '=')
                out.m_op = kOperator_greater_equal;
            else
                out.m_op = kOperator_greater;
            break;
        case '[':
            out.m_type = kType_operator;
            out.m_op = kOperator_bracket_begin;
            break;
        case ']':
            out.m_type = kType_operator;
            out.m_op = kOperator_bracket_end;
            break;
        case '(':
            out.m_type = kType_operator;
            out.m_op = kOperator_paranthesis_begin;
            break;
        case ')':
            out.m_type = kType_operator;
            out.m_op = kOperator_paranthesis_end;
            break;
        case '^':
            out.m_type = kType_operator;
            if (ch1 == '^')
                out.m_op = kOperator_logical_xor;
            else if (ch1 == '=')
                out.m_op = kOperator_bit_xor_assign;
            else
                out.m_op = kOperator_bit_xor;
            break;
        case '|':
            out.m_type = kType_operator;
            if (ch1 == '|')
                out.m_op = kOperator_logical_or;
            else if (ch1 == '=')
                out.m_op = kOperator_bit_or_assign;
            else
                out.m_op = kOperator_bit_or;
            break;
        case '&':
            out.m_type = kType_operator;
            if (ch1 == '&')
                out.m_op = kOperator_logical_and;
            else if (ch1 == '=')
                out.m_op = kOperator_bit_and_assign;
            else
                out.m_op = kOperator_bit_and;
            break;
        case '~':
            out.m_type = kType_operator;
            out.m_op = kOperator_bit_not;
            break;
        case '=':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_op = kOperator_equal;
            else
                out.m_op = kOperator_assign;
            break;
        case '!':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_op = kOperator_not_equal;
            else
                out.m_op = kOperator_logical_not;
            break;
        case ':':
            out.m_type = kType_operator;
            out.m_op = kOperator_colon;
            break;
        case ',':
            out.m_type = kType_operator;
            out.m_op = kOperator_comma;
            break;
        case '?':
            out.m_type = kType_operator;
            out.m_op = kOperator_questionmark;
            break;
        default:
            m_error = "invalid character encountered";
            return;
        }
        // Skip whitespace for operator
        if (out.m_type == kType_operator)
            m_position += strlen(kOperators[out.m_op].string);
    }
}

std::string lexer::readNumeric(bool isOctal, bool isHex) {
    std::string digits;
    if (isOctal) {
        while (m_position < m_data.length() && is_octal(m_data[m_position]))
            digits.push_back(m_data[m_position++]);
    } else if (isHex) {
        while (m_position < m_data.length() && is_hex(m_data[m_position]))
            digits.push_back(m_data[m_position++]);
    } else {
        while (m_position < m_data.length() && is_digit(m_data[m_position]))
            digits.push_back(m_data[m_position++]);
    }
    return digits;
}

token lexer::peek() {
    token out;
    // Record the position to restore it later
    const size_t record = m_position;
    // Ignore whitespace and comment tokens when peeking
    do {
        read(out);
    } while (out.m_type == kType_whitespace || out.m_type == kType_comment);
    m_position = record;
    return out;
}

token lexer::read() {
    token out;
    read(out);
    return out;
}

const char *lexer::error() const {
    return m_error.size() ? m_error.c_str() : NULL;
}

const char *token::name() const {
    return kTokenNames[m_type];
}

const keywordInfo *token::keyword() const {
    return &kKeywords[m_keyword];
}

const operatorInfo *token::oper() const {
    return &kOperators[m_op];
}

const char *token::identifier() const {
    return m_identifier.c_str();
}

}
