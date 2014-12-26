#include <string.h>
#include <stdlib.h>

#include "lexer.h"

namespace glsl {

// Lookup table of keywords
#undef KEYWORD
#define KEYWORD(X) { #X, kKeyword_##X },
static const keywordInfo kKeywords[] = {
    #include "lexemes.h"
};
#undef KEYWORD
#define KEYWORD(...)

// Lookup table of operators
#undef OPERATOR
#define OPERATOR(X, S, PREC) { #X, S, PREC },
static const operatorInfo kOperators[] = {
    #include "lexemes.h"
};
#undef OPERATOR
#define OPERATOR(...)

int token::precedence() const {
    return kOperators[m_operator].precedence;
}

/// location
location::location()
    : column(1)
    , line(1)
    , position(0)
{
}

void location::advanceColumn(size_t count) {
    column += count;
    position += count;
}

void location::advanceLine() {
    line++;
    position++;
    column = 1;
}

static inline bool isDigit(int ch) {
    return unsigned(ch) - '0' < 10;
}

static inline bool isChar(int ch) {
    return (unsigned(ch) | 32) - 'a' < 26;
}

static inline bool isOctal(int ch) {
    return unsigned(ch) - '0' < 8;
}

static inline bool isHex(int ch) {
    return (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F') || isDigit(ch);
}

static inline bool isSpace(int ch) {
    return (ch >= '\t' && ch <= '\r') || ch == ' ';
}

lexer::lexer(const std::string &string)
    : m_data(string)
{
}

int lexer::at(int offset) const {
    if (position() + offset < m_data.length())
        return m_data[position() + offset];
    return 0;
}

void lexer::read(token &out) {
    // TODO: Line continuation (backslash `\'.)
    if (position() == m_data.length()) {
        out.m_type = kType_eof;
        return;
    }

    int ch1 = at(1);
    int ch2 = at(2);

    // Lex numerics
    if (isDigit(at()) || (at() == '.' && isDigit(ch1)))
    {
        bool isFloat = false;
        bool isDouble = false;
        bool isUnsigned = false;
        bool isOctalish = false;
        bool isHexish = false;

        if (at() == '0') {
            if (ch1 && (ch1 == 'x' || ch1 == 'X')) {
                isHexish = true;
                m_location.advanceColumn(2);
            }
            // TODO: Octal
        }

        std::string numeric = readNumeric(isOctalish, isHexish);
        if (position() != m_data.length() && at() == '.') {
            isFloat = true;
            numeric.push_back('.');
            m_location.advanceColumn();
            numeric += readNumeric(isOctalish, isHexish);
        }

        if (position() != m_data.length() && (at() == 'e' || at() == 'E')) {
            ch1 = at(1);
            ch2 = at(2);
            if ((ch1 == '+' || ch1 == '-') && (ch2 >= '0' && ch2 <= '9')) {
                numeric.push_back(ch1);
                numeric.push_back(ch2);
                m_location.advanceColumn(2);
                numeric += readNumeric(isOctalish, isHexish);
                isFloat = true;
            } else {
                m_error = "invalid numeric literal";
                return;
            }
        }

        if (position() != m_data.length() && isChar(at())) {
            ch1 = at(1);
            if (at() == 'f' || at() == 'F') {
                isFloat = true;
            } else if ((at() == 'l' && ch1 == 'f') || (at() == 'L' && ch1 == 'F')) {
                isFloat = false;
                isDouble = true;
            } else if (at() == 'u' || at() == 'U') {
                if (isFloat) {
                    m_error = "invalid use of suffix on literal";
                    return;
                }
                isUnsigned = true;
            } else {
                m_error = "invalid numeric literal";
                return;
            }
            m_location.advanceColumn();
        }

        if (isHexish && (isFloat || isDouble)) {
            m_error = "invalid numeric literal";
            return;
        }

        int base = isHexish ? 16 : (isOctalish ? 8 : 10);
        if (isFloat) {
            out.m_type = kType_constant_float;
            out.asFloat = strtof(numeric.c_str(), 0);
        } else if (isDouble) {
            out.m_type = kType_constant_double;
            out.asDouble = strtof(numeric.c_str(), 0);
        } else if (isUnsigned) {
            out.m_type = kType_constant_uint;
            out.asUnsigned = strtoul(numeric.c_str(), 0, base);
        } else {
            out.m_type = kType_constant_int;
            out.asInt = strtol(numeric.c_str(), 0, base);
        }
    } else if (isChar(at()) || at() == '_') {
        // Identifiers
        out.m_type = kType_identifier;
        out.m_identifier.clear();
        while (position() != m_data.length() && (isChar(at()) || isDigit(at()) || at() == '_')) {
            out.m_identifier.push_back(at());
            m_location.advanceColumn();
        }

        for (size_t i = 0; i < sizeof(kKeywords)/sizeof(kKeywords[0]); i++) {
            if (strcmp(kKeywords[i].name, out.m_identifier.c_str()))
                continue;
            out.m_type = kType_keyword;
            out.m_keyword = int(i);
        }
    } else {
        switch (at()) {
        // Non operators
        case '\n':
        case '\t':
        case '\f':
        case '\v':
        case '\r':
        case ' ':
            while (position() < m_data.length() && isSpace(at())) {
                if (at() == '\n')
                    m_location.advanceLine();
                else
                    m_location.advanceColumn();
            }
            out.m_type = kType_whitespace; // Whitespace already skippped
            break;
        case ';':
            out.m_type = kType_semicolon;
            m_location.advanceColumn();
            break;
        case '{':
            out.m_type = kType_scope_begin;
            m_location.advanceColumn();
            break;
        case '}':
            out.m_type = kType_scope_end;
            m_location.advanceColumn();
            break;

        // Operators
        case '.':
            out.m_type = kType_operator;
            out.m_operator = kOperator_dot;
            break;
        case '+':
            out.m_type = kType_operator;
            if (ch1 == '+')
                out.m_operator = kOperator_increment;
            else if (ch1 == '=')
                out.m_operator = kOperator_add_assign;
            else
                out.m_operator = kOperator_plus;
            break;
        case '-':
            out.m_type = kType_operator;
            if (ch1 == '-')
                out.m_operator = kOperator_decrement;
            else if (ch1 == '=')
                out.m_operator = kOperator_sub_assign;
            else
                out.m_operator = kOperator_minus;
            break;
        case '/':
            if (ch1 == '/') {
                // Skip line comments
                while (position() != m_data.length()) {
                    if (at() == '\n') {
                        m_location.advanceLine();
                        break;
                    }
                    m_location.advanceColumn();
                }
                out.m_type = kType_comment;
            } else if (ch1 == '*') {
                // Skip block comments
                while (position() != m_data.length()) {
                    if (at() == '\n') {
                        m_location.advanceLine();
                        continue;
                    }
                    if (at() == '*' && position() + 1 < m_data.length() && m_data[position() + 1] == '/') {
                        m_location.advanceColumn(2);
                        break;
                    }
                    m_location.advanceColumn();
                }
                out.m_type = kType_comment;
            } else if (ch1 == '=') {
                out.m_type = kType_operator;
                out.m_operator = kOperator_divide_assign;
            } else {
                out.m_type = kType_operator;
                out.m_operator = kOperator_divide;
            }
            break;
        case '*':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_operator = kOperator_multiply_assign;
            else
                out.m_operator = kOperator_multiply;
            break;
        case '%':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_operator = kOperator_modulus_assign;
            else
                out.m_operator = kOperator_modulus;
            break;
        case '<':
            out.m_type = kType_operator;
            if (ch1 == '<' && ch2 == '=')
                out.m_operator = kOperator_shift_left_assign;
            else if (ch1 == '<')
                out.m_operator = kOperator_shift_left;
            else if (ch1 == '=')
                out.m_operator = kOperator_less_equal;
            else
                out.m_operator = kOperator_less;
            break;
        case '>':
            out.m_type = kType_operator;
            if (ch1 == '>' && ch2 == '=')
                out.m_operator = kOperator_shift_right_assign;
            else if (ch1 == '>')
                out.m_operator = kOperator_shift_right;
            else if (ch1 == '=')
                out.m_operator = kOperator_greater_equal;
            else
                out.m_operator = kOperator_greater;
            break;
        case '[':
            out.m_type = kType_operator;
            out.m_operator = kOperator_bracket_begin;
            break;
        case ']':
            out.m_type = kType_operator;
            out.m_operator = kOperator_bracket_end;
            break;
        case '(':
            out.m_type = kType_operator;
            out.m_operator = kOperator_paranthesis_begin;
            break;
        case ')':
            out.m_type = kType_operator;
            out.m_operator = kOperator_paranthesis_end;
            break;
        case '^':
            out.m_type = kType_operator;
            if (ch1 == '^')
                out.m_operator = kOperator_logical_xor;
            else if (ch1 == '=')
                out.m_operator = kOperator_bit_xor_assign;
            else
                out.m_operator = kOperator_bit_xor;
            break;
        case '|':
            out.m_type = kType_operator;
            if (ch1 == '|')
                out.m_operator = kOperator_logical_or;
            else if (ch1 == '=')
                out.m_operator = kOperator_bit_or_assign;
            else
                out.m_operator = kOperator_bit_or;
            break;
        case '&':
            out.m_type = kType_operator;
            if (ch1 == '&')
                out.m_operator = kOperator_logical_and;
            else if (ch1 == '=')
                out.m_operator = kOperator_bit_and_assign;
            else
                out.m_operator = kOperator_bit_and;
            break;
        case '~':
            out.m_type = kType_operator;
            out.m_operator = kOperator_bit_not;
            break;
        case '=':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_operator = kOperator_equal;
            else
                out.m_operator = kOperator_assign;
            break;
        case '!':
            out.m_type = kType_operator;
            if (ch1 == '=')
                out.m_operator = kOperator_not_equal;
            else
                out.m_operator = kOperator_logical_not;
            break;
        case ':':
            out.m_type = kType_operator;
            out.m_operator = kOperator_colon;
            break;
        case ',':
            out.m_type = kType_operator;
            out.m_operator = kOperator_comma;
            break;
        case '?':
            out.m_type = kType_operator;
            out.m_operator = kOperator_questionmark;
            break;
        default:
            m_error = "invalid character encountered";
            return;
        }
        // Skip whitespace for operator
        if (out.m_type == kType_operator)
            m_location.advanceColumn(strlen(kOperators[out.m_operator].string));
    }
}

std::string lexer::readNumeric(bool isOctalish, bool isHexish) {
    std::string digits;
    if (isOctalish) {
        while (position() < m_data.length() && isOctal(at())) {
            digits.push_back(at());
            m_location.advanceColumn();
        }
    } else if (isHexish) {
        while (position() < m_data.length() && isHex(at())) {
            digits.push_back(at());
            m_location.advanceColumn();
        }
    } else {
        while (position() < m_data.length() && isDigit(at())) {
            digits.push_back(at());
            m_location.advanceColumn();
        }
    }
    return digits;
}

token lexer::peek() {
    token out;
    backup();
    read(out, true);
    restore();
    return out;
}

void lexer::read(token &out, bool) {
    do {
        read(out);
    } while (out.m_type == kType_whitespace || out.m_type == kType_comment);
}

const char *lexer::error() const {
    return m_error.size() ? m_error.c_str() : NULL;
}

void lexer::backup() {
    m_backup = m_location;
}

void lexer::restore() {
    m_location = m_backup;
}

}
