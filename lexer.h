#ifndef LEXER_HDR
#define LEXER_HDR
#include <string>

namespace glsl {

#define KEYWORD(...)
#define OPERATOR(...)
#define TYPENAME(...)

// Types
#define TYPE(X) kType_##X,
enum {
    #include "lexemes.h"
};
#undef TYPE
#define TYPE(...)

// Keywords
#undef KEYWORD
#define KEYWORD(X) kKeyword_##X,
enum {
    #include "lexemes.h"
};
#undef KEYWORD
#define KEYWORD(...)

// Operators
#undef OPERATOR
#define OPERATOR(X, ...) kOperator_##X,
enum {
    #include "lexemes.h"
};
#undef OPERATOR
#define OPERATOR(...)

struct keywordInfo {
    const char *name;
    int type;
};

struct operatorInfo {
    const char *name;
    const char *string;
    int precedence;
};

struct token {
    const char *name() const;
    const keywordInfo *keyword() const;
    const operatorInfo *oper() const;
    const char *identifier() const;

    int type() const;
    int precedence() const;

private:
    friend struct lexer;
    friend struct parser;
    int m_type;
    int m_keyword;
    int m_operator;
    std::string m_identifier;
    union {
        int asInt;
        unsigned asUnsigned;
        float asFloat;
        double asDouble;
    };
};

inline int token::type() const {
    return m_type;
}

struct lexer {
    lexer(const std::string &data);

    token read();
    token peek();

    const char *error() const;

    void backup();
    void restore();

protected:
    friend struct parser;
    void read(token &out);
    void read(token &out, bool);

    std::string readNumeric(bool isOctal, bool isHex);

private:
    size_t m_position;
    size_t m_backup;
    std::string m_data;
    std::string m_error;
};

}

#endif
