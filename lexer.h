#ifndef LEXER_HDR
#define LEXER_HDR
#include <string>

namespace glsl {

// Types
#define TYPE(X) kType_##X,
#define KEYWORD(...)
#define OPERATOR(...)
enum {
    #include "lexemes.h"
};
#undef TYPE
#undef KEYWORD
#undef OPERATOR

// Keywords
#define KEYWORD(X) kKeyword_##X,
#define TYPE(...)
#define OPERATOR(...)
enum {
    #include "lexemes.h"
};
#undef KEYWORD
#undef TYPE
#undef OPERATOR

// Operators
#define OPERATOR(X, ...) kOperator_##X,
#define TYPE(...)
#define KEYWORD(...)
enum {
    #include "lexemes.h"
};
#undef OPERATOR
#undef TYPE
#undef KEYWORD

struct keywordInfo {
    const char *name;
    int type;
};

struct operatorInfo {
    const char *name;
    const char *string;
};

struct token {
    const char *name() const;
    const keywordInfo *keyword() const;
    const operatorInfo *oper() const;
    const char *identifier() const;

    int type() const;

private:
    friend struct lexer;
    friend struct parser;
    int m_type;
    int m_keyword;
    int m_op;
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

protected:
    void read(token &out);
    std::string readNumeric(bool isOctal, bool isHex);

private:
    size_t m_position;
    std::string m_data;
    std::string m_error;
};

}

#endif
