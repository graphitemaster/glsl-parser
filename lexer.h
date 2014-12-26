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

struct location {
    location();
    size_t column;
    size_t line;
    size_t position;
private:
    friend struct lexer;
    void advanceColumn(size_t count = 1);
    void advanceLine();
};

struct lexer {
    lexer(const std::string &data);

    token read();
    token peek();

    const char *error() const;

    void backup();
    void restore();

    size_t line() const;
    size_t column() const;

protected:
    friend struct parser;

    size_t position() const;

    int current() const;

    void read(token &out);
    void read(token &out, bool);

    std::string readNumeric(bool isOctal, bool isHex);

private:
    location m_location;
    location m_backup;

    std::string m_data;
    std::string m_error;
};

inline size_t lexer::position() const {
    return m_location.position;
}

inline size_t lexer::line() const {
    return m_location.line;
}

inline size_t lexer::column() const {
    return m_location.column;
}

}

#endif
