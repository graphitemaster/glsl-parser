#ifndef LEXER_HDR
#define LEXER_HDR
#include <stddef.h> // size_t

#include <vector>

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
    token();
    friend struct lexer;
    friend struct parser;
    int m_type;
    union {
        char *asIdentifier;
        int asInt;
        int asKeyword;
        int asOperator;
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
    lexer(const char *data);

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

    int at(int offset = 0) const;

    void read(token &out);
    void read(token &out, bool);

    std::vector<char> readNumeric(bool isOctal, bool isHex);

private:
    const char *m_data;
    size_t m_length;
    const char *m_error;
    location m_location;
    location m_backup;
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
