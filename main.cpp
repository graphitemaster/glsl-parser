#include <stdio.h>
#include "lexer.h"

using namespace glsl;

void print(const token &t) {
    switch (t.type()) {
        case kType_keyword:
            printf("%s", t.keyword()->name);
            break;
        case kType_identifier:
            printf("%s", t.identifier());
            break;
        case kType_constant_int:
        case kType_constant_uint:
        case kType_constant_float:
        case kType_constant_double:
            break;
        case kType_operator:
            printf("%s `%s'", t.oper()->name, t.oper()->string);
            break;
        case kType_semicolon:
            printf(";");
            break;
        case kType_scope_begin:
            printf("{");
            break;
        case kType_scope_end:
            printf("}");
            break;
    }
}

#define STRINGIZE(X) #X
int main() {
    const char *source = STRINGIZE(
        uniform Transformation {
            mat4 projection_matrix;
            mat4 modelview_matrix;
        };

        in vec3 vertex;

        void main(void) {
            gl_Position = projection_matrix * modelview_matrix * vec4(vertex, 1.0);
        }
    );

    lexer l(source);
    token t;
    for (;;) {
        t = l.read();
        if (l.error()) {
            printf("%s\n", l.error());
            return 1;
        }
        if (t.type() == kType_eof)
            break;
        if (t.type() != kType_whitespace) {
            printf("%-20s", t.name());
            print(t);
        }
        printf("\n");
    }
    return 0;
}
