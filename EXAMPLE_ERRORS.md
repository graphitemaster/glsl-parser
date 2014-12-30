# Error samples

Here are some example diagnostics to show how superior glsl-parser is compared to
that of driver-specific diagnostics.

    $ echo "layout(error)" | ./glsl-parser -
    <stdin>:1:13: error: unknown layout qualifier `error'

    $ echo "layout(shared = 1)" | ./glsl-parser -
    <stdin>:1:16: error: unexpected layout qualifier value on `shared' layout qualifier

    $ echo "layout(location)" | ./glsl-parser -
    <stdin>:1:17: error: expected layout qualifier value for `location' layout qualifier

    $ echo "patch in vec3 foo;" | ./glsl-parser -
    <stdin>:1:15: error: applying `patch' qualifier to input can only be done in tessellation evaluation shaders

    $ echo "smooth out vec3 foo;" | ./glsl-parser -
    <stdin>:1:16: error: cannot use interpolation qualifier on fragment shader output

    $ echo "const out vec3 foo;" | ./glsl-parser -
    <stdin>:1:15: error: multiple storage qualifiers in declaration

    $ echo "centroid sample vec3 foo;" | ./glsl-parser -
    <stdin>:1:21: error: multiple auxiliary storage qualifiers in declaration

    $ echo "lowp mediump vec3 foo;" | ./glsl-parser -
    <stdin>:1:18: error: multiple precision qualifiers in declaration

    $ echo "const foo;" | ./glsl-parser -
    <stdin>:1:11: error: expected typename

    $ echo "const float foo;" | ./glsl-parser -
    <stdin>:1:17: error: const-qualified variable declared but not initialized

    $ echo "float a; const float b = a;" | ./glsl-parser -
    <stdin>:1:28: error: not a valid constant expression

    $ echo "void a;" | ./glsl-parser -
    <stdin>:1:8: error: `void' cannot be used in declaration

    $ echo "void (void);" | ./glsl-parser -
    <stdin>:1:7: error: expected name for declaration

    $ echo "void foo() { 1 = 1; }" | ./glsl-parser -
    <stdin>:1:20: error: not a valid lvalue

    $ echo "in float a; void foo() { a = 1; }" | ./glsl-parser -
    <stdin>:1:32: error: cannot write to a variable declared as input

    $ echo "const float a = 0; void foo() { a = 1; }" | ./glsl-parser -
    <stdin>:1:39: error: cannot write to a const variable outside of its declaration

    $ echo "void foo() { a = 0; }" | ./glsl-parser -
    <stdin>:1:15: error: `a' was not declared in this scope

    $ echo "vec3 a; void foo() { a.; }" | ./glsl-parser -
    <stdin>:1:25: error: expected field identifier or swizzle after `.'

    $ echo "void foo() { 1[0] = 1; }" | ./glsl-parser -
    <stdin>:1:17: error: cannot be subscripted

    $ echo "float a; void foo() { switch(0) { case a: break; } }" | ./glsl-parser -
    <stdin>:1:42: error: case label is not a valid constant expression

    $ echo "void foo() { switch(0) { case 0: case 0: break; } }" | ./glsl-parser -
    <stdin>:1:41: error: duplicate case label `0'

    $ echo "void foo() { switch(0) { default; } }" | ./glsl-parser -
    <stdin>:1:34: error: expected `:' after `default' in case label

    $ echo "void main(float) { }" | ./glsl-parser -
    <stdin>:1:19: error: `main' cannot have parameters

    $ echo "float main() { }" | ./glsl-parser -
    <stdin>:1:15: error: `main' must be declared to return void

    $ echo "const uint big = 0xFFFFFFFFFU;" | ./glsl-parser -
    <stdin>:1:30: error: literal needs more than 32-bits

    $ echo "const float foo = 0.1u;" | ./glsl-parser -
    <stin>:1:23: error: invalid use of suffix on literal
