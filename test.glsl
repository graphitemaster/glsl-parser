// Single line comments

/* Block comments */

/* Spanning multiple lines
 * as well */

in float test_in;
out float test_out;
inout float test_inout;
const float test_const;
float test_global;
centroid float test_centroid;
//float test_array_unsized[]; // Broken
float test_array_sized[10];

void test_prototype_empty();
void test_prototype_void(void);
void test_prototype_named(float something);
void test_prototype_unnamed(float);
//void test_prototype_named_array(float something[10]); // Broken
//void test_prototype_unnamed_array(float[10]); // Broken

void test_function_empty() {
}

void test_function_void(void) {
}

void test_control_flow() {
    if (0) { }
    if (1) { } else { }
    if (1) { } else if (2) { }
    switch (0) {
        case 0:
        case 1:
            break;
        default:
            break;
    }
    for (;;) {
    }
    do { } while (0);
    //while (1.0f) { } // Broken
}


