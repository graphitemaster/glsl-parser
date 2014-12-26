// Single line comments

/* Block comments */

float test_default;
in float test_in;
out float test_out;
attribute float test_attribute;
uniform float test_uniform;
varying float test_varying;

float test_default_array[1];
in float test_in_array[2];
out float test_out_array[3];
attribute float test_attribute_array[4];
uniform float test_uniform_array[5];
varying float test_varying_array[6];

in centroid float test_centroid_in;
in sample float test_sample_in;
in patch float test_patch_in;

// in & out can come after the auxiliary qualifier as well
centroid in float test_centroid_in_after;
sample in float test_sample_in_after;
patch in float test_patch_in_after;

out centroid float test_centroid_out;
out sample float test_sample_out;
out patch float test_patch_out;

// in & out can come after the auxiliary qualifier as well
centroid out float test_centroid_out_after;
sample out float test_sample_out_after;
patch out float test_patch_out_after;

smooth float test_smooth;
flat float test_flat;
noperspective float test_noperspective;

void test_function_empty_proto();
void test_function_empty_void_proto(void);
void test_function_pass_proto(float pass);
void test_function_pass_array_proto(float array[10]);
