void test() {
    int test_uninitialized_int;
    int test_initialized_int = 42;
    uint test_uninitialized_uint;
    uint test_initialized_uint_no_suffix = 42;
    uint test_initialized_uint_suffix = 42u;
    uint test_hex_no_suffix_upper = 0xFF;
    uint test_hex_suffix_upper = 0xFFu;
    uint test_hex_no_suffix_lower = 0xff;
    uint test_hex_suffix_lower = 0xffu;
    uint test_hex_no_suffix_mixed = 0xFf;
    uint test_hex_suffix_mixed = 0xFfU;
    int test_negative = -1;
    uint test_octal = 0777;
}
