void main() {
    float a = 0, b = 0, c = 0, d = 0;
    float w = (a ? a,b : b,c);
    float z = a ? b ? c : d : w;
}
