struct foo
{
    vec3 a;
    vec2 b;
    float c[100];
};

struct bar
{
    foo a, b, c;
} a, b, c;

void main( )
{
    bar a, b, c;
}
