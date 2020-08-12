pure unittest
{
    int x;
    x = x;                      // diagnose

    int y;
    y = x;

    int* xp;
    xp = xp;                    // diagnose

    *(&x) = *(&x);              // should diagnose

    static assert(__traits(compiles, { int t; t = t; }));
}

int x;
void test() @safe nothrow @nogc
{
    int x = x;                  // shouldn't this give a shadowing warning?
}
