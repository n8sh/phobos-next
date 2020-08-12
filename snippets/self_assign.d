pure unittest
{
    int x;
    x = x;                      // diagnose

    int y;
    y = x;                      // no diagnose

    int* xp;
    xp = xp;                    // diagnose

    *(&x) = *(&x);              // should diagnose
}

int x;
void test() @safe nothrow @nogc
{
    int x = x;                  // shouldn't this give a shadowing warning?
}
