@trusted pure:

unittest
{
    int x;
    x = x;                      // diagnose

    int y;
    y = x;                      // no diagnose

    y = 32;                     // no diagnose

    int* xp;
    xp = xp;                    // diagnose

    *(&x) = *(&x);              // should diagnose
}

int x;
void test()
{
    int x = x;                  // shouldn't this give a shadowing warning?
}
