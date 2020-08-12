@trusted pure unittest
{
    int x;
    x = x;                      // diagnostics

    int y;
    y = x;                      // no diagnostics

    y = 32;                     // no diagnostics

    *(&x) = *(&x);              // should diagnostics

    int* xp;
    xp = xp;                    // diagnostics
}

int x;
void test()
{
    int x = x;
}
