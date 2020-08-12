@trusted pure unittest
{
    int x;
    x = x;                      // warn

    int y;
    y = x;                      // no warn

    y = 32;                     // no warn

    *(&x) = *(&x);              // should warn

    int* xp;
    xp = xp;
}

int x;
void test()
{
    int x = x;
}
