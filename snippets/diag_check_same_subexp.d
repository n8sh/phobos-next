/// Neither GCC 10` nor Clang 10 warn here.
void check_equal_lhs_and_rhs(int i)
{
    bool x, y;
    alias xa = x;

    enum { x1 = (0 | 1), x2 }

    if (1 & 2)
        i = 42;

    if (x1 & x1)
        i = 42;

    if (x & x)
        i = 42;

    i = x + x;
    i = x - x;
    i = x * x;

    if (x & xa)
        i = 42;

    if (x & y)
        i = 42;

    if (x | x)
        i = 42;

    if (x & x |
        x & x)
        i = 42;

    if (x && x)
        i = 42;

    if (x || x)
        i = 42;

    if ((x && x) ||
        (x && x))
        i = 42;
}
