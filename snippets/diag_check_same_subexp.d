/// Neither GCC 10` nor Clang 10 warn here.
void check_equal_lhs_and_rhs(int i)
{
    bool x, y;
    alias xa = x;

    enum { a = 0, b = 1 }

    if (a & a)                  // TODO: no warn for enumerators
        i = 42;

    if (b & b)                  // TODO: no warn for enumerators
        i = 42;

    if (a & b)
        i = 42;

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

    if (x && x)                 // TODO: warn
        i = 42;

    if (x || x)                 // TODO: warn
        i = 42;

    if ((x && x) ||             // TODO: warn
        (x && x))               // TODO: warn
        i = 42;

    const i1 = true ? 41 : 42;  // TODO: warn
    const i2 = true ? 42 : 42;  // TODO: warn
}
