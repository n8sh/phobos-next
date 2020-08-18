/// Neither GCC 10` nor Clang 10 warn here.
void check_equal_lhs_and_rhs(int i)
{
    bool x, y;
    alias xa = x;

    enum { a = 0, b = 1 }
    enum { x1 = (0 | 1), x2 }

    if (a & a)
        i = 42;

    if (b & b)
        i = 42;

    if (a & b)
        i = 42;

    if (1 & 2)
        i = 42;

    if (false & false)
        i = 42;

    if (true & true)
        i = 42;

    if (x1 & x1)
        i = 42;

    if (x & x)                  // warn
        i = 42;

    i = x + x;
    i = x - x;
    i = x * x;

    if (x & xa)                 // warn
        i = 42;

    if (x & y)
        i = 42;

    if (x | x)                  // warn
        i = 42;

    if (x & x |                 // warn
        x & x)                  // warn
        i = 42;

    if (x && x)                 // warn
        i = 42;

    if (x || x)                 // warn
        i = 42;

    if ((x && x) ||             // warn
        (x && x))               // warn
        i = 42;

    const i1 = true ? 42 : 42;
    const i2 = true ? a : a;
    const i3 = true ? x : x;    // warn
}
