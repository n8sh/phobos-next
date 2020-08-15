/// Neither GCC 10` nor Clang 10 warn here.
void check_equal_lhs_and_rhs(int x)
{
    bool b;
    if (b && b)
        x = 42;
    if (b || b)
        x = 42;
}
