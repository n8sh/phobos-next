module projectEuler;

/** Solve Euler Problem 2.
    See also: https://www.reddit.com/r/programming/comments/rif9x/uniform_function_call_syntax_for_the_d/
 */
auto problem2()
{
    import std.range : recurrence;
    import std.algorithm : until, filter, reduce;
    return recurrence!"a[n-1] + a[n-2]"(1, 1).until!"a > 4_000_000"()
                                             .filter!"a % 2 == 0"()
                                             .reduce!"a + b"();
}

unittest
{
    assert(problem2() == 4613732);
}
