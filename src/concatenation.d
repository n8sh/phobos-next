module concatenation;

import std.traits : isStaticArray;
import std.meta : allSatisfy;

/// Sum of the lengths of the static arrays 'A'.
template sumOfLengths(A...)
    if (A.length)
{
    static if (A.length == 1)
    {
        enum sumOfLengths = A[0].length;
    }
    else
    {
        enum sumOfLengths = A[0].length + sumOfLengths!(A[1 .. $]);
    }
}

@safe pure nothrow @nogc unittest
{
    int[2] x, y, z;
    static assert(sumOfLengths!(x, y, z) == 6);
}

/** Returns: concatenation of the static arrays `Args` as a static array.
 * Move to Phobos's std.array.
 */
ElementType!(Args[0])[sumOfLengths!Args] concatenate(Args...)(Args args)
    if (allSatisfy!(isStaticArray, Args))
{
    typeof(return) result = void; // @trusted
    foreach (const i, arg; args)
    {
        static if (i == 0)
        {
            enum offset = 0;
        }
        else
        {
            enum offset = sumOfLengths!(args[0 .. i]);
        }
        result[offset .. offset + arg.length] = arg[];
    }
    return result;
}

private alias ElementType(A : E[n], E, size_t n) = E;

@safe pure nothrow @nogc unittest
{
    int[2] x = [1, 2];
    const int[2] y = [3, 4];
    auto z = concatenate(x, y);
    static assert(is(typeof(z) == int[4]));
    assert(z == [1, 2, 3, 4]);
}
