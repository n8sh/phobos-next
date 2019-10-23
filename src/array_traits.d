module array_traits;

/// Is `true` iff `T` is a slice of `char`s.
enum isCharsSlice(T) = (is(T : const(char)[])); // TODO rename to `isCharArray`

///
@safe pure unittest
{
    static assert(isCharsSlice!(char[]));
    static assert(isCharsSlice!(const(char[])));
    static assert(isCharsSlice!(const char[]));
    static assert(isCharsSlice!(const const(char)[]));

    static assert(!isCharsSlice!(wstring));
    static assert(!isCharsSlice!(dstring));
}

/** Is `true` iff all `Ts` are slices with same unqualified matching element types.
 *
 * Used to define template-restrictions on template parameters of only arrays
 * (slices) of the same unqualified element types.
 */
template isSameArrays(Ts...)
if (Ts.length >= 2)
{
    enum isSlice(T) = is(T : const(E)[], E);
    enum isSliceOf(T, E) = is(T : const(E)[]);
    static if (isSlice!(Ts[0]))
    {
        alias E = typeof(Ts[0].init[0]);
        static foreach (T; Ts[1 .. $])
        {
            static if (is(typeof(isSameArrays) == void) && // not yet defined
                       !(isSliceOf!(T, E)))
            {
                enum isSameArrays = false;
            }
        }
        static if (is(typeof(isSameArrays) == void)) // if not yet defined
        {
            enum isSameArrays = true;
        }
    }
    else
    {
        enum isSameArrays = false;
    }
}

///
@safe pure unittest
{
    static assert(isSameArrays!(int[], int[]));
    static assert(isSameArrays!(const(int)[], int[]));
    static assert(isSameArrays!(int[], const(int)[]));
    static assert(isSameArrays!(int[], immutable(int)[]));

    static assert(isSameArrays!(int[], int[], int[]));
    static assert(isSameArrays!(int[], const(int)[], int[]));
    static assert(isSameArrays!(int[], const(int)[], immutable(int)[]));
    static assert(isSameArrays!(const(int)[], const(int)[], const(int)[]));

    static assert(!isSameArrays!(int, char));
    static assert(!isSameArrays!(int, const(char)));
    static assert(!isSameArrays!(int, int));

    static assert(!isSameArrays!(int[], char[]));
    static assert(!isSameArrays!(int[], char[], char[]));
    static assert(!isSameArrays!(char[], int[]));

    static assert(!isSameArrays!(char[], dchar[]));
    static assert(!isSameArrays!(wchar[], dchar[]));
    static assert(!isSameArrays!(char[], wchar[]));
}
