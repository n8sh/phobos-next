module array_traits;

/// Is `true` iff `T` is a slice of `char`s.
enum isCharsSlice(T) = (is(T : const(char)[]));

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
 */
template isEqualableSlices(Ts...)
if (Ts.length >= 2)
{
    private enum isSlice(T) = is(T : const(E)[], E);
    private enum isSliceOf(T, E) = is(T : const(E)[]);
    static if (isSlice!(Ts[0]))
    {
        alias E = typeof(Ts[0].init[0]);
        static foreach (T; Ts[1 .. $])
        {
            static if (is(typeof(isEqualableSlices) == void) && // not yet defined
                       !(isSliceOf!(T, E)))
            {
                enum isEqualableSlices = false;
            }
        }
        static if (is(typeof(isEqualableSlices) == void)) // if not yet defined
        {
            enum isEqualableSlices = true;
        }
    }
    else
    {
        enum isEqualableSlices = false;
    }
}

///
@safe pure unittest
{
    static assert(isEqualableSlices!(int[], int[]));
    static assert(isEqualableSlices!(const(int)[], int[]));
    static assert(isEqualableSlices!(int[], const(int)[]));
    static assert(isEqualableSlices!(int[], immutable(int)[]));

    static assert(isEqualableSlices!(int[], int[], int[]));
    static assert(isEqualableSlices!(int[], const(int)[], int[]));
    static assert(isEqualableSlices!(int[], const(int)[], immutable(int)[]));
    static assert(isEqualableSlices!(const(int)[], const(int)[], const(int)[]));

    static assert(!isEqualableSlices!(int, char));
    static assert(!isEqualableSlices!(int, const(char)));
    static assert(!isEqualableSlices!(int, int));

    static assert(!isEqualableSlices!(int[], char[]));
    static assert(!isEqualableSlices!(int[], char[], char[]));
    static assert(!isEqualableSlices!(char[], int[]));
}
