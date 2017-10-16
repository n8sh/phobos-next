module container_algorithm;

import std.traits : hasMember;

/** Try to pop first occurrence of `needle` in `haystack` (if any).
    Returns: `true` iff pop was made, `false` otherwise.
 */
bool popFirstMaybe(alias pred = "a == b", C, E)(ref C haystack,
                                                in E needle)
    if (hasMember!(C, "length") &&
        hasMember!(C, "popAt"))
    // TODO activate this restriction
    // if (hasSlicing!C &&
    //     is(ElementType!C == E.init))
{
    import std.functional : binaryFun;
    // doesn't work for uncopyable element types: import std.algorithm.searching : countUntil;
    size_t offset = 0;
    foreach (const ref e; haystack[])
    {
        if (binaryFun!pred(e, needle)) { break; }
        offset += 1;
    }
    if (offset != haystack.length)
    {
        haystack.popAt(offset);
        return true;
    }
    return false;
}

/** Remove element at index `index` in `r`.
 *
 * TODO reuse in array*.d
 * TODO better name removeAt
 */
void shiftToFrontAt(T)(T[] r, size_t index)
    @trusted
{
    assert(index + 1 <= r.length);

    // TODO use this instead:
    // immutable si = index + 1;   // source index
    // immutable ti = index;       // target index
    // immutable restLength = this.length - (index + 1);
    // moveEmplaceAll(_store.ptr[si .. si + restLength],
    //                _store.ptr[ti .. ti + restLength]);

    // for each element index that needs to be moved
    foreach (immutable i; 0 .. r.length - (index + 1))
    {
        immutable si = index + i + 1; // source index
        immutable ti = index + i;     // target index
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(r.ptr[si], // TODO remove `move` when compiler does it for us
                    r.ptr[ti]);
    }
}

@safe pure nothrow @nogc unittest
{
    int[4] x = [11, 12, 13, 14];
    x[].shiftToFrontAt(1);
    assert(x == [11, 13, 14, 14]);
}

version(unittest)
{
    // import array_help : s;
    // import dbgio;
}
