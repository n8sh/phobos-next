module container_algorithm;

import std.range.primitives : hasSlicing;

/** Try to pop first occurrence of `needle` in `haystack` (if any).
    Returns: `true` iff pop was made, `false` otherwise.
 */
bool popFirst(C, E)(ref C haystack,
                    in E needle)
    if (__traits(hasMember, C, `popAt`) &&
        hasSlicing!C)
    // TODO activate this restriction
    // if (isContainer!C &&
    //     is(ElementType!C == E.init))
{
    import std.algorithm.searching : countUntil;
    immutable offset = haystack[].countUntil(needle);
    if (offset != -1)
    {
        haystack.popAt(offset);
        return true;
    }
    return false;
}
