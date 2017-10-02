module container_algorithm;

import std.traits : hasMember;

/** Try to pop first occurrence of `needle` in `haystack` (if any).
    Returns: `true` iff pop was made, `false` otherwise.
 */
bool popFirst(C, E)(ref C haystack,
                    in E needle)
    if (hasMember!(C, "popAt"))
    // TODO activate this restriction
    // if (hasSlicing!C &&
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
