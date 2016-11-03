module setops_ex;

/** Specialization for `std.algorithm.setopts.setUnion` for AA. */
auto setUnionUpdate(T1, T2)(T1 a, T2 b)
    @trusted
    if (isAA!T1 &&
        isAA!T2)
{
    if (a.length < b.length)
    {
        return setUnionHelper(a, b);
    }
    else
    {
        return setUnionHelper(b, a);
    }
}

/** Helper function for `setUnionUpdate` that assumes `small` has shorter length than
    `large` .
*/
private static auto setUnionHelper(Small, Large)(const Small small, Large large)
{
    Large united = large.dup;   // TODO this shallow copy prevents large from being `const`
    foreach (const ref e; small.byKeyValue)
    {
        if (auto hitPtr = e.key in large)
        {
            (*hitPtr) = e.value; // TODO this potentially changes the value of
        }
        else
        {
            united[e.key] = e.value;
        }
    }
    return united;
}

/** Is `true` if `Set` is set-like container, that is provides membership
    checking via the `in` operator or `contains`.
    TODO Move to Phobos std.traits
*/
template hasContains(Set)
{
    import std.traits : hasMember;
    enum isSetOf = hasMember!(Set, "contains"); // TODO extend to check `in` operator aswell
}

/** Is `true` if `Map` is map-like container, that is provides membership
    checking via the `in` operator or `contains`.
    TODO Move to Phobos std.traits
*/
template isAA(Map)
{
    import std.traits : isAssociativeArray;
    enum isAA = isAssociativeArray!Map; // TODO check if in operator returns reference to value
}

version(unittest)
{
    import std.algorithm.comparison : equal;
}

/// union of associative array (via keys)
@safe pure unittest
{
    alias Map = string[int];

    Map a = [0 : "a", 1 : "b"];
    Map b = [2 : "c"];

    Map c = [0 : "a", 1 : "b", 2 : "c"];

    import dbgio : dln;
    // test associativity
    assert(setUnionUpdate(a, b) == c);
    assert(setUnionUpdate(b, a) == c);
}
