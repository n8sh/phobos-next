module setops_ex;

/** Generalization for `std.algorithm.setopts.setUnion` with optimized
    special-handling of hash-set/map support.
 */
auto setUnion(T1, T2)(T1 a, T2 b)
    @trusted
{
    import std.range : CommonType, ElementType;
    import std.traits : hasMember;
    alias E = CommonType!(ElementType!T1,
                          ElementType!T2);
    static if (isMapLike!T1 &&
               isMapLike!T2)
    {
        if (a.length < b.length)
        {
            return setUnionhelper(a, b);
        }
        else
        {
            return setUnionhelper(b, a);
        }
    }
    else
    {
        import std.algorithm.sorting : merge;
        return merge(a, b);
    }
}

/** Helper function for `setUnion` that assumes `small` has shorter length than `large` . */
private static auto setUnionhelper(Small, Large)(Small small, Large large)
{
    Large united = large;
    foreach (const ref e; small.byKeyValue)
    {
        if (auto hitPtr = e.key in large)
        {
            (*hitPtr) = e.value;
        }
        else
        {
            large[e.key] = e.value;
        }
    }
    return united;
}

/** Is `true` if `Set` is set-like container, that is provides membership
    checking via the `in` operator or `contains`.
*/
template isSetLike(Set)
{
    import std.traits : hasMember;
    enum isSetLike = hasMember!(Set, "contains"); // TODO extend to check `in` operator aswell
}

/** Is `true` if `Map` is map-like container, that is provides membership
    checking via the `in` operator or `contains`.
*/
template isMapLike(Map)
{
    import std.traits : isAssociativeArray;
    enum isMapLike = isAssociativeArray!Map; // TODO check if in operator returns reference to value
}

version(unittest)
{
    import std.algorithm.comparison : equal;
}

/// union of arrays
@safe pure unittest
{
    assert(setUnion([1, 2], [2, 3]).equal([1, 2, 2, 3]));
}

/// union of associative array (via keys)
@safe pure unittest
{
    alias Map = string[int];

    Map a = [0 : "a", 1 : "b"];
    Map b = [2 : "c"];

    Map c = [0 : "a", 1 : "b", 2 : "c"];

    // test associativity
    assert(setUnion(a, b) == c);
    assert(setUnion(b, a) == c);
}
