module eager_algorithms;

import std.functional : unaryFun;

import typecons_ex : hasIndexing;

version(unittest)
{
    import std.algorithm.comparison : equal;
    import dbgio : dln;
}

/** Returns: `r` eagerly in-place filtered on `predicate`. */
C filteredInplace(alias predicate, C)(C r)
    if (is(typeof(unaryFun!predicate)) &&
        hasIndexing!C)
{
    import std.traits : hasElaborateDestructor;
    import std.range.primitives : ElementType;
    import std.algorithm.mutation : move;

    alias pred = unaryFun!predicate;

    size_t dstIx = 0;           // destination index

    // skip leading passing elements
    foreach (ref e; r)
    {
        if (!pred(e)) { break; }
        dstIx += 1;
    }

    // inline filtering
    foreach (immutable srcIx; dstIx + 1 .. r.length)
    {
        if (pred(r[srcIx]))
        {
            move(r[srcIx], r[dstIx]);
            dstIx += 1;
        }
        else
        {
            static if (hasElaborateDestructor!(ElementType!C))
            {
                .destroy(e);
            }
        }
    }

    r.length = dstIx;           // truncate

    return move(r);
}

pure nothrow @nogc unittest
{
    import unique_range : intoUniqueRange;
    import array_ex : UncopyableArray;
    alias A = UncopyableArray!int;

    immutable int[6] c1 = [3, 11, 13, 15, 17, 19];
    assert(A.withElements(3, 11, 12, 13, 14, 15, 16, 17, 18, 19)
            .filteredInplace!(_ => _ & 1)
            .intoUniqueRange()
            .equal(c1[]));

    immutable int[6] c2 = [3, 11, 12, 13, 14, 15];
    assert(A.withElements(3, 11, 12, 13, 14, 15, 16, 17, 18, 19)
            .filteredInplace!(_ => _ <= 15)
            .intoUniqueRange()
            .equal(c2[]));
}
