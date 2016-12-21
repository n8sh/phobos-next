module eager_algorithms;

import std.functional : unaryFun;

import typecons_ex : hasIndexing;

/** Returns: `r` eagerly in-place filtered on `predicate`. */
C filteredInplace(alias predicate, C)(C r)
    if (is(typeof(unaryFun!predicate)) &&
        hasIndexing!C)
{
    import std.traits : hasElaborateDestructor;
    import std.range.primitives : ElementType;
    import std.algorithm.mutation : move;

    alias pred = unaryFun!predicate;

    size_t count = 0;

    // skip leading filtered elements
    foreach (ref e; r)
    {
        if (!pred(e)) { break; }
        count += 1;
    }

    foreach (immutable ix, ref e; r)
    {
        if (pred(e))
        {
            move(e, r[count]);
            count += 1;
        }
        else
        {
            static if (hasElaborateDestructor!(ElementType!C))
            {
                .destroy(e);
            }
        }
    }

    r.length = count;           // truncate

    return move(r);
}

version(unittest)
{
    import std.algorithm.comparison : equal;
}

pure nothrow @nogc unittest
{
    import unique_range : intoUniqueRange;
    import array_ex : UncopyableArray;
    alias A = UncopyableArray!int;
    immutable int[5] c = [1, 3, 5, 7, 9];
    assert(A.withElements(1, 2, 3, 4, 5, 6, 7, 8, 9)
                  .filteredInplace!(_ => _ & 1)
                  .intoUniqueRange()
            .equal(c[]));
}
