module containers_ex;

version = benchmark;

import std.range : isInputRange;
private import std.experimental.allocator.mallocator : Mallocator;

/** Instantiator for $(D HashSet) in $(D containers-em).
 */
auto hashSet(Allocator = Mallocator, R)(R r)
    if (isInputRange!R)
{
    import std.range : ElementType, hasLength;
    alias E = ElementType!R; // TODO Unqual?

    import hashmap : HashSet;
    static if (is(Allocator == Mallocator))
    {
        static if (hasLength!R)
        {
            const bucketCount = r.length; // TODO use
            auto set = HashSet!(E, Allocator)();
        }
        else
        {
            auto set = HashSet!(E, Allocator)();
        }
    }
    else
    {
        static if (hasLength!R)
        {
            const bucketCount = r.length; // TODO use
            auto set = HashSet!(E, Allocator)(Allocator());
        }
        else
        {
            auto set = HashSet!(E, Allocator)(Allocator());
        }
    }

    foreach (const e; r)
    {
        set.insert(e);
    }

    return set; // make it const to indicate fixed
}

// unittest
// {
//     const x = [1, 2, 3, 2, 1];
//     auto hx = x.hashSet;
//     assert(1 in hx);
//     assert(2 in hx);
//     assert(3 in hx);
//     static assert(is(typeof(3 in hx) == const(int)*));
// }

version(none) unittest
{
    import std.range : iota;
    import std.algorithm : count;
    enum n = 1024* 1024;

    alias T = int;

    void testHashSetMallocator()
    {
        auto hx = iota!T(0, n).hashSet;
    }

    void testHashSetInSituRegion()
    {
        import std.experimental.allocator.building_blocks.region : Region, InSituRegion;
        import std.experimental.allocator.mallocator : Mallocator;
        import std.experimental.allocator.building_blocks.allocator_list : AllocatorList;

        import std.algorithm : max;
        // Create a scalable list of regions. Each gets at least 1MB at a time by using malloc.
        alias LocalAllocator = InSituRegion!(n, T.alignof);
        auto hx = iota!T(0, n).hashSet!LocalAllocator; // TODO Use LocalAllocator
    }

    void testAA()
    {
        bool[T] aa;
        foreach (const e; iota!T(0, n)) { aa[e] = true; }
    }

    import std.datetime : benchmark, Duration;
    auto r = benchmark!(testHashSetMallocator,
                        testHashSetInSituRegion,
                        testAA)(10);
    import std.stdio : writeln;
    import std.conv : to;
    writeln("testHashSetMallocator: ", to!Duration(r[0]));
    writeln("testHashSetInSituRegion:     ", to!Duration(r[1]));
    writeln("testAA:                ", to!Duration(r[2]));
}
