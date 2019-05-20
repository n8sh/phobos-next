module pure_gc_allocator;

static if (__VERSION__ >= 2087)
{
    version(LDC) static assert(0, "TODO Use std.experimental.allocator.building_blocks.gc_allocator.d instead of this module");
}

import std.experimental.allocator.common;

/**
 * D's built-in garbage-collected allocator purified.
 *
 * TODO remove and place with GCAllocator when https://github.com/dlang/phobos/pull/6432 has been merged.
 */
struct PureGCAllocator
{
    import core.memory : GC;
    import std.typecons : Ternary;

    /**
    The alignment is a static constant equal to `platformAlignment`, which
    ensures proper alignment for any D data type.
    */
    enum uint alignment = platformAlignment;

    /**
    Standard allocator methods per the semantics defined above. The $(D
    deallocate) and `reallocate` methods are `@system` because they may
    move memory around, leaving dangling pointers in user code.
    */
    pure nothrow @trusted void[] allocate(size_t bytes) shared const
    {
        if (!bytes) return null;
        auto p = GC.malloc(bytes);
        return p ? p[0 .. bytes] : null;
    }

    /// Ditto
    pure nothrow @trusted bool expand(ref void[] b, size_t delta) shared const
    {
        if (delta == 0) return true;
        if (b is null) return false;
        immutable curLength = GC.sizeOf(b.ptr);
        assert(curLength != 0); // we have a valid GC pointer here
        immutable desired = b.length + delta;
        if (desired > curLength) // check to see if the current block can't hold the data
        {
            immutable sizeRequest = desired - curLength;
            immutable newSize = GC.extend(b.ptr, sizeRequest, sizeRequest);
            if (newSize == 0)
            {
                // expansion unsuccessful
                return false;
            }
            assert(newSize >= desired);
        }
        b = b.ptr[0 .. desired];
        return true;
    }

    /// Ditto
    pure nothrow @system bool reallocate(ref void[] b, size_t newSize) shared const
    {
        import core.exception : OutOfMemoryError;
        try
        {
            auto p = cast(ubyte*) GC.realloc(b.ptr, newSize);
            b = p[0 .. newSize];
        }
        catch (OutOfMemoryError)
        {
            // leave the block in place, tell caller
            return false;
        }
        return true;
    }

    /// Ditto
    pure nothrow @trusted @nogc
    Ternary resolveInternalPointer(const void* p, ref void[] result) shared const
    {
        auto r = GC.addrOf(cast(void*) p);
        if (!r) return Ternary.no;
        result = r[0 .. GC.sizeOf(r)];
        return Ternary.yes;
    }

    /// Ditto
    pure nothrow @system @nogc
    bool deallocate(void[] b) shared const
    {
        GC.free(b.ptr);
        return true;
    }

    /// Ditto
    pure nothrow @safe @nogc
    size_t goodAllocSize(size_t n) shared const
    {
        if (n == 0)
            return 0;
        if (n <= 16)
            return 16;

        import core.bitop : bsr;

        auto largestBit = bsr(n-1) + 1;
        if (largestBit <= 12) // 4096 or less
            return size_t(1) << largestBit;

        // larger, we use a multiple of 4096.
        return ((n + 4095) / 4096) * 4096;
    }

    /**
    Returns the global instance of this allocator type. The garbage collected
    allocator is thread-safe, therefore all of its methods and `instance` itself
    are `shared`.
    */

    static shared const PureGCAllocator instance;

    // Leave it undocummented for now.
    nothrow @trusted void collect() shared const
    {
        GC.collect();
    }
}

version(none):

///
pure @system unittest
{
    auto buffer = PureGCAllocator.instance.allocate(1024 * 1024 * 4);
    // deallocate upon scope's end (alternatively: leave it to collection)
    scope(exit) PureGCAllocator.instance.deallocate(buffer);
    //...
}

pure @safe unittest
{
    auto b = PureGCAllocator.instance.allocate(10_000);
    assert(PureGCAllocator.instance.expand(b, 1));
}

pure @system unittest
{
    import core.memory : GC;
    import std.typecons : Ternary;

    // test allocation sizes
    assert((() nothrow @safe @nogc => PureGCAllocator.instance.goodAllocSize(1))() == 16);
    for (size_t s = 16; s <= 8192; s *= 2)
    {
        assert((() nothrow @safe @nogc => PureGCAllocator.instance.goodAllocSize(s))() == s);
        assert((() nothrow @safe @nogc => PureGCAllocator.instance.goodAllocSize(s - (s / 2) + 1))() == s);

        auto buffer = PureGCAllocator.instance.allocate(s);
        scope(exit) () nothrow @nogc { PureGCAllocator.instance.deallocate(buffer); }();

        void[] p;
        assert((() nothrow @safe => PureGCAllocator.instance.resolveInternalPointer(null, p))() == Ternary.no);
        assert((() nothrow @safe => PureGCAllocator.instance.resolveInternalPointer(&buffer[0], p))() == Ternary.yes);
        assert(p.ptr is buffer.ptr && p.length >= buffer.length);

        assert(GC.sizeOf(buffer.ptr) == s);

        auto buffer2 = PureGCAllocator.instance.allocate(s - (s / 2) + 1);
        scope(exit) () nothrow @nogc { PureGCAllocator.instance.deallocate(buffer2); }();

        assert(GC.sizeOf(buffer2.ptr) == s);
    }

    // anything above a page is simply rounded up to next page
    assert((() nothrow @safe @nogc => PureGCAllocator.instance.goodAllocSize(4096 * 4 + 1))() == 4096 * 5);
}

pure nothrow @safe unittest
{
    import std.typecons : Ternary;

    void[] buffer = PureGCAllocator.instance.allocate(42);
    void[] result;
    Ternary found = PureGCAllocator.instance.resolveInternalPointer(&buffer[0], result);

    assert(found == Ternary.yes && &result[0] == &buffer[0] && result.length >= buffer.length);
    assert(PureGCAllocator.instance.resolveInternalPointer(null, result) == Ternary.no);
    void *badPtr = (() @trusted => cast(void*)(0xdeadbeef))();
    assert(PureGCAllocator.instance.resolveInternalPointer(badPtr, result) == Ternary.no);
}
