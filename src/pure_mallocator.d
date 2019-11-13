module pure_mallocator;

// TOODO Use std.experimental.allocator.building_blocks.mallocator instead of this module

import std.experimental.allocator.common;

/**
 * The C heap allocator purified.
 *
 * TODO Remove when https://github.com/dlang/phobos/pull/6411 has been merged
 * that adds `calloc`.
 */
struct PureMallocator
{
    const shared pure nothrow @nogc:

    /**
     * The alignment is a static constant equal to $(D platformAlignment), which
     * ensures proper alignment for any D data type.
    */
    enum uint alignment = platformAlignment;

    /**
     * Standard allocator methods per the semantics defined above. The $(D
     * deallocate) and $(D reallocate) methods are $(D @system) because they may
     * move memory around, leaving dangling pointers in user code. Somewhat
     * paradoxically, $(D malloc) is $(D @safe) but that's only useful to safe
     * programs that can afford to leak memory allocated.
     */
    void[] allocate(size_t bytes) @trusted
    {
        version(LDC) pragma(inline, true);
        if (!bytes) { return null; }
        void* p = fakePureMalloc(bytes);
        return p ? p[0 .. bytes] : null;
    }

    void[] allocateZeroed(size_t bytes) @trusted
    {
        version(LDC) pragma(inline, true);
        if (!bytes) { return null; }
        void* p = fakePureCalloc(bytes, 1);
        return p ? p[0 .. bytes] : null;
    }

    /// ditto
    bool deallocate(void[] b) @system
    {
        pragma(inline, true);
        fakePureFree(b.ptr);        // `free` doesn't need `b.length`
        // `true` indicates support,
        // See_Also: https://dlang.org/phobos/std_experimental_allocator.html#.IAllocator.deallocate
        return true;
    }

    /// ditto
    bool reallocate(ref void[] b, size_t s) @system
    {
        if (!s)
        {
            // fuzzy area in the C standard, see http://goo.gl/ZpWeSE
            // so just deallocate and nullify the pointer
            deallocate(b);
            b = null;
            return true;
        }
        ubyte* p = cast(ubyte*)fakePureRealloc(b.ptr, s);
        if (!p) { return false; }
        b = p[0 .. s];
        return true;
    }

    /// Deallocate using a pointer only like what `free` does.
    bool deallocatePtr(void* b) @system
    {
        pragma(inline, true);
        fakePureFree(b);            // `free` doesn't need `b.length`
        return true; // `true` indicates support, https://dlang.org/phobos/std_experimental_allocator.html#.IAllocator.deallocate
    }

    /**
     * Returns the global instance of this allocator type. The C heap allocator is
     * thread-safe, therefore all of its methods and `it` itself are
     * $(D x).
     */
    static shared PureMallocator instance;
}

///
@trusted pure nothrow @nogc unittest
{
    auto buf = PureMallocator.instance.allocate(16);
    assert(&buf[0]);
    assert(buf.length);

    assert(PureMallocator.instance.deallocate(buf));

    import std.experimental.allocator : makeArray;
    PureMallocator.instance.makeArray!int(64, 42);
}

extern (C) private pure @system @nogc nothrow
{
    pragma(mangle, "malloc") void* fakePureMalloc(size_t);
    pragma(mangle, "calloc") void* fakePureCalloc(size_t nmemb, size_t size);
    pragma(mangle, "realloc") void* fakePureRealloc(void* ptr, size_t size);
    pragma(mangle, "free") void fakePureFree(void* ptr);
}
