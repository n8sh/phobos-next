module pure_mallocator;

import std.experimental.allocator.common;

/**
   The C heap allocator purified.
 */
struct PureMallocator
{
    import core.memory : pureMalloc, pureCalloc, pureRealloc, pureFree;

    pure:

    /**
    The alignment is a static constant equal to $(D platformAlignment), which
    ensures proper alignment for any D data type.
    */
    enum uint alignment = platformAlignment;

    /**
    Standard allocator methods per the semantics defined above. The
    $(D deallocate) and $(D reallocate) methods are $(D @system) because they
    may move memory around, leaving dangling pointers in user code. Somewhat
    paradoxically, $(D malloc) is $(D @safe) but that's only useful to safe
    programs that can afford to leak memory allocated.
    */
    pragma(inline, true)
    void[] allocate(size_t bytes) shared
        @trusted @nogc
    {
        if (!bytes) return null;
        auto p = pureMalloc(bytes);
        return p ? p[0 .. bytes] : null; // TODO can we use .ptr?
    }

    pragma(inline, true)
    void[] zeroallocate(size_t bytes) shared
        @trusted @nogc
    {
        if (!bytes) return null;
        auto p = pureCalloc(bytes, 1);
        return p ? p[0 .. bytes] : null; // TODO can we use .ptr?
    }

    /// Ditto
    pragma(inline, true)
    bool deallocate(void[] b) shared
        @system @nogc nothrow
    {
        pureFree(b.ptr);
        return true;
    }

    /// Ditto
    bool reallocate(ref void[] b, size_t s) shared
        @system @nogc nothrow
    {
        if (!s)
        {
            // fuzzy area in the C standard, see http://goo.gl/ZpWeSE
            // so just deallocate and nullify the pointer
            deallocate(b);
            b = null;
            return true;
        }
        auto p = cast(ubyte*) pureRealloc(b.ptr, s);
        if (!p) return false;
        b = p[0 .. s];
        return true;
    }

    /**
    Returns the global instance of this allocator type. The C heap allocator is
    thread-safe, therefore all of its methods and `it` itself are
    $(D shared).
    */
    static shared PureMallocator instance;
}
