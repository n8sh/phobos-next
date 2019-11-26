/// Qualified (`@safe pure nothrow @nogc`) C memory management.
module nxt.qcmeman;

extern(C)
{
    // qualified C memory allocations
    pure nothrow @nogc:

    /* See_Also:
     * https://forum.dlang.org/post/mailman.1130.1521239659.3374.digitalmars-d@puremagic.com
     * for an explanation of why `pureMalloc` and `pureCalloc` both can
     * be @trusted. */
    void* malloc(size_t size) @trusted;

    void* calloc(size_t nmemb, size_t size) @trusted;

    void* realloc(void* ptr, size_t size) @system;

    void* alloca(size_t length) @safe;

    void free(void* ptr) @system;

    void gc_addRange( in void* p, size_t sz, const TypeInfo ti = null);
    void gc_removeRange( in void* p );
}

/**
 * Pure variants of C's memory allocation functions `malloc`, `calloc`, and
 * `realloc` and deallocation function `free`.
 *
 * Purity is achieved by saving and restoring the value of `errno`, thus
 * behaving as if it were never changed.
 *
 * See_Also:
 *     $(LINK2 https://dlang.org/spec/function.html#pure-functions, D's rules for purity),
 *     which allow for memory allocation under specific circumstances.
 */
private void* pureMalloc(size_t size) @trusted pure @nogc nothrow
{
    debug const errno = fakePureGetErrno();
    void* ret = fakePureMalloc(size);
    debug if (!ret || errno != 0)
    {
        cast(void)fakePureSetErrno(errno);
    }
    return ret;
}
/// ditto
private void* pureCalloc(size_t nmemb, size_t size) @trusted pure @nogc nothrow
{
    debug const errno = fakePureGetErrno();
    void* ret = fakePureCalloc(nmemb, size);
    debug if (!ret || errno != 0)
    {
        cast(void)fakePureSetErrno(errno);
    }
    return ret;
}
/// ditto
private void* pureRealloc(void* ptr, size_t size) @system pure @nogc nothrow
{
    debug const errno = fakePureGetErrno();
    void* ret = fakePureRealloc(ptr, size);
    debug if (!ret || errno != 0)
    {
        cast(void)fakePureSetErrno(errno);
    }
    return ret;
}
/// ditto
void pureFree(void* ptr) @system pure @nogc nothrow
{
    debug const errno = fakePureGetErrno();
    fakePureFree(ptr);
    debug cast(void)fakePureSetErrno(errno);
}

// locally purified for internal use here only
extern (C) private pure @system @nogc nothrow
{
    pragma(mangle, "getErrno") int fakePureGetErrno();
    pragma(mangle, "setErrno") int fakePureSetErrno(int);

    pragma(mangle, "malloc") void* fakePureMalloc(size_t);
    pragma(mangle, "calloc") void* fakePureCalloc(size_t nmemb, size_t size);
    pragma(mangle, "realloc") void* fakePureRealloc(void* ptr, size_t size);

    pragma(mangle, "free") void fakePureFree(void* ptr);
}
