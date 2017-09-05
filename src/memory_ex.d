module memory_ex;

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
void* pureMalloc(size_t size) @trusted pure @nogc nothrow
{
    const errno = fakePureGetErrno();
    void* ret = fakePureMalloc(size);
    if (!ret || errno != 0)
    {
        cast(void)fakePureSetErrno(errno);
    }
    return ret;
}
/// ditto
void* pureCalloc(size_t nmemb, size_t size) @trusted pure @nogc nothrow
{
    const errno = fakePureGetErrno();
    void* ret = fakePureCalloc(nmemb, size);
    if (!ret || errno != 0)
    {
        cast(void)fakePureSetErrno(errno);
    }
    return ret;
}
/// ditto
void* pureRealloc(void* ptr, size_t size) @system pure @nogc nothrow
{
    const errno = fakePureGetErrno();
    void* ret = fakePureRealloc(ptr, size);
    if (!ret || errno != 0)
    {
        cast(void)fakePureSetErrno(errno);
    }
    return ret;
}
/// ditto
void pureFree(void* ptr) @system pure @nogc nothrow
{
    const errno = fakePureGetErrno();
    fakePureFree(ptr);
    cast(void)fakePureSetErrno(errno);
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
