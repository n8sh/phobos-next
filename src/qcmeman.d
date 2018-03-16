/// Qualified (`@safe pure nothrow @nogc`) C memory management.
module qcmeman;

extern(C)
{
    // qualified C memory allocations
    pure nothrow @nogc:

    /* See also:
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
