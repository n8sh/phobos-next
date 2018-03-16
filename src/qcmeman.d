/// Qualified (`@safe pure nothrow @nogc`) C memory management.
module qcmeman;

extern(C)
{
    // qualified C memory allocations
    pure nothrow @nogc:

    void* malloc(size_t size) @trusted; // TODO should this and `pureMalloc` be `@system`?
    void* calloc(size_t nmemb, size_t size) @safe;
    void* realloc(void* ptr, size_t size);

    void* alloca(size_t length) @safe;

    void free(void* ptr);

    void gc_addRange( in void* p, size_t sz, const TypeInfo ti = null);
    void gc_removeRange( in void* p );
}
