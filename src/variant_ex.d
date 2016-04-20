module variant_ex;

/** A variant pointer to either of `Types`.

    Realizes a very lightweight version of polymorphism packed inside one single
    pointer. Typically the three least significant bits are used to store type
    information.
 */
struct VariantPointer(Types...)
{
    enum maxCount = (void*).alignof; // lookup pointer alignment
    private enum N = Types.length; // useful local shorthand

    import std.meta : staticIndexOf;
    enum tixOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if N is <= 256

    enum bool allows(T) = tixOf!T >= 0;
    static assert(Types.length <= maxCount, "Can only represent 8 different types");
    union
    {
        void* _raw;
    }
    static assert(this.sizeof == (void*).sizeof); // should have same size as pointer

    this(T)(T* value) @safe @nogc nothrow
        if (allows!T)
    in
    {
        assert((cast(size_t)value & (maxCount-1)) == 0);
    }
    body
    {
        init(value);
    }

    auto opAssign(T)(T* that) @trusted @nogc nothrow
        if (allows!T)
    in
    {
        assert((cast(size_t)that & (maxCount-1)) == 0);
    }
    body
    {
        init(that);
        return this;
    }

    private void init(T)(T that) @trusted @nogc nothrow
    {
        _raw = cast(void*)that;
        // _tix = cast(Ix)tixOf!U; // set type tag
    }
}

pure nothrow unittest
{
    alias VP = VariantPointer!(byte, short, int, long,
                               float, double, real, char);
    VP x = cast(byte*)16;
    x = cast(short*)32;

    // TODO how to check this?
    // import std.exception : assertThrown;
    // assertThrown(x = cast(short*)1);
    // assertThrown(x = cast(short*)15);
}
