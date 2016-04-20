module variant_pointer;

version(unittest)
{
    import dbg : dln;
}

/** A variant pointer to either of `Types`.

    Realizes a very lightweight version of polymorphism packed inside one single
    pointer. Typically most significant bits are used to store type
    information. These are normally unused on modern systems.

    TODO Ask forum.dlang.org: Is it safe to assume that `typeBits` most significant bits of a
    pointer are zero?

    See also: http://forum.dlang.org/post/sybuoliqhhefcovxjfjv@forum.dlang.org
 */
struct VariantPointer(Types...)
{
    static assert(this.sizeof == (void*).sizeof); // should have same size as pointer

    alias S = size_t;
    private enum N = Types.length; // useful local shorthand

    /// Number of bits used to represent value type pointed to.
    enum typeBits = 8;

    /// Maximum number of different `Types` allowed.
    enum maxTypeCount = 2^^typeBits;

    enum typeShift = 8*S.sizeof - typeBits;
    enum typeMask = cast(S)(maxTypeCount - 1) << typeShift;

    static assert(N <= maxTypeCount, "Can only represent 8 different types");

    import std.meta : staticIndexOf;
    enum indexOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if N is <= 256

    /// Is `true` iff a `T*` can be assigned to `this`.
    enum allows(T) = indexOf!T >= 0;

    extern (D) S toHash() const pure nothrow
    {
        import core.internal.hash : hashOf;
        return _raw.hashOf;
    }

    pure nothrow @nogc:

    this(T)(T* value)
        if (allows!T)
    {
        init(value);
    }

    auto opAssign(T)(T* that)
        if (allows!T)
    {
        init(that);
        return this;
    }

    private void init(T)(T* that)
    in
    {
        assert(!(cast(S)that & typeMask)); // check that top-most bits of pointer aren't already occupied
    }
    body
    {
        _raw = (cast(S)that | // pointer in lower part
                (cast(S)(indexOf!T) << typeShift)); // use higher bits for type information
    }

    private bool isOfType(T)() @safe const nothrow @nogc
        if (allows!T)
    {
        return ((_raw & typeMask) >> typeShift) == indexOf!T;
    }

    @property inout(T)* peek(T)() inout @trusted @nogc nothrow
        if (allows!T)
    {
        static if (is(T == void)) static assert(allows!T, "Cannot store a " ~ T.stringof ~ " in a " ~ name);
        if (!isOfType!T) { return null; }
        return cast(inout T*)(cast(S)_raw & ~typeMask);
    }

    private S _raw;
}

///
pure nothrow unittest
{
    import std.meta : AliasSeq;

    alias Types = AliasSeq!(byte, short, int, long,
                            ubyte, ushort, uint, ulong,
                            float, double, real,
                            char, wchar, dchar);

    alias VP = VariantPointer!Types;

    VP vp;

    foreach (T; Types)
    {
        static assert(!__traits(compiles, { T[] a; vp = &a; }));
        static assert(!__traits(compiles, { vp.peek!(T[]*); }));

        // assignment from stack pointer
        T a = 73;
        vp = &a;
        foreach (U; Types)
        {
            static if (is(T == U))
            {
                assert(vp.peek!U);
                assert(*(vp.peek!U) == a);
            }
            else
            {
                assert(!vp.peek!U);
            }
        }

        // assignment from heap pointer
        T* b = new T;
        *b = 73;
        vp = b;
        foreach (U; Types)
        {
            static if (is(T == U))
            {
                assert(vp.peek!U);
                assert(*(vp.peek!U) == *b);
            }
            else
            {
                assert(!vp.peek!U);
            }
        }

    }
}
