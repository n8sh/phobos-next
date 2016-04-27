module variant_ex;

/** A variant of pointers to `Types` packed into a word (`size_t`).

    Realizes a very lightweight version of polymorphism packed inside one single
    word/pointer. Typically most significant bits are used to store type
    information. These are normally unused on 64-bit systems (tested on Linux).

    Typically used in tree-data containers to realize hybrid value (sparsely
    packed sub-tree) and pointer (to dense sub-tree) packing of sub-nodes.

    See also: http://forum.dlang.org/post/sybuoliqhhefcovxjfjv@forum.dlang.org

    TODO Make `typeIndex` start at 1 for defined types. Already defaults to zero for null. Look in variant_ex.d

    TODO Ask forum.dlang.org: Is it safe to assume that `typeBits` most significant bits of a
    pointer are zero? If not put them in least significant part.

    TODO What todo with the fact that the GC will fail to scan VariantPointer?
    Can the GC be tweaked to mask out the type bits before scanning?

    TODO Enable support for `is null` instead of `isNull`?

    TODO Use `enforce()` instead of `assert()` in VariantPointer:init()

    TODO Move to Phobos std.variant
*/
struct VariantPointer(Types...)
{
    static assert(this.sizeof == (void*).sizeof); // should have same size as pointer

    alias S = size_t; // TODO templatize?

    /// Number of bits used to represent value type pointed to.
    static      if (Types.length <= 2)   { enum typeBits = 1; }
    else static if (Types.length <= 4)   { enum typeBits = 2; }
    else static if (Types.length <= 8)   { enum typeBits = 3; }
    else static if (Types.length <= 16)  { enum typeBits = 4; }
    else static if (Types.length <= 32)  { enum typeBits = 5; }
    else static if (Types.length <= 64)  { enum typeBits = 6; }
    else static if (Types.length <= 128) { enum typeBits = 7; }
    else static if (Types.length <= 256) { enum typeBits = 8; }
    else                                 { static assert(false, "Too many Types"); }

    enum typeShift = 8*S.sizeof - typeBits;
    enum typeMask = cast(S)(2^^typeBits - 1) << typeShift;

    import std.meta : staticIndexOf;
    enum indexOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if Types.length is <= 256

    /// Is `true` iff a `T*` can be assigned to `this`.
    enum allows(T) = indexOf!T >= 0;

    extern (D) S toHash() const pure nothrow
    {
        import core.internal.hash : hashOf;
        return _raw.hashOf;
    }

    @property string toString() const @trusted // TODO pure
    {
        import std.conv : to;
        final switch (currentIndex)
        {
            foreach (const i, T; Types)
            {
            case i: return T.stringof ~ `@` ~ ptr.to!string;
            }
        }
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

    this(typeof(null) value)
    {
        // null is the default
    }

    auto opAssign(typeof(null) that)
    {
        _raw = 0;
        return this;
    }

    @property inout(void)* ptr() inout @trusted @nogc nothrow { return cast(void*)(_raw & ~typeMask); }

    @property inout(T)* peek(T)() inout @trusted @nogc nothrow
        if (allows!T)
    {
        static if (is(T == void)) static assert(allows!T, "Cannot store a " ~ T.stringof ~ " in a " ~ name);
        if (!isOfType!T) { return null; }
        return cast(inout T*)ptr;
    }

    bool opEquals(T)(T* that) const @trusted nothrow @nogc
    {
        auto x = peek!T; // if `this` contains pointer of to instance of type `T`
        return x && x == that; // and is equal to it
    }

    bool isNull() const @safe pure nothrow @nogc { return ptr is null; }

    bool opCast(T : bool)() const @safe pure nothrow @nogc { return ptr !is null; }

    private void init(T)(T* that)
        in
    {
        assert(!(cast(S)that & typeMask), "Top-most bits of pointer are already occupied"); // TODO use enforce instead?
    }
    body
    {
        _raw = (cast(S)that | // pointer in lower part
                (cast(S)(indexOf!T) << typeShift)); // use higher bits for type information
    }

    /** Get zero-offset index of current variant type. */
    private auto currentIndex() const @safe pure nothrow @nogc
    {
        return (_raw & typeMask) >> typeShift;
    }

    private bool isOfType(T)() @safe const nothrow @nogc
        if (allows!T)
    {
        return currentIndex == indexOf!T;
    }

    private S _raw;             // raw untyped word

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
    assert(vp.isNull);
    vp = null;
    assert(vp.isNull);
    assert(!vp);

    foreach (T; Types)
    {
        static assert(!__traits(compiles, { T[] a; vp = &a; }));
        static assert(!__traits(compiles, { vp.peek!(T[]*); }));

        // assignment from stack pointer
        T a = 73;
        T a_ = 73;
        vp = &a;
        assert(vp == &a);
        assert(vp != &a_);
        assert(vp);
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
        T* b_ = new T;
        *b = 73;
        *b_ = 73;
        vp = b;
        assert(vp == b);
        assert(vp != b_);
        assert(vp);
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
