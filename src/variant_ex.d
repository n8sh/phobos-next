module variant_ex;

/** A variant of `Types` packed into a word (`size_t`).

    Realizes a very lightweight version of polymorphism packed inside one single
    pointer. Typically most significant bits are used to store type
    information. These are normally unused on 64-bit systems.

    TODO Make typeIndex start at 1 for defined types. Already defaults to zero for null.

    TODO Ask forum.dlang.org: Is it safe to assume that `typeBits` most significant bits of a
    pointer are zero?

    See also: http://forum.dlang.org/post/sybuoliqhhefcovxjfjv@forum.dlang.org

    TODO What todo with the fact that the GC will fail to scan WordVariant?
    Can the GC be tweaked to mask out the type bits before scanning?

    TODO Enable support for is null instead of isNull?

    TODO Use `enforce()` instead of `assert()` in WordVariant:init()

    TODO Move to Phobos std.variant
 */
struct WordVariant(Types...)
{
    static assert(this.sizeof == (void*).sizeof); // should have same size as pointer

    alias S = size_t;

    /// Number of bits used to represent value type pointed to.
    enum typeBits = 8;

    /// Maximum number of different `Types` allowed.
    enum maxTypeCount = 2^^typeBits;

    enum typeShift = 8*S.sizeof - typeBits;
    enum typeMask = cast(S)(maxTypeCount - 1) << typeShift;

    static assert(Types.length <= maxTypeCount, "Can only represent 8 different types");

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
    }

    auto opAssign(typeof(null) that)
    {
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

    bool opEquals(U)(U* that) const @trusted nothrow @nogc
    {
        auto x = peek!U; // if `this` contains pointer of to instance of type `U`
        return x && x == that; // and is equal to it
    }

    bool isNull() const @safe pure nothrow @nogc { return ptr is null; }

    bool opCast(T : bool)() const @safe pure nothrow @nogc { return ptr !is null; }

    private void init(T)(T* that)
    in
    {
        // TODO use enforce instead?
        assert(!(cast(S)that & typeMask)); // check that top-most bits of pointer aren't already occupied
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

    alias VP = WordVariant!Types;

    VP vp;
    vp = null;
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
