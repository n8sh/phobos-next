module variant_ex;

/** A variant of `Types` packed into a word (`size_t`).

    Realizes a very lightweight version of polymorphism packed inside one single
    word/pointer. Typically most significant bits are used to store type
    information. These are normally unused on 64-bit systems (tested on Linux).

    Typically used in tree-data containers to realize hybrid value (sparsely
    packed sub-tree) and pointer (to dense sub-tree) packing of sub-nodes.

    TODO Make `typeIndex` start at 1 for defined types. Already defaults to zero for null.

    TODO Ask forum.dlang.org: Is it safe to assume that `typeBits` most significant bits of a
    pointer are zero?

    See also: http://forum.dlang.org/post/sybuoliqhhefcovxjfjv@forum.dlang.org

    TODO What todo with the fact that the GC will fail to scan WordVariant?
    Can the GC be tweaked to mask out the type bits before scanning?

    TODO Enable support for `is null` instead of `isNull`?

    TODO Use `enforce()` instead of `assert()` in WordVariant:init()

    TODO Move to Phobos std.variant
 */
struct WordVariant(Types...)
{
    // TODO assert that each Type in Types is either a pointer or has size equal to 7

    import std.meta : staticIndexOf;
    import traits_ex : allSame, sizesOf;

    enum typeSizes = sizesOf!Types;
    // static assert(allSame!typeSizes, "Types must all be of equal size");

    static assert(this.sizeof == size_t.sizeof, "Types must all <= size_t.sizeof");

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
            case i + 1: return T.stringof ~ `@` ~ peek!T.to!string;
            }
        }
    }

    pure nothrow @nogc:

    this(T)(T value)
        if (allows!T)
    {
        init(value);
    }

    this(typeof(null) value)
    {
        _raw = S.init;
    }

    auto opAssign(T)(T that)
        if (allows!T)
    {
        init(that);
        return this;
    }

    auto opAssign(typeof(null) that)
    {
        _raw = S.init;
        return this;
    }

    /** Reference. */
    static private struct Ref(T)
    {
        S _raw;
        const bool defined;
        bool opCast(T : bool)() const @safe pure nothrow @nogc { return defined; }
        T opUnary(string op : "*")() inout { return cast(T)(_raw & ~typeMask); }
    }

    @property inout(Ref!T) peek(T)() inout @trusted @nogc nothrow
        if (allows!T)
    {
        if (!isOfType!T) { return typeof(return).init; }
        return typeof(return)(_raw, true);
    }

    bool opEquals(T)(T that) const @trusted nothrow @nogc
    {
        auto x = peek!T; // if `this` contains pointer of to instance of type `T`
        return x && *x == that; // and is equal to it
    }

    bool isNull() const @safe pure nothrow @nogc { return _raw == 0; }

    bool opCast(T : bool)() const @safe pure nothrow @nogc { return !isNull; }

    private void init(T)(T that)
    in
    {
        assert(!(cast(S)that & typeMask), "Top-most bits of pointer are already occupied"); // TODO use enforce instead?
    }
    body
    {
        _raw = (cast(S)that | // data in lower part
                (cast(S)(indexOf!T + 1) << typeShift)); // use higher bits for type information
    }

    /** Get zero-offset index of current variant type. */
    private auto currentIndex() const @safe pure nothrow @nogc
    {
        return ((_raw & typeMask) >> typeShift);
    }

    private bool isOfType(T)() @safe const nothrow @nogc
        if (allows!T)
    {
        return !isNull && currentIndex == indexOf!T + 1;
    }

    private S _raw;             // raw untyped word

}

///
pure nothrow unittest
{
    import std.meta : AliasSeq;
    import dbg : dln;

    alias Types = AliasSeq!(byte*, short*, int*, long*,
                            ubyte*, ushort*, uint*, ulong*,
                            float*, double*, real*,
                            char*, wchar*, dchar*);

    alias VP = WordVariant!Types;

    VP vp;
    assert(vp.isNull);
    vp = null;
    assert(vp.isNull);
    assert(!vp);

    foreach (Tp; Types)
    {
        alias T = typeof(*Tp.init);

        static assert(!__traits(compiles, { T[] a; vp = &a; }));
        static assert(!__traits(compiles, { vp.peek!(T[]*); }));

        // assignment from stack pointer
        T a = 73;
        T a_ = 73;

        vp = &a;
        assert(vp);
        assert(!vp.isNull);
        assert(vp.currentIndex != 0);
        assert(vp.isOfType!Tp);

        assert(vp == &a);
        assert(vp != &a_);
        assert(vp);

        foreach (Up; Types)
        {
            alias U = typeof(*Up.init);
            static if (is(T == U))
            {
                assert(vp.peek!Up);
                assert(*(vp.peek!Up) == &a);
            }
            else
            {
                assert(!vp.peek!Up);
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
        foreach (Up; Types)
        {
            alias U = typeof(*Up.init);
            static if (is(T == U))
            {
                assert(vp.peek!Up);
                assert(*(vp.peek!Up) == b);
            }
            else
            {
                assert(!vp.peek!Up);
            }
        }

    }
}
