/** Lightweight versions of polymorphism packed inside one single
    word/pointer.

    Most significant bits are used to store type information.

    These higher bits are normally unused on 64-bit systems (tested on
    Linux). 16 higher bits are, on Linux, either 1 (kernel-space) or 0 (user-space).

    See also: http://forum.dlang.org/post/sybuoliqhhefcovxjfjv@forum.dlang.org

    TODO Ask forum.dlang.org: Is it safe to assume that `typeBits` most significant bits of a
    pointer are zero? If not put them in least significant part.

    TODO What todo with the fact that the GC will fail to scan WordVariant?
    Can the GC be tweaked to mask out the type bits before scanning?

    TODO Enable support for `is null` instead of `isNull`?

    TODO Use `enforce()` instead of `assert()` in WordVariant:init()

    TODO Move to Phobos std.variant
*/
module variant_ex;

import std.meta : staticIndexOf;

static private template bitsNeeeded(size_t length)
{
    static      if (length <= 2)   { enum bitsNeeeded = 1; }
    else static if (length <= 4)   { enum bitsNeeeded = 2; }
    else static if (length <= 8)   { enum bitsNeeeded = 3; }
    else static if (length <= 16)  { enum bitsNeeeded = 4; }
    else static if (length <= 32)  { enum bitsNeeeded = 5; }
    else static if (length <= 64)  { enum bitsNeeeded = 6; }
    else static if (length <= 128) { enum bitsNeeeded = 7; }
    else static if (length <= 256) { enum bitsNeeeded = 8; }
    else                           { static assert(false, `Too large length`); }
}

/** A variant of `Types` packed into a word (`size_t`).

    Suitable for use in tree-data containers, such as radix trees (tries), where
    hybrid value (sparsely packed sub-tree) and pointer (to dense sub-tree)
    packing of sub-nodes is needed.
 */
struct WordVariant(Types...)
{
    import traits_ex : allSame, sizesOf;
    static assert(allSame!(sizesOf!(Types)), "Types must have equal size");
    static assert(this.sizeof == (void*).sizeof, "Size must be same as word (pointer)");

    alias S = size_t; // TODO templatize?

    import typecons_ex : makeEnumFromSymbolNames;
    alias Ix = makeEnumFromSymbolNames!(`ix_`, ``, true, Types);
    static assert(Ix.undefined == 0);

    enum typeBits = bitsNeeeded!(Types.length); // number of bits needed to represent variant type
    enum typeShift = 8*S.sizeof - typeBits;
    enum typeMask = cast(S)(2^^typeBits - 1) << typeShift;

    enum indexOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if Types.length is <= 256

    /// Is `true` iff a `T` can be stored.
    enum canStore(T) = indexOf!T >= 0;

    pure:

    @property string toString() const @trusted // TODO pure
    {
        import std.conv : to;
        final switch (typeIndex) // typeIndex starts at 0 (undefined)
        {
            foreach (const i, T; Types)
            {
            case i + 1: return T.stringof ~ `@` ~ peek!T.to!string; // which means that we must add 1
            }
        }
    }

    nothrow:

    extern (D) auto toHash() const
    {
        import core.internal.hash : hashOf;
        return _raw.hashOf;
    }

    @nogc:

    /// Construction from `value`.
    this(T)(T value) if (canStore!T) { init(value); }
    /// ditto
    this(typeof(null) value) { _raw = S.init; }

    /// Assignment from `that`.
    auto ref opAssign(T)(T that) if (canStore!T) { init(that); return this; }
    /// ditto
    auto ref opAssign(typeof(null) that) { _raw = S.init; return this; }

pragma(inline):

    /** Reference to peek of type `T`. */
    static private struct Ref(T)
    {
        S _raw;
        const bool defined;
        pragma(inline):
        bool opCast(T : bool)() const { return defined; }
        T opUnary(string op : `*`)() @trusted inout { return cast(T)(_raw & ~typeMask); }
    }

    @property inout(Ref!T) peek(T)() inout @trusted if (canStore!T)
    {
        if (!isOfType!T) { return typeof(return).init; }
        return typeof(return)(_raw, true);
    }

    bool opEquals(T)(T that) const @trusted
    {
        auto x = peek!T; // if `this` contains pointer of to instance of type `T`
        return x && *x == that; // and is equal to it
    }

    bool isNull() const { return _raw == S.init; }

    bool opCast(T : bool)() const { return !isNull; }

    private void init(T)(T that) @trusted
    in
    {
        assert(!((*(cast(S*)(&that))) & typeMask), `Top-most bits of parameter is already occupied`); // TODO use enforce instead?
    }
    body
    {
        _raw = ((*(cast(S*)(&that))) | // data in lower part
                (cast(S)(indexOf!T + 1) << typeShift)); // use higher bits for type information
    }

    private bool isOfType(T)() const if (canStore!T)
    {
        return !isNull && typeIndex == indexOf!T + 1;
    }

    inout(T) as(T)() inout @trusted if (canStore!T)
    in
    {
        assert(isOfType!T);
    }
    body
    {
        inout x = rawValue;
        return *(cast(typeof(return)*)(cast(void*)&x)); // reinterpret
    }

    /** Get zero-offset index as `Ix` of current variant type. */
    Ix typeIx() const
    {
        return cast(typeof(return))typeIndex;
    }

    /** Get zero-offset index of current variant type. */
    private auto typeIndex() const
    {
        return ((_raw & typeMask) >> typeShift);
    }

    private S rawValue() const { return _raw & ~typeMask; }

    private S _raw;             // raw untyped word

}

///
pure nothrow unittest
{
    import std.meta : AliasSeq;

    alias Types = AliasSeq!(byte*, short*, int*, long*,
                            ubyte*, ushort*, uint*, ulong*,
                            float*, double*, real*,
                            char*, wchar*, dchar*);

    alias V = WordVariant!Types;

    V v;
    assert(v.isNull);
    v = null;
    assert(v.isNull);
    assert(!v);

    foreach (Tp; Types)
    {
        alias T = typeof(*Tp.init);

        static assert(!__traits(compiles, { T[] a; v = &a; }));
        static assert(!__traits(compiles, { v.peek!(T[]*); }));

        // assignment from stack pointer
        T a = 73;
        T a_ = 73;

        v = &a;
        assert(v);
        assert(!v.isNull);
        assert(v.typeIndex != 0);
        assert(v.isOfType!Tp);

        assert(v == &a);
        assert(v != &a_);
        assert(v);

        foreach (Up; Types)
        {
            alias U = typeof(*Up.init);
            static if (is(T == U))
            {
                assert(v.peek!Up);
                assert(*(v.peek!Up) == &a);
                assert(v.as!Up == &a);
            }
            else
            {
                assert(!v.peek!Up);
            }
        }

        // assignment from heap pointer
        T* b = new T;
        T* b_ = new T;
        *b = 73;
        *b_ = 73;
        v = b;
        assert(v == b);
        assert(v != b_);
        assert(v);
        foreach (Up; Types)
        {
            alias U = typeof(*Up.init);
            static if (is(T == U))
            {
                assert(v.peek!Up);
                assert(*(v.peek!Up) == b);
            }
            else
            {
                assert(!v.peek!Up);
            }
        }

    }
}

/** A typed pointer to a variant of `Types`, packed into a word (`size_t`).
    See also: `std.bitmanip.taggedPointer`.
 */
struct VariantPointerTo(Types...)
{
    static assert(this.sizeof == (void*).sizeof, "Size must be same as word (pointer)");

    alias S = size_t; // TODO templatize?

    enum typeBits = bitsNeeeded!(Types.length); // number of bits needed to represent variant type
    enum typeShift = 8*S.sizeof - typeBits;
    enum typeMask = cast(S)(2^^typeBits - 1) << typeShift;

    enum indexOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if Types.length is <= 256

    /// Is `true` iff a pointer to a `T` can be stored.
    enum canStorePointerTo(T) = indexOf!T >= 0;

    pure:

    @property string toString() const @trusted // TODO pure
    {
        import std.conv : to;
        final switch (typeIndex)
        {
            foreach (const i, T; Types)
            {
            case i: return T.stringof ~ `@` ~ ptr.to!string;
            }
        }
    }

    nothrow:

    extern (D) auto toHash() const
    {
        import core.internal.hash : hashOf;
        return _raw.hashOf;
    }

    @nogc:

    /// Construction from `value`.
    this(T)(T* value) if (canStorePointerTo!T) { init(value); }
    /// ditto
    this(typeof(null) value) { /* null is the default */ }

    /// Assignment from `that`.
    auto ref opAssign(T)(T* that) if (canStorePointerTo!T) { init(that); return this; }
    /// ditto
    auto ref opAssign(typeof(null) that) { _raw = S.init; return this; }

    @property inout(void)* ptr() inout @trusted { return cast(void*)(_raw & ~typeMask); }

    @property inout(T)* peek(T)() inout @trusted if (canStorePointerTo!T)
    {
        static if (is(T == void)) static assert(canStorePointerTo!T, `Cannot store a ` ~ T.stringof ~ ` in a ` ~ name);
        if (!pointsTo!T) { return null; }
        return cast(inout T*)ptr;
    }

    bool opEquals(T)(T* that) const @trusted
    {
        auto x = peek!T; // if `this` contains pointer of to instance of type `T`
        return x && x == that; // and is equal to it
    }

    bool isNull() const { return ptr is null; }

    bool opCast(T : bool)() const { return ptr !is null; }

    private void init(T)(T* that)
    in
    {
        assert(!(cast(S)that & typeMask), `Top-most bits of pointer are already occupied`); // TODO use enforce instead?
    }
    body
    {
        _raw = (cast(S)that | // pointer in lower part
                (cast(S)(indexOf!T) << typeShift)); // use higher bits for type information
    }

    /** Get zero-offset index of current variant type. */
    private auto typeIndex() const
    {
        return (_raw & typeMask) >> typeShift;
    }

    private bool pointsTo(T)() const if (canStorePointerTo!T)
    {
        return typeIndex == indexOf!T;
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

    alias V = VariantPointerTo!Types;

    V v;
    assert(v.isNull);
    v = null;
    assert(v.isNull);
    assert(!v);

    foreach (T; Types)
    {
        static assert(!__traits(compiles, { T[] a; v = &a; }));
        static assert(!__traits(compiles, { v.peek!(T[]*); }));

        // assignment from stack pointer
        T a = 73;
        T a_ = 73;
        v = &a;
        assert(v == &a);
        assert(v != &a_);
        assert(v);
        foreach (U; Types)
        {
            static if (is(T == U))
            {
                assert(v.peek!U);
                assert(*(v.peek!U) == a);
            }
            else
            {
                assert(!v.peek!U);
            }
        }

        // assignment from heap pointer
        T* b = new T;
        T* b_ = new T;
        *b = 73;
        *b_ = 73;
        v = b;
        assert(v == b);
        assert(v != b_);
        assert(v);
        foreach (U; Types)
        {
            static if (is(T == U))
            {
                assert(v.peek!U);
                assert(*(v.peek!U) == *b);
            }
            else
            {
                assert(!v.peek!U);
            }
        }

    }
}
