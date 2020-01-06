module nxt.vary;

import core.internal.traits : hasElaborateCopyConstructor, hasElaborateDestructor;
import nxt.traits_ex : haveCommonType;

static class LightAlgebraicException : Exception
{
    this(string s) pure @nogc
    {
        super(s);
    }
}

/** Light-Weight Version of $(D std.variant.Algebraic) that, when possible,
    provides `@safe pure nothrow @nogc` and more space-efficient storage (packing).

    TODO Use

    align(1)
    struct Unaligned
    {
    align(1):
    ubyte filler;
    Victim* p;
    }

    See_Also: http://forum.dlang.org/post/osfrjcuabwscvrecuvre@forum.dlang.org
    See_Also: https://issues.dlang.org/show_bug.cgi?id=15399
 */
private struct LightAlgebraic(bool memoryPacked = false,
                              TypesParam...)
{
    @safe:

    alias Ix = ubyte; // type index type. TODO use uint or size_t when there is room (depending on `memoryPacked`)
    enum maxTypesCount = 2^^(Ix.sizeof * 8) - 1; // maximum number of allowed type parameters

    import core.internal.traits : Unqual;
    import std.meta : anySatisfy, allSatisfy, staticIndexOf, staticMap, NoDuplicates;
    import core.stdc.string : memcmp;
    import std.traits : StdCommonType = CommonType, isIntegral, hasIndirections, isCopyable, hasAliasing;
    import nxt.traits_ex : isComparable, isEquable, sizesOf, stringsOf, allSame;

public:

    enum name = LightAlgebraic.stringof;
    alias Types = NoDuplicates!TypesParam;
    alias CommonType = StdCommonType!Types;
    enum hasCommonType = !is(CommonType == void);

    enum typeSizes = sizesOf!Types;
    enum typeNames = stringsOf!Types;
    enum typeCount = Types.length;

    /// Is `true` iff `this` may have aliasing through any of `Types`.
    enum mayHaveAliasing = anySatisfy!(hasAliasing, Types);

    immutable static typeNamesRT = [typeNames]; // typeNames accessible at run-time, because `[typeNames]` is not @nogc

    /// Is $(D true) if all $(D Types) stored in this $(D LightAlgebraic) has the same length.
    enum hasFixedSize = allSame!typeSizes;

    private enum N = typeCount; // useful local shorthand

    private enum indexOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if N is <= 256

    // static checking
    static assert(N >= 2,
                  "No use storing only one type in a " ~ name);
    static assert(N < maxTypesCount,
                  "Cannot store more than " ~ maxTypesCount.stringof ~ " Types in a " ~ name);

    /** Is `true` if `U` is allowed to be assigned to `this`. */
    enum bool allowsAssignmentFrom(U) = ((N == 0 ||
                                          indexOf!(U) >= 0 ||      // either direct match or
                                          ((!hasIndirections!U) && // no indirections and
                                           indexOf!(Unqual!U) >= 0))); // ok to remove constness of value types

    enum dataMaxSize = maxSize!Types;

    auto ref to(U)() const // TODO pure @nogc
    {
        final switch (typeIndex)
        {
            import std.conv : to;
            foreach (const i, T; Types)
            {
            case i: return as!T.to!U;
            }
        }
    }

    @property void toString()(scope void delegate(scope const(char)[]) sink) const // template-lazy. TODO pure
    {
        import std.format : formattedWrite;
        if (!hasValue) { return sink("<Uninitialized LightAlgebraic>"); }
        final switch (typeIndex)
        {
            foreach (const i, T; Types)
            {
            case i:
                sink.formattedWrite!`%s`(as!T);
                return;
            }
        }
    }

    /** Returns: $(D this) as a HTML-tagged $(D string). */
    @property void toHTML()(scope void delegate(scope const(char)[]) sink) const // template-lazy. TODO pure
    {
        // wrap information in HTML tags with CSS propertie
        immutable tag = `dlang-` ~ typeName;
        sink(`<`); sink(tag); sink(`>`);
        toString(sink);
        sink(`</`); sink(tag); sink(`>`);
    }

    pure:

    /** Returns: Name (as a $(D string)) of Currently Stored Type. */
    private auto ref typeName()() const @safe nothrow @nogc // template-lazy
    {
        pragma(inline, true);
        return hasValue ? typeNamesRT[typeIndex] : null;
    }

    /** Copy construct from `that`. */
    this()(in LightAlgebraic that) @safe nothrow @nogc
    {
        _store = that._store;
        _tix = that._tix;
        pragma(msg, "Run postblits for " ~ Types.stringof);
    }

    /// Destruct.
    ~this()
    {
        pragma(inline, true);
        if (hasValue)
        {
            release();
        }
    }

    /// Construct copy from `that`.
    this(T)(T that) @trusted nothrow @nogc
        if (allowsAssignmentFrom!T)
    {
        import core.lifetime : moveEmplace;

        alias MT = Unqual!T;
        moveEmplace(*cast(MT*)&that,
                    *cast(MT*)(&_store)); // TODO ok when `that` has indirections?

        _tix = cast(Ix)(indexOf!MT + 1); // set type tag
    }

    LightAlgebraic opAssign(T)(T that) @trusted nothrow @nogc
        if (allowsAssignmentFrom!T)
    {
        import core.lifetime : moveEmplace;

        if (hasValue)
        {
            release();
        }

        alias MT = Unqual!T;
        moveEmplace(*cast(MT*)&that,
                    *cast(MT*)(&_store)); // TODO ok when `that` has indirections?

        _tix = cast(Ix)(indexOf!MT + 1); // set type tag

        return this;
    }

    /** If the $(D LightAlgebraic) object holds a value of the $(I exact) type $(D T),
        returns a pointer to that value. Otherwise, returns $(D null). In cases
        where $(D T) is statically disallowed, $(D peek) will not compile.
    */
    @property inout(T)* peek(T)() inout @trusted nothrow @nogc
    {
        pragma(inline, true);
        alias MT = Unqual!T;
        static if (!is(MT == void))
        {
            static assert(allowsAssignmentFrom!MT, "Cannot store a " ~ MT.stringof ~ " in a " ~ name);
        }
        if (!isOfType!MT) return null;
        return cast(inout MT*)&_store; // TODO alignment
    }

    /// Get Value of type $(D T).
    @property auto ref inout(T) get(T)() inout @trusted
    {
        version(LDC) pragma(inline, true); // DMD cannot inline
        if (!isOfType!T) throw new LightAlgebraicException("LightAlgebraic doesn't contain type");
        return as!T;
    }

    /// ditto
    @property inout(Types[index]) get(uint index)() inout @safe
        if (index < Types.length)
    {
        pragma(inline, true);
        return get!(Types[index]);
    }

    /// Interpret data as type $(D T).
    private @property auto ref inout(T) as(T)() inout @trusted nothrow @nogc
    {
        static if (_store.alignof >= T.alignof)
        {
            return *(cast(T*)&_store);
        }
        else
        {
            inout(T) result;
            (cast(ubyte*)&result)[0 .. T.sizeof] = _store[0 .. T.sizeof];
            return result;
        }
    }

    /// Returns: $(D true) iff $(D this) $(D LightAlgebraic) can store an instance of $(D T).
    bool isOfType(T)() const @safe nothrow @nogc // TODO shorter name such `isA`, `ofType`
    {
        pragma(inline, true);
        return _tix == indexOf!T + 1;
    }

    /// Force $(D this) to the null/uninitialized/unset/undefined state.
    void clear() @safe nothrow @nogc
    {
        pragma(inline, true);
        if (_tix != _tix.init)
        {
            release();
            _tix = _tix.init; // this is enough to indicate undefined, no need to zero `_store`
        }
    }
    /// ditto
    alias nullify = clear;      // compatible with std.typecons.Nullable

    /// Nullable type support.
    static immutable nullValue = typeof(this).init;

    /// ditto
    void opAssign(typeof(null))
    {
        pragma(inline, true);
        clear();
    }

    /// Release internal store.
    private void release() @trusted nothrow @nogc
    {
        final switch (typeIndex)
        {
            foreach (const i, T; Types)
            {
            case i:
                static if (hasElaborateDestructor!T)
                {
                    .destroy(*cast(T*)&_store); // reinterpret
                }
                return;
            }
            case Ix.max:
                return;
        }
        // TODO don't call if all types satisfy traits_ex.isValueType
        // _store[] = 0; // slightly faster than: memset(&_store, 0, _store.sizeof);
    }

    /// Returns: $(D true) if this has a defined value (is defined).
    bool hasValue() const @safe nothrow @nogc
    {
        pragma(inline, true);
        return _tix != _tix.init;
    }

    bool isNull() const @safe nothrow @nogc
    {
        pragma(inline, true);
        return _tix == _tix.init;
    }

    size_t currentSize()() const @safe nothrow @nogc // template-lazy
    {
        if (isNull) { return 0; }
        final switch (typeIndex)
        {
            foreach (const i, const typeSize; typeSizes)
            {
            case i:
                return typeSize;
            }
        }
    }

    /// Blindly Implicitly Convert Stored Value in $(D U).
    private U convertTo(U)() const @safe nothrow
    {
        assert(hasValue);
        final switch (typeIndex)
        {
            foreach (const i, T; Types)
            {
            case i:
                return as!T;
            }
        }
    }

    static if (hasCommonType)
    {
        CommonType commonValue() const @safe pure nothrow @nogc
        {
            assert(hasValue);
            final switch (typeIndex)
            {
                foreach (const i, T; Types)
                {
                case i:
                    return cast(CommonType)as!T;
                }
            }
        }
    }

    static if (allSatisfy!(isEquable, Types))
    {
        static if (hasCommonType)
        {
            bool opEquals()(in LightAlgebraic that) const @trusted nothrow @nogc // template-lazy, opEquals is nothrow @nogc
            {
                if (_tix != that._tix)
                {
                    return (this.convertTo!CommonType ==
                            that.convertTo!CommonType);
                }
                if (!this.hasValue &&
                    !that.hasValue)
                {
                    return true; // TODO same behaviour as floating point NaN?
                }
                final switch (typeIndex)
                {
                    foreach (const i, T; Types)
                    {
                    case i:
                        return this.as!T == that.as!T;
                    }
                }
            }
        }
        else
        {
            bool opEquals()(in LightAlgebraic that) const @trusted nothrow // template-lazy
            {
                if (_tix != that._tix)
                {
                    return false; // this needs to be nothrow or otherwise x in aa will throw which is not desirable
                }

                if (!this.hasValue &&
                    !that.hasValue)
                {
                    return true; // TODO same behaviour as floating point NaN?
                }

                final switch (typeIndex)
                {
                    foreach (const i, T; Types)
                    {
                    case i:
                        static if (isIntegral!T) // TODO extend by reusing some generic trait, say isBitwiseComparable
                        {
                            return memcmp(cast(void*)&this._store,
                                          cast(void*)&that._store, currentSize) == 0; // this is faster than final switch
                        }
                        else
                        {
                            return (this.as!T ==
                                    that.as!T);
                        }
                    }
                }
                assert(false); // this is for knet to compile but not in this modulue. TODO remove when compiler is fixed
            }
        }

        bool opEquals(T)(in T that) const @trusted nothrow
        {
            // TODO assert failure only if none of the Types isComparable to T
            static assert (allowsAssignmentFrom!T,
                           "Cannot equal any possible type of " ~ LightAlgebraic.stringof ~
                           " with " ~ T.stringof);

            if (!isOfType!T) return false; // throw new LightAlgebraicException("Cannot equal LightAlgebraic with current type " ~ "[Types][typeIndex]" ~ " with different types " ~ "T.stringof");

            static if (isIntegral!T) // TODO extend by reusing some generic trait, say isBitwiseComparable
            {
                return memcmp(cast(void*)&this._store,
                              cast(void*)&that, T.sizeof) == 0; // this is faster than final switch
            }
            else
            {
                return (this.as!T == that);
            }
        }
    }

    static if (allSatisfy!(isComparable, Types))
    {
        int opCmp()(in LightAlgebraic that) const @trusted // template-lazy, TODO extend to LightAlgebraic!(ThatTypes)
        {
            static if (hasCommonType) // TODO extend to haveCommonType!(Types, ThatTypes)
            {
                if (_tix != that._tix)
                {
                    // TODO functionize to defaultOpCmp to avoid postblits:
                    const a = this.convertTo!CommonType;
                    const b = that.convertTo!CommonType;
                    return a < b ? -1 : a > b ? 1 : 0;
                }
            }
            else
            {
                if (_tix != that._tix)
                {
                    throw new LightAlgebraicException("Cannot compare LightAlgebraic of type " ~ typeNamesRT[typeIndex] ~
                                             " with LightAlgebraic of type " ~ typeNamesRT[that.typeIndex]);
                }
            }

            final switch (typeIndex)
            {
                foreach (const i, T; Types)
                {
                case i:
                    // TODO functionize to defaultOpCmp to avoid postblits:
                    const a = this.as!T;
                    const b = that.as!T;
                    return a < b ? -1 : a > b ? 1 : 0;
                }
            }
        }

        int opCmp(U)(in U that) const @trusted
        {
            static if (haveCommonType!(Types, U)) // TODO is CommonType or isComparable the correct way of checking this?
            {
                final switch (typeIndex)
                {
                    foreach (const i, T; Types)
                    {
                    case i:
                        const a = this.as!T;
                        return a < that ? -1 : a > that ? 1 : 0; // TODO functionize to defaultOpCmp
                    }
                }
            }
            else
            {
                static assert(allowsAssignmentFrom!U, // TODO relax to allowsComparisonWith!U
                              "Cannot compare " ~ LightAlgebraic.stringof ~ " with " ~ U.stringof);
                if (!isOfType!U)
                {
                    throw new LightAlgebraicException("Cannot compare " ~ LightAlgebraic.stringof ~ " with " ~ U.stringof);
                }
                // TODO functionize to defaultOpCmp to avoid postblits:
                const a = this.as!U;
                return a < that ? -1 : a > that ? 1 : 0;
            }
        }
    }

    extern (D) hash_t toHash() const @trusted pure nothrow
    {
        import core.internal.hash : hashOf;
        const typeof(return) hash = _tix.hashOf;
        if (hasValue)
        {
            final switch (typeIndex)
            {
                foreach (const i, T; Types)
                {
                case i: return as!T.hashOf(hash);
                }
            }
        }
        return hash;
    }

    import std.digest : isDigest;

    /// TODO use `isHashable`?
    void toDigest(Digest)(scope ref Digest digest) const nothrow @nogc
        if (isDigest!Digest)
    {
        import nxt.digestion : digestAny;
        digestAny(digest, _tix);
        if (hasValue)
        {
            final switch (typeIndex)
            {
                foreach (const i, T; Types)
                {
                case i:
                    digestAny(digest, as!T);
                    return;
                }
            }
        }
    }

private:
    static if (memoryPacked)
    {
        // immutable to make hasAliasing!(LightAlgebraic!(...)) false
        static if (mayHaveAliasing)
        {
            ubyte[dataMaxSize] _store;
        }
        else
        {
            // to please hasAliasing!(typeof(this)):
            immutable(ubyte)[dataMaxSize] _store;
        }
    }
    else
    {
        // immutable to make hasAliasing!(LightAlgebraic!(...)) false
        union
        {
            static if (mayHaveAliasing)
            {
                ubyte[dataMaxSize] _store;
                void* alignDummy; // non-packed means good alignment. TODO check for maximum alignof of Types
            }
            else
            {
                // to please hasAliasing!(typeof(this)):
                immutable(ubyte)[dataMaxSize] _store;
                immutable(void)* alignDummy; // non-packed means good alignment. TODO check for maximum alignof of Types
            }
        }
    }

    size_t typeIndex() const nothrow @nogc
    {
        pragma(inline, true);
        assert(_tix != 0, "Cannot get index from uninitialized (null) variant.");
        return _tix - 1;
    }

    Ix _tix = 0;                // type index
}

alias FastAlgebraic(Types...) = LightAlgebraic!(false, Types);

alias PackedAlgebraic(Types...) = LightAlgebraic!(true, Types);

/// Copied from std.variant.
private static template maxSize(T...)
{
    static if (T.length == 1)
    {
        enum size_t maxSize = T[0].sizeof;
    }
    else
    {
        import std.algorithm.comparison : max;
        enum size_t maxSize = max(T[0].sizeof, maxSize!(T[1 .. $]));
    }
}


@safe:

unittest
{
    // FastAlgebraic!(float, double, bool) a;
    // a = 2.1;  assert(a.to!string == "2.1");  assert(a.toHTML == "<dlang-double>2.1</dlang-double>");
    // a = 2.1f; assert(a.to!string == "2.1");  assert(a.toHTML == "<dlang-float>2.1</dlang-float>");
    // a = true; assert(a.to!string == "true"); assert(a.toHTML == "<dlang-bool>true</dlang-bool>");
}

pure:

nothrow @nogc unittest
{
    static assert(FastAlgebraic!(float, float, double).typeCount == 2);
}

nothrow @nogc unittest
{
    alias C = FastAlgebraic!(float, double);
    C a = 1.0;
    const C b = 2.0;
    const C c = 2.0f;
    const C d = 1.0f;

    assert(a.commonValue == 1);
    assert(b.commonValue == 2);
    assert(c.commonValue == 2);
    assert(d.commonValue == 1);

    // nothrow comparison possible
    assert(a < b);
    assert(a < c);
    assert(a == d);

    static assert(!a.hasFixedSize);
    static assert(a.allowsAssignmentFrom!float);
    static assert(a.allowsAssignmentFrom!double);
    static assert(!a.allowsAssignmentFrom!string);

    a.clear();
    assert(!a.hasValue);
    assert(a.peek!float is null);
    assert(a.peek!double is null);
    assert(a.currentSize == 0);
}

/// aliasing traits
nothrow @nogc unittest
{
    import std.traits : hasAliasing;
    static assert(!hasAliasing!(FastAlgebraic!(long, double)));
    static assert(!hasAliasing!(FastAlgebraic!(long, string)));
    static assert(!hasAliasing!(FastAlgebraic!(long, immutable(double)*)));
    static assert(hasAliasing!(FastAlgebraic!(long, double*)));
}

nothrow @nogc unittest
{
    alias V = FastAlgebraic!(long, double);
    const a = V(1.0);

    static assert(a.hasFixedSize);

    assert(a.isOfType!double);
    assert(a.peek!long is null);
    assert(a.peek!double !is null);

    static assert(is(typeof(a.peek!long) == const(long)*));
    static assert(is(typeof(a.peek!double) == const(double)*));
}

/// equality and comparison
unittest
{
    FastAlgebraic!(float, double, string) a, b;

    static assert(!a.hasFixedSize);

    a = 1.0f;
    b = 1.0f;
    assert(a == b);

    a = 1.0f;
    b = 2.0f;
    assert(a != b);
    assert(a < b);
    assert(b > a);

    a = "alpha";
    b = "alpha";
    assert(a == b);

    a = "a";
    b = "b";
    assert(a != b);
    assert(a < b);
    assert(b > a);
}

/// AA keys
nothrow unittest
{
    alias C = FastAlgebraic!(float, double);
    static assert(!C.hasFixedSize);
    string[C] a;
    a[C(1.0f)] = "1.0f";
    a[C(2.0)] = "2.0";
    assert(a[C(1.0f)] == "1.0f");
    assert(a[C(2.0)] == "2.0");
}

/// verify nothrow comparisons
nothrow @nogc unittest
{
    alias C = FastAlgebraic!(int, float, double);
    static assert(!C.hasFixedSize);
    assert(C(1.0) < 2);
    assert(C(1.0) < 2.0);
    assert(C(1.0) < 2.0);
    static assert(!__traits(compiles, { C(1.0) < 'a'; })); // cannot compare with char
    static assert(!__traits(compiles, { C(1.0) < "a"; })); // cannot compare with string
}

/// TODO
nothrow @nogc unittest
{
    // alias C = FastAlgebraic!(int, float, double);
    // alias D = FastAlgebraic!(float, double);
    // assert(C(1) < D(2.0));
    // assert(C(1) < D(1.0));
    // static assert(!__traits(compiles, { C(1.0) < "a"; })); // cannot compare with string
}

/// if types have CommonType comparison is nothrow @nogc
nothrow @nogc unittest
{
    alias C = FastAlgebraic!(short, int, long, float, double);
    static assert(!C.hasFixedSize);
    assert(C(1) != C(2.0));
    assert(C(1) == C(1.0));
}

unittest
{
    import std.exception : assertThrown;

    static assert(hasElaborateCopyConstructor!(char[2]) == false);
    static assert(hasElaborateCopyConstructor!(char[]) == false);

    // static assert(FastAlgebraic!(char, wchar).sizeof == 2 + 1);
    // static assert(FastAlgebraic!(wchar, dchar).sizeof == 4 + 1);
    // static assert(FastAlgebraic!(long, double).sizeof == 8 + 1);
    // static assert(FastAlgebraic!(int, float).sizeof == 4 + 1);
    // static assert(FastAlgebraic!(char[2], wchar[2]).sizeof == 2 * 2 + 1);

    import std.datetime : Date, TimeOfDay;
    alias C = FastAlgebraic!(string,
                        // fixed length strings: small string optimizations (SSOs)
                        int, float,
                        long, double, Date, TimeOfDay);
    static assert(!C.hasFixedSize);

    static assert(C.allowsAssignmentFrom!int);
    static assert(!C.allowsAssignmentFrom!(int[2]));
    static assert(C.allowsAssignmentFrom!(const(int)));

    static assert(C.dataMaxSize == string.sizeof);
    static assert(!__traits(compiles, { assert(d == 'a'); }));

    assert(C() == C());         // two undefined are equal

    C d;
    C e = d;                    // copy construction
    assert(e == d);             // two undefined should not equal

    d = 11;
    assert(d != e);

    // TODO Allow this d = cast(ubyte)255;

    d = 1.0f;
    assertThrown!LightAlgebraicException(d.get!double);
    assert(d.hasValue);
    assert(d.isOfType!float);
    assert(d.peek!float !is null);
    assert(!d.isOfType!double);
    assert(d.peek!double is null);
    assert(d.get!float == 1.0f);
    assert(d == 1.0f);
    assert(d != 2.0f);
    assert(d < 2.0f);
    assert(d != "2.0f");
    assertThrown!LightAlgebraicException(d < 2.0);
    assertThrown!LightAlgebraicException(d < "2.0");
    assert(d.currentSize == float.sizeof);

    d = 2;
    assert(d.hasValue);
    assert(d.peek!int !is null);
    assert(!d.isOfType!float);
    assert(d.peek!float is null);
    assert(d.get!int == 2);
    assert(d == 2);
    assert(d != 3);
    assert(d < 3);
    assertThrown!LightAlgebraicException(d < 2.0f);
    assertThrown!LightAlgebraicException(d < "2.0");
    assert(d.currentSize == int.sizeof);

    d = "abc";
    assert(d.hasValue);
    assert(d.get!0 == "abc");
    assert(d.get!string == "abc");
    assert(d.isOfType!string);
    assert(d.peek!string !is null);
    assert(d == "abc");
    assert(d != "abcd");
    assert(d < "abcd");
    assertThrown!LightAlgebraicException(d < 2.0f);
    assertThrown!LightAlgebraicException(d < 2.0);
    assert(d.currentSize == string.sizeof);

    d = 2.0;
    assert(d.hasValue);
    assert(d.get!double == 2.0);
    assert(d.isOfType!double);
    assert(d.peek!double !is null);
    assert(d == 2.0);
    assert(d != 3.0);
    assert(d < 3.0);
    assertThrown!LightAlgebraicException(d < 2.0f);
    assertThrown!LightAlgebraicException(d < "2.0");
    assert(d.currentSize == double.sizeof);

    d.clear();
    assert(d.peek!int is null);
    assert(d.peek!float is null);
    assert(d.peek!double is null);
    assert(d.peek!string is null);
    assert(!d.hasValue);
    assert(d.currentSize == 0);

    assert(C(1.0f) == C(1.0f));
    assert(C(1.0f) <  C(2.0f));
    assert(C(2.0f) >  C(1.0f));

    assertThrown!LightAlgebraicException(C(1.0f) <  C(1.0));
    // assertThrown!LightAlgebraicException(C(1.0f) == C(1.0));
}

///
pure unittest
{
    String15 s;
    String15 t = s;
    assert(t == s);

    alias V = FastAlgebraic!(String15, string);
    V v = String15("first");
    assert(v.peek!String15);
    assert(!v.peek!string);

    v = String15("second");
    assert(v.peek!String15);
    assert(!v.peek!string);

    v = "third";
    assert(!v.peek!String15);
    assert(v.peek!string);

    auto w = v;
    assert(v == w);
    w.clear();
    assert(!v.isNull);
    assert(w.isNull);
    w = v;
    assert(!w.isNull);

    v = V.init;
    assert(v == V.init);
}

/// check default values
@safe pure unittest
{
    alias V = FastAlgebraic!(String15, string);
    V _;
    assert(_._tix == V.Ix.init);
    assert(V.init._tix == V.Ix.init);

    // TODO import nxt.bit_traits : isInitAllZeroBits;
    // TODO static assert(isInitAllZeroBits!(V));
}

version(unittest)
{
    import nxt.fixed_array : StringN;
    alias String15 = StringN!(15);
}
