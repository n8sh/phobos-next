module vary;

import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor;;
import traits_ex : haveCommonType;

static class VaryNException : Exception
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

    See also: http://forum.dlang.org/post/osfrjcuabwscvrecuvre@forum.dlang.org
    See also: https://issues.dlang.org/show_bug.cgi?id=15399
 */
private struct VaryN(bool memoryPacked = false, TypesParam...)
{
    alias Ix = ubyte; // type index type
    enum maxTypesCount = 2^^(Ix.sizeof * 8) - 1; // maximum number of allowed type parameters

    import std.typecons : Unqual;
    import std.meta : allSatisfy, staticIndexOf, staticMap, NoDuplicates;
    import core.stdc.string : memcpy, memset, memcmp;
    import std.traits : StdCommonType = CommonType, isIntegral, hasIndirections, isCopyable;
    import traits_ex : isComparable, isEquable, sizesOf, stringsOf, allSame;

public:

    enum name = VaryN.stringof;
    alias Types = NoDuplicates!TypesParam;
    alias CommonType = StdCommonType!Types;
    enum hasCommonType = !is(CommonType == void);

    enum typeSizes = sizesOf!Types;
    enum typeNames = stringsOf!Types;
    enum typeCount = Types.length;

    immutable static typeNamesRT = [typeNames]; // typeNames accessible at run-time, because `[typeNames]` is not @nogc

    /// Is $(D true) if all $(D Types) stored in this $(D VaryN) has the same length.
    enum hasFixedSize = allSame!typeSizes;

    private enum N = typeCount; // useful local shorthand

    private enum indexOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if N is <= 256

    // static checking
    static assert(N >= 2,
                  "No use storing only one type in a " ~ name);
    static assert(N < maxTypesCount,
                  "Cannot store more than " ~ maxTypesCount.stringof ~ " Types in a " ~ name);

    /** Is `true` if `U` is allowed to be assigned to `this`. */
    private enum bool allowsAssignmentFrom(U) = ((N == 0 ||
                                                  indexOf!(U) >= 0 ||      // either direct match or
                                                  ((!hasIndirections!U) && // no indirections and
                                                   indexOf!(Unqual!U) >= 0))); // ok to remove constness of value types

    import std.variant : maxSize, VariantException;
    enum dataMaxSize = maxSize!Types;

    auto ref to(U)() const // TODO pure @nogc
    {
        final switch (_tix)
        {
            import std.conv : to;
            foreach (const i, T; Types)
            {
            case i: return as!T.to!U;
            }
        }
    }

    @property void toString()(scope void delegate(const(char)[]) sink) const // template-lazy. TODO pure
    {
        import std.format : formattedWrite;
        if (!hasValue) { return sink("<Uninitialized VaryN>"); }
        final switch (_tix)
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
    @property void toHTML()(scope void delegate(const(char)[]) sink) const // template-lazy. TODO pure
    {
        // wrap information in HTML tags with CSS propertie
        immutable tag = `dlang-` ~ typeName;
        sink(`<`); sink(tag); sink(`>`);
        toString(sink);
        sink(`</`); sink(tag); sink(`>`);
    }

    pure:

    /** Returns: Name (as a $(D string)) of Currently Stored Type. */
    auto ref typeName() const @safe nothrow @nogc
    {
        return hasValue ? typeNamesRT[_tix] : null;
    }

    /** Copy construct from `that`. */
    this(in VaryN that) @safe nothrow @nogc
    {
        _store = that._store;
        _tix = that._tix;
        // TODO run postblits
    }

    ~this()
    {
        if (hasValue) { release(); }
    }

    this(T)(T that) @trusted nothrow @nogc
        if (allowsAssignmentFrom!T)
    {
        import std.algorithm.mutation : moveEmplace;

        alias MT = Unqual!T;
        moveEmplace(*cast(MT*)&that,
                    *cast(MT*)(&_store)); // TODO ok when `that` has indirections?

        _tix = cast(Ix)indexOf!MT; // set type tag
    }

    VaryN opAssign(T)(T that) @trusted nothrow @nogc
        if (allowsAssignmentFrom!T)
    {
        import std.algorithm.mutation : moveEmplace;

        if (hasValue) { release(); }

        alias MT = Unqual!T;
        moveEmplace(*cast(MT*)&that,
                    *cast(MT*)(&_store)); // TODO ok when `that` has indirections?

        _tix = cast(Ix)indexOf!MT; // set type tag

        return this;
    }

    /** If the $(D VaryN) object holds a value of the $(I exact) type $(D T),
        returns a pointer to that value. Otherwise, returns $(D null). In cases
        where $(D T) is statically disallowed, $(D peek) will not compile.
    */
    @property inout(T)* peek(T)() inout @trusted nothrow @nogc
    {
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
        if (!isOfType!T) throw new VaryNException("VaryN doesn't contain type");
        return as!T;
    }

    /// ditto
    @property inout(Types[index]) get(uint index)() inout @safe
        if (index < Types.length)
    {
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
            T result;
            (cast(ubyte*)&result)[0 .. T.sizeof] = _store[0 .. T.sizeof];
            return result;
        }
    }

    /// Returns: $(D true) iff $(D this) $(D VaryN) can store an instance of $(D T).
    bool isOfType(T)() const @safe nothrow @nogc // TODO shorter name such `isA`, `ofType`
    {
        return _tix == indexOf!T;
    }

    /// Force $(D this) to the null/uninitialized/unset/undefined state.
    void clear() @safe nothrow @nogc
    {
        if (hasValue) { release(); }
        _tix = Ix.max; // this is enough to indicate undefined, no need to zero `_store`
    }
    /// ditto
    alias nullify = clear; // compatible with std.typecons.Nullable
    /// ditto
    alias makeUndefined = clear;
    /// ditto
    pragma(inline) void opAssign(typeof(null)) { clear(); }

    /// Release internal store.
    private void release() @trusted nothrow @nogc
    {
        final switch (_tix)
        {
            foreach (const i, T; Types)
            {
            case i:
                static if (hasElaborateDestructor!T)
                {
                    .destroy(*cast(T*)&_store); // reinterpret
                }
            }
        }

        // TODO don't call if all types satisfy traits_ex.isValueType
        // _store[] = 0; // slightly faster than: memset(&_store, 0, _store.sizeof);
    }

    /// Returns: $(D true) if this has a defined value (is defined).
    bool hasValue() const @safe nothrow @nogc { return _tix != Ix.max; }

    size_t currentSize() const @safe nothrow @nogc
    {
        if (hasValue)
        {
            final switch (_tix)
            {
                foreach (const i, const typeSize; typeSizes)
                {
                case i:
                    return typeSize;
                }
            }
        }
        else
        {
            return 0;
        }
    }

    /// Blindly Implicitly Convert Stored Value in $(D U).
    private U convertTo(U)() const @safe nothrow
    {
        assert(hasValue);
        final switch (_tix)
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
            final switch (_tix)
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
            bool opEquals(in VaryN that) const @trusted nothrow @nogc // opEquals is nothrow @nogc
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

                final switch (_tix)
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
            bool opEquals(in VaryN that) const @trusted nothrow
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

                final switch (_tix)
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
                           "Cannot equal any possible type of " ~ VaryN.stringof ~
                           " with " ~ T.stringof);

            if (!isOfType!T) return false; // throw new VaryNException("Cannot equal VaryN with current type " ~ "[Types][_tix]" ~ " with different types " ~ "T.stringof");

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
        int opCmp(in VaryN that) const @trusted // TODO extend to VaryN!(ThatTypes)
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
                    throw new VaryNException("Cannot compare VaryN of type " ~ typeNamesRT[_tix] ~
                                             " with VaryN of type " ~ typeNamesRT[that._tix]);
                }
            }

            final switch (_tix)
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
                final switch (_tix)
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
                              "Cannot compare " ~ VaryN.stringof ~ " with " ~ U.stringof);
                if (!isOfType!U)
                {
                    throw new VaryNException("Cannot compare " ~ VaryN.stringof ~ " with " ~ U.stringof);
                }
                // TODO functionize to defaultOpCmp to avoid postblits:
                const a = this.as!U;
                return a < that ? -1 : a > that ? 1 : 0;
            }
        }
    }

    extern (D) size_t toHash() const @trusted pure nothrow
    {
        import core.internal.hash : hashOf;
        const size_t hash = _tix.hashOf;
        if (hasValue)
        {
            final switch (_tix)
            {
                foreach (const i, T; Types)
                {
                case i: return as!T.hashOf(hash);
                }
            }
        }
        return hash;
    }

    import std.digest.digest : isDigest;

    void toDigest(Digest)(scope ref Digest digest) const nothrow @nogc
        if (isDigest!Digest)
    {
        import hash_ex : digestAny;
        digestAny(digest, _tix);
        if (hasValue)
        {
            final switch (_tix)
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
        ubyte[dataMaxSize] _store;
    }
    else
    {
        union
        {
            ubyte[dataMaxSize] _store;
            void* alignDummy; // non-packed means good alignment. TODO check for maximum alignof of Types
        }
    }
    Ix _tix = Ix.max; // Type Index if != Ix.max
}

alias FastVariant(Types...) = VaryN!(false, Types);
alias PackedVariant(Types...) = VaryN!(true, Types);

@safe:

unittest
{
    // FastVariant!(float, double, bool) a;
    // a = 2.1;  assert(a.to!string == "2.1");  assert(a.toHTML == "<dlang-double>2.1</dlang-double>");
    // a = 2.1f; assert(a.to!string == "2.1");  assert(a.toHTML == "<dlang-float>2.1</dlang-float>");
    // a = true; assert(a.to!string == "true"); assert(a.toHTML == "<dlang-bool>true</dlang-bool>");
}

pure:

nothrow @nogc unittest
{
    static assert(FastVariant!(float, float, double).typeCount == 2);
}

nothrow @nogc unittest
{
    alias C = FastVariant!(float, double);
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

nothrow @nogc unittest
{
    const a = FastVariant!(long, double)(1.0);

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
    FastVariant!(float, double, string) a, b;

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
    alias C = FastVariant!(float, double);
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
    alias C = FastVariant!(int, float, double);
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
    // alias C = FastVariant!(int, float, double);
    // alias D = FastVariant!(float, double);
    // assert(C(1) < D(2.0));
    // assert(C(1) < D(1.0));
    // static assert(!__traits(compiles, { C(1.0) < "a"; })); // cannot compare with string
}

/// if types have CommonType comparison is nothrow @nogc
nothrow @nogc unittest
{
    alias C = FastVariant!(short, int, long, float, double);
    static assert(!C.hasFixedSize);
    assert(C(1) != C(2.0));
    assert(C(1) == C(1.0));
}

unittest
{
    import std.exception : assertThrown;

    static assert(hasElaborateCopyConstructor!(char[2]) == false);
    static assert(hasElaborateCopyConstructor!(char[]) == false);

    // static assert(FastVariant!(char, wchar).sizeof == 2 + 1);
    // static assert(FastVariant!(wchar, dchar).sizeof == 4 + 1);
    // static assert(FastVariant!(long, double).sizeof == 8 + 1);
    // static assert(FastVariant!(int, float).sizeof == 4 + 1);
    // static assert(FastVariant!(char[2], wchar[2]).sizeof == 2 * 2 + 1);

    import std.datetime : Date, TimeOfDay;
    alias C = FastVariant!(string,
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
    assertThrown!VaryNException(d.get!double);
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
    assertThrown!VaryNException(d < 2.0);
    assertThrown!VaryNException(d < "2.0");
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
    assertThrown!VaryNException(d < 2.0f);
    assertThrown!VaryNException(d < "2.0");
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
    assertThrown!VaryNException(d < 2.0f);
    assertThrown!VaryNException(d < 2.0);
    assert(d.currentSize == string.sizeof);

    d = 2.0;
    assert(d.hasValue);
    assert(d.get!double == 2.0);
    assert(d.isOfType!double);
    assert(d.peek!double !is null);
    assert(d == 2.0);
    assert(d != 3.0);
    assert(d < 3.0);
    assertThrown!VaryNException(d < 2.0f);
    assertThrown!VaryNException(d < "2.0");
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

    assertThrown!VaryNException(C(1.0f) <  C(1.0));
    // assertThrown!VaryNException(C(1.0f) == C(1.0));
}

///
pure unittest
{
    import arrayn : StringN, Checking;
    alias String15 = StringN!(15, Checking.viaScope);

    String15 s;
    String15 t = s;
    assert(t == s);

    FastVariant!(String15, string) v = String15("first");
    assert(v.peek!String15);
    assert(!v.peek!string);

    v = String15("second");
    assert(v.peek!String15);
    assert(!v.peek!string);

    v = "third";
    assert(!v.peek!String15);
    assert(v.peek!string);
}
