module vary;

import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor;;
import traits_ex : haveCommonType;

enum isAllowedVaryType(T) = (!hasElaborateCopyConstructor!T &&
                             !hasElaborateDestructor!T);

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
private struct VaryN(bool memoryPacked = false,
                     TypesParam...)
{
    alias Ix = ubyte; // type index type
    enum maxParamCount = 2^^(Ix.sizeof * 8) - 1; // maximum number of allowed type parameters

    import std.typecons : Unqual;
    import std.meta : allSatisfy, staticIndexOf, staticMap, NoDuplicates;
    import core.stdc.string : memcpy, memset, memcmp;
    import std.traits : isIntegral, hasIndirections;
    import traits_ex : isComparable, isEquable, sizesOf, stringsOf, allSame;

public:

    enum name = VaryN.stringof;
    alias Types = NoDuplicates!TypesParam;
    alias commonType = CommonType!Types;

    enum typeSizes = sizesOf!Types;
    enum typeNames = stringsOf!Types;
    enum typeCount = Types.length;

    immutable static typeNamesRT = [typeNames]; // typeNames accessible at run-time, because `[typeNames]` is not @nogc

    /// Is $(D true) if all $(D Types) stored in this $(D VaryN) has the same length.
    enum hasFixedSize = allSame!typeSizes;

    private enum N = typeCount; // useful local shorthand

    enum indexOf(T) = staticIndexOf!(T, Types); // TODO cast to ubyte if N is <= 256

    // static checking
    static assert(N >= 2,
                  "No use storing only one type in a " ~ name);
    static assert(N < maxParamCount,
                  "Cannot store more than " ~ maxParamCount.stringof ~ " Types in a " ~ name);
    static assert(allSatisfy!(isAllowedVaryType, Types),
                  "Cannot store some of the types " ~ Types.stringof ~ " in a " ~ name);

    /** `true` if `U` is allowed to be assigned to `this` */
    enum bool allowsAssignmentFrom(U) = ((N == 0 ||
                                          indexOf!(U) >= 0 ||      // either direct match or
                                          ((!hasIndirections!U) && // no indirections and
                                           indexOf!(Unqual!U) >= 0))); // ok to remove constness of value types

    // Use same as staticIndexOf
    template staticAssignableTypeIndexOf(U)
    {
        static auto f(U)()
        {
            foreach (i, T; Types)
            {
                import std.traits : isAssignable;
                static if (isAssignable!(T, U))
                {
                    return i;
                }
            }
            return 0;
        }
        enum canStore = f!U;
    }

    import std.variant : maxSize, VariantException;
    enum dataMaxSize = maxSize!Types;

    auto ref to(U)() const @safe // TODO pure @nogc
    {
        final switch (_tix)
        {
            import std.conv : to;
            foreach (const i, T; Types)
            {
            case i: return this.interpretAs!T.to!U;
            }
        }
    }

    @property string toString() const @trusted // TODO pure
    {
        if (!hasValue) { return "<Uninitialized VaryN>"; }
        import std.conv : to;
        final switch (_tix)
        {
            foreach (const i, T; Types)
            {
            case i: return this.interpretAs!T.to!(typeof(return));
            }
        }
    }

    /** Returns: $(D this) as a HTML-tagged $(D string). */
    @property string toHTML() const @trusted // TODO pure
    {
        immutable tag = `dlang-` ~ typeName;
        return `<` ~ tag ~ `>` ~ toString ~ `</` ~ tag ~ `>`; // wrap information in HTML tags with CSS propertie
    }

    pure:

    /** Returns: Name (as a $(D string)) of Currently Stored Type. */
    auto ref typeName() const @safe nothrow @nogc
    {
        return hasValue ? typeNamesRT[_tix] : null;
    }

    /** Copy Construct. */
    this(VaryN that) @safe @nogc nothrow
    {
        _data = that._data;
        _tix = that._tix;
        // TODO run postblits
    }

    this(T)(T value) @safe @nogc nothrow
        if (allowsAssignmentFrom!T)
    {
        // static assert(allowsAssignmentFrom!T, "Cannot store a " ~ T.stringof ~ " in a " ~ name ~ ", valid types are " ~ Types.stringof);
        init(value);
    }

    VaryN opAssign(T)(T that) @trusted @nogc nothrow
        if (allowsAssignmentFrom!T)
    {
        // static assert(allowsAssignmentFrom!T, "Cannot store a " ~ T.stringof ~ " in a " ~ name ~ ", valid types are " ~ Types.stringof);
        if (hasValue) clearDataIndirections;
        init(that);
        return this;
    }

    /** Initialize without checking for clearing.
     */
    private void init(T)(T that) @trusted @nogc nothrow
    {
        alias U = Unqual!T;
        static if (_data.alignof >= T.alignof)
        {
            *(cast(U*)&_data) = that;
        }
        else
        {
            (cast(ubyte*)&_data)[0 .. T.sizeof] = (cast(ubyte*)&that)[0 .. T.sizeof];
        }
        _tix = cast(Ix)indexOf!U; // set type tag
    }

    /** If the $(D VaryN) object holds a value of the $(I exact) type $(D T),
        returns a pointer to that value. Otherwise, returns $(D null). In cases
        where $(D T) is statically disallowed, $(D peek) will not compile.
    */
    @property inout(T)* peek(T)() inout @trusted @nogc nothrow
    {
        alias U = Unqual!T;
        static if (!is(U == void))
            static assert(allowsAssignmentFrom!U, "Cannot store a " ~ U.stringof ~ " in a " ~ name);
        if (!isOfType!U) return null;
        return cast(inout U*)&_data; // TODO alignment
    }

    /// Get Value of type $(D T).
    @property auto ref inout(T) get(T)() inout @trusted
    {
        if (!isOfType!T) throw new VaryNException("VaryN doesn't contain type");
        return interpretAs!T;
    }

    /// ditto
    @property inout(Types[index]) get(uint index)() inout @safe
        if (index < Types.length)
    {
        return get!(Types[index]);
    }

    /// Interpret data as type $(D T).
    private @property auto ref inout(T) interpretAs(T)() inout @trusted @nogc nothrow
    {
        static if (_data.alignof >= T.alignof)
        {
            return *(cast(T*)&_data);
        }
        else
        {
            T result;
            (cast(ubyte*)&result)[0 .. T.sizeof] = _data[0 .. T.sizeof];
            return result;
        }
    }

    /// Returns: $(D true) iff $(D this) $(D VaryN) can store an instance of $(D T).
    bool isOfType(T)() const @safe nothrow @nogc
    {
        return _tix == indexOf!T;
    }

    /// Force $(D this) to the null (undefined) state.
    void clear() @safe @nogc nothrow
    {
        clearDataIndirections;
        _tix = Ix.max;
    }
    alias nullify = clear; // compatible with std.typecons.Nullable

    /// Clear indirections stored in $(D _data).
    private void clearDataIndirections() @safe @nogc nothrow
    {
        // TODO don't call if all types satisfy traits_ex.isValueType
        _data[] = 0; // slightly faster than: memset(&_data, 0, _data.sizeof);
    }

    /// Returns: $(D true) if this has a defined value (is defined).
    bool hasValue() const @safe @nogc nothrow { return _tix != Ix.max; }
    alias defined = hasValue;
    alias isDefined = hasValue;

    size_t currentSize() const @safe @nogc nothrow
    {
        if (hasValue)
        {
            // TODO move calculation to cstor using staticArrayOf
            size_t[N] szs;
            foreach (const i, const typeSize; typeSizes)
            {
                szs[i] = typeSize;
            }
            return szs[_tix];
        }
        else
            return 0;
    }

    import std.traits : CommonType;

    /// Blindly Implicitly Convert Stored Value in $(D U).
    private U convertTo(U)() const @safe nothrow
    {
        final switch (_tix)
        {
            foreach (const i, T; Types)
            {
            case i:
                return this.interpretAs!T;
            }
        }
    }

    static if (allSatisfy!(isEquable, Types))
    {
        static if (!is(commonType == void)) // if Types have a CommonType
        {
            bool opEquals(in VaryN that) const @trusted nothrow @nogc // opEquals is nothrow @nogc
            {
                return (this.convertTo!commonType ==
                        that.convertTo!commonType);
            }
        }
        else
        {
            bool opEquals(in VaryN that) const @trusted nothrow
            {
                if (_tix != that._tix)
                    return false; // this needs to be nothrow or otherwise x in aa will throw which is not desirable

                if (!this.hasValue &&
                    !that.hasValue)
                    return false; // TODO same behaviour as floating point NaN?

                final switch (_tix)
                {
                    foreach (const i, T; Types)
                    {
                    case i:
                        static if (isIntegral!T) // TODO extend by reusing some generic trait, say isBitwiseComparable
                        {
                            return memcmp(cast(void*)&this._data,
                                          cast(void*)&that._data, currentSize) == 0; // this is faster than final switch
                        }
                        else
                        {
                            return (this.interpretAs!T ==
                                    that.interpretAs!T);
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
                return memcmp(cast(void*)&this._data,
                              cast(void*)&that, T.sizeof) == 0; // this is faster than final switch
            }
            else
            {
                return (this.interpretAs!T == that);
            }
        }
    }

    static if (allSatisfy!(isComparable, Types))
    {
        int opCmp(in VaryN that) const @trusted // TODO extend to VaryN!(ThatTypes)
        {
            static if (haveCommonType!Types) // TODO extend to haveCommonType!(Types, ThatTypes)
            {
                pragma(msg, "TODO nothrow opCmp is possible for " ~ VaryN.stringof);
            }
            if (_tix != that._tix)
            {
                throw new VaryNException("Cannot compare VaryN of type " ~ typeNamesRT[_tix] ~
                                         " with VaryN of type " ~ typeNamesRT[that._tix]);
            }
            final switch (_tix)
            {
                foreach (const i, T; Types)
                {
                case i:
                    const a = this.interpretAs!T;
                    const b = that.interpretAs!T;
                    return a < b ? -1 : a > b ? 1 : 0; // TODO functionize to defaultOpCmp
                }
            }
        }

        int opCmp(U)(in U that) const @trusted
        {
            static assert(allowsAssignmentFrom!U, // TODO relax to allowsComparisonWith!U
                          "Cannot compare " ~ VaryN.stringof ~ " with " ~ U.stringof);

            static if (haveCommonType!Types)
            {
                pragma(msg, "TODO nothrow opCmp is possible for " ~ VaryN.stringof);
            }
            if (!isOfType!U)
            {
                throw new VaryNException("Cannot compare " ~ VaryN.stringof ~ " with " ~ U.stringof);
            }

            const a = this.interpretAs!U;
            const b = that;
            return a < b ? -1 : a > b ? 1 : 0; // TODO functionize to defaultOpCmp
        }
    }

    extern (D) size_t toHash() const @trusted pure nothrow
    {
        import core.internal.hash : hashOf;
        size_t hash = _tix.hashOf;
        final switch (_tix)
        {
            foreach (const i, T; Types)
            {
            case i: return this.interpretAs!T.hashOf(hash);
            }
        }
    }

private:
    static if (memoryPacked)
    {
        ubyte[dataMaxSize] _data;
    }
    else
    {
        union
        {
            ubyte[dataMaxSize] _data;
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
    FastVariant!(float, double, bool) a;
    a = 2.1;  assert(a.to!string == "2.1");  assert(a.toHTML == "<dlang-double>2.1</dlang-double>");
    a = 2.1f; assert(a.to!string == "2.1");  assert(a.toHTML == "<dlang-float>2.1</dlang-float>");
    a = true; assert(a.to!string == "true"); assert(a.toHTML == "<dlang-bool>true</dlang-bool>");
}

pure:

@nogc nothrow unittest
{
    static assert(FastVariant!(float, float, double).typeCount == 2);
}

@nogc nothrow unittest
{
    FastVariant!(float, double) a = 1.0;

    static assert(!a.hasFixedSize);
    static assert(a.allowsAssignmentFrom!float);
    static assert(a.allowsAssignmentFrom!double);
    static assert(!a.allowsAssignmentFrom!string);

    a.clear;
    assert(!a.hasValue);
    assert(a.peek!float is null);
    assert(a.peek!double is null);
    assert(a.currentSize == 0);
}

@nogc nothrow unittest
{
    const a = FastVariant!(long, double)(1.0);

    static assert(a.hasFixedSize);

    assert(a.peek!long is null);
    assert(a.peek!double !is null);

    static assert(is(typeof(a.peek!long) == const(long)*));
    static assert(is(typeof(a.peek!double) == const(double)*));
}

/// equality and comparison
unittest
{
    FastVariant!(float, double, string) a, b;

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
    string[C] a;
    a[C(1.0f)] = "1.0f";
    a[C(2.0)] = "2.0";
}

/// if types have CommonType comparison is nothrow @nogc
nothrow @nogc unittest
{
    alias C = FastVariant!(short, int, long, float, double);
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

    static assert(C.allowsAssignmentFrom!int);
    static assert(!C.allowsAssignmentFrom!(int[2]));
    static assert(C.allowsAssignmentFrom!(const(int)));

    static assert(C.dataMaxSize == string.sizeof);
    static assert(!__traits(compiles, { assert(d == 'a'); }));

    assert(C() != C());         // two undefined should not equal

    C d;
    C e = d;                    // copy construction
    assert(e != d);             // two undefined should not equal

    d = 11;
    assert(d != e);

    // TODO Allow this d = cast(ubyte)255;

    d = 1.0f;
    assertThrown!VaryNException(d.get!double);
    assert(d.hasValue);
    assert(d.peek!float !is null);
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
    assert(d.peek!double !is null);
    assert(d == 2.0);
    assert(d != 3.0);
    assert(d < 3.0);
    assertThrown!VaryNException(d < 2.0f);
    assertThrown!VaryNException(d < "2.0");
    assert(d.currentSize == double.sizeof);

    d.clear;
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
