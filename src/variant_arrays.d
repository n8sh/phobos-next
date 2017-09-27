module variant_arrays;

/** Polymorphic index into an element in `VariantArrays`. */
private struct VariantIndex(Types...)
{
    import std.meta : staticIndexOf;
private:
    alias Kind = ubyte;              // kind code
    alias Size = size_t;             // size type

    import bit_traits : bitsNeeded;

    /// Number of bits needed to represent kind.
    enum kindBits = bitsNeeded!(Types.length);

    /// Get number kind of kind type `SomeKind`.
    enum nrOfKind(SomeKind) = staticIndexOf!(SomeKind, Types); // TODO cast to ubyte if Types.length is <= 256

    /// Is `true` iff an index to a `SomeKind`-kind can be stored.
    enum canReferTo(SomeKind) = nrOfKind!SomeKind >= 0;

    pragma(inline, true):

    /// Construct.
    this(Kind kind, Size index) // TODO can ctor inferred by bitfields?
    {
        _kindNr = kind;
        _index = index;
    }

    /// Returns: `true` iff `this` targets a value of type `SomeKind`.
    bool isA(SomeKind)() const { return nrOfKind!(SomeKind) == _kindNr; }

    import std.bitmanip : bitfields;
    mixin(bitfields!(Size, "_index", 8*Size.sizeof - kindBits,
                     Kind, "_kindNr", kindBits));
}

private mixin template VariantArrayOf(Type)
{
    import basic_copyable_array : CopyableArray;
    CopyableArray!Type store;
}

/** Stores set of variants.

    Enables lightweight storage of polymorphic objects.

    Each element is indexed by a corresponding `VariantIndex`.
 */
private struct VariantArrays(Types...)
{
    alias Index = VariantIndex!Types;

    import basic_copyable_array : CopyableArray;

    pragma(inline, true):

    /// Returns: array type (as a string) of `Type`.
    private static immutable(string) arrayTypeStringOfIndex(uint typeIndex)()
    {
        return `CopyableArray!(Types[` ~ typeIndex.stringof ~ `])`;
    }

    /// Returns: array instance (as a strinng) storing `Type`.
    private static immutable(string) arrayInstanceString(Type)()
    {
        return `_values` ~ Type.mangleof;
    }

    /** Insert `value` at back.
     */
    pragma(inline)                             // DMD cannot inline
    Index insertBack(SomeKind)(SomeKind value) // TODO add array type overload
        if (Index.canReferTo!SomeKind)
    {
        mixin(`alias arrayInstance = ` ~ arrayInstanceString!SomeKind ~ `;`);
        const currentIndex = arrayInstance.length;
        arrayInstance.insertBack(value);
        return Index(Index.nrOfKind!SomeKind,
                     currentIndex);
    }
    alias put = insertBack;     // polymorphic `OutputRange` support

    /// ditto
    void opOpAssign(string op, SomeKind)(SomeKind value) // TODO add array type overload
        if (op == "~" &&
            Index.canReferTo!SomeKind)
    {
        insertBack(value);
    }

    /// Get reference to element of type `SomeKind` at `index`.
    scope ref inout(SomeKind) at(SomeKind)(in size_t index) inout return
        if (Index.canReferTo!SomeKind)
    {
        mixin(`return ` ~ arrayInstanceString!SomeKind ~ `[index];`);
    }

    /// Peek at element of type `SomeKind` at `index`.
    scope inout(SomeKind)* peek(SomeKind)(in Index index) inout return @system
        if (Index.canReferTo!SomeKind)
    {
        if (Index.nrOfKind!SomeKind == index._kindNr)
        {
            return &at!SomeKind(index._index);
        }
        else
        {
            return null;
        }
    }

    /// Constant access to all elements of type `SomeKind`.
    scope const(SomeKind)[] allOf(SomeKind)() const return
        if (Index.canReferTo!SomeKind)
    {
        mixin(`return ` ~ arrayInstanceString!SomeKind ~ `[];`);
    }

    /** Returns: length of store. */
    @property size_t length() const
    {
        typeof(return) lengthSum = 0;
        foreach (Type; Types)
        {
            mixin(`lengthSum += ` ~ arrayInstanceString!Type ~ `.length;`);
        }
        return lengthSum;
    }

    /** Check if empty. */
    @property bool empty() const { return length == 0; }

private:
    static if (__VERSION__ >= 2076)
    {
        static foreach (const typeIndex, Type; Types)
        {
            // mixin VariantArrayOf!(Type);
            mixin(arrayTypeStringOfIndex!typeIndex ~ ` ` ~ arrayInstanceString!Type ~ `;`);
        }
    }
    else
    {
        mixin({
                string s = "";
                foreach (const typeIndex, Type; Types)
                {
                    s ~= arrayTypeStringOfIndex!typeIndex ~ ` ` ~ arrayInstanceString!Type ~ `;`;
                }
                return s;
            }());
    }
}

///
@safe pure nothrow @nogc unittest
{
    import fixed_array : BasicFixedArray;

    alias Chars(uint capacity) = BasicFixedArray!(char, capacity);
    alias Data = VariantArrays!(Chars!7,
                                Chars!15,
                                ulong);

    Data data;
    assert(data.length == 0);
    assert(data.empty);

    assert(data.put(ulong(13)).isA!ulong);
    assert(data.at!ulong(0) == ulong(13));
    assert(data.length == 1);
    assert(!data.empty);
    assert(data.allOf!ulong == [ulong(13)].s);

    assert(data.put(Chars!7(`1234567`)).isA!(Chars!7));
    assert(data.at!(Chars!7)(0) == Chars!7(`1234567`));
    assert(data.allOf!(Chars!7) == [Chars!7(`1234567`)].s);
    assert(data.length == 2);

    assert(data.put(Chars!15(`123`)).isA!(Chars!15));
    assert(data.at!(Chars!15)(0) == Chars!15(`123`));
    assert(data.allOf!(Chars!15) == [Chars!15(`123`)].s);
    assert(data.length == 3);

    assert(data.put(Chars!15(`1234`)).isA!(Chars!15));
    assert(data.at!(Chars!15)(1) == Chars!15(`1234`));
    assert(data.allOf!(Chars!15) == [Chars!15(`123`), Chars!15(`1234`)].s);
    assert(data.length == 4);
}

// version = extraTests;

version(extraTests)
{
static private:
    alias I = VariantIndex!(Rel1, Rel2,
                            Int);

    // relations
    struct Rel1 { I[1] args; }
    struct Rel2 { I[2] args; }

    alias S = VariantArrays!(Rel1, Rel2,
                             Int);

    struct Int { int value; }
}

///
version(extraTests)
@safe pure nothrow @nogc unittest
{
    S s;

    S.Index top = s.put(Rel1(s.put(Rel1(s.put(Rel2([s.put(Int(42)),
                                                    s.put(Int(43))]))))));
    assert(s.allOf!Rel1.length == 2);
    assert(s.allOf!Rel2.length == 1);
    assert(s.allOf!Int.length == 2);
    assert(s.length == 5);
}

/// put and peek
version(extraTests)
pure nothrow @nogc unittest
{
    S s;

    const n = 10;
    foreach (const i; 0 .. n)
    {
        S.Index lone = s.put(Int(i));
        Int* lonePtr = s.peek!Int(lone);
        assert(lonePtr);
        assert(*lonePtr == Int(i));
    }
    assert(s.length == 10);
}

version(unittest)
{
    import array_help : s;
}

version = benchmark;

version(benchmark)
unittest
{
    alias E = uint;
    immutable n = 5_000_000;

    import std.stdio : writeln;
    import std.datetime : MonoTime;
    import std.meta : AliasSeq;
    foreach (A; AliasSeq!(VariantArrays!(E)))
    {
        A a;

        immutable before = MonoTime.currTime();

        foreach (uint i; 0 .. n)
        {
            a ~= i;
        }

        immutable after = MonoTime.currTime();

        writeln("Added ", n, " integer nodes into ", A.stringof, " in ", after - before);
    }

}
