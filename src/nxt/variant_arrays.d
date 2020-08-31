module nxt.variant_arrays;

@safe:

/** Typed index (reference) into an element in `VariantArrays`.
 */
private struct VariantRef(DefinedTypes...)
{
    @safe:

    import std.meta : staticIndexOf;

    alias Kind = ubyte;              // kind code
    alias Size = size_t;             // size type

    import nxt.bit_traits : bitsNeeded;

    /// Used to indicate undefined value.
    private struct Undefined {}

    import std.meta : AliasSeq;

    alias Types = AliasSeq!(Undefined, DefinedTypes);

    /// Number of bits needed to represent kind.
    private enum kindBits = bitsNeeded!(Types.length);

    /** Get number kind of kind type `SomeKind`.
        TODO: make private?
     */
    enum nrOfKind(SomeKind) = staticIndexOf!(SomeKind, Types); // TODO: cast to ubyte if Types.length is <= 256

    /// Is `true` iff an index to a `SomeKind`-kind can be stored.
    enum canReferenceType(SomeKind) = nrOfKind!SomeKind >= 0;

    /// Comparsion works like for integers.
    int opCmp(in typeof(this) rhs) const @trusted
    {
        version(LDC) pragma(inline, true);
        if (this.rawWord < rhs.rawWord)
        {
            return -1;
        }
        if (this.rawWord > rhs.rawWord)
        {
            return +1;
        }
        return 0;
    }

    pragma(inline, true):

    /// Construct from mutable `that`.
    this(in typeof(this) that)
    {
        rawWord = that.rawWord;
    }
    /// Construct from constant `that`.
    this(typeof(this) that)
    {
        rawWord = that.rawWord;
    }

    /// Construct.
    this(Kind kind, Size index) // TODO: can ctor inferred by bitfields?
    {
        _kindNr = kind;
        _index = index;
    }

    /// Construct from raw word representation `rawWord`.
    private this(Size rawWord)
    {
        this.rawWord = rawWord;
    }

    /// Get kindNr.
    Kind kindNr() const
    {
        return _kindNr;
    }

    /// Get index.
    Size index() const
    {
        return _index;
    }

    /// Cast to `size_t`.
    size_t opCast(T : size_t)() const
    {
        return rawWord;
    }

    import core.internal.traits : Unqual;

    /// Allow cast to unqualified.
    U opCast(U : Unqual!(typeof(this)))() const
    {
        return U(rawWord);
    }

    /// The index itself is the hash.
    hash_t toHash() const @property { return rawWord; }
    static assert(hash_t.sizeof == rawWord.sizeof);

    /// Cast to `bool`, meaning 'true' if defined, `false` otherwise.
    bool opCast(U : bool)() const { return isDefined(); }

    /// Returns: `true` iff is defined.
    bool isDefined() const { return rawWord != 0; }

    /// Returns: `true` iff `this` targets a value of type `SomeKind`.
    public bool isA(SomeKind)() const
    {
        return nrOfKind!(SomeKind) == _kindNr;
    }

    @property void toString(scope void delegate(scope const(char)[]) sink) const @trusted
    {
        import std.format : formattedWrite;
        if (isDefined)
        {
            sink.formattedWrite!`%s(%s@%s)`(Unqual!(typeof(this)).stringof, _index, _kindNr);
        }
        else
        {
            sink.formattedWrite!`%s(null)`(Unqual!(typeof(this)).stringof);
        }
    }

    import std.bitmanip : bitfields;
    union
    {
        // TODO: big-endian machine
        mixin(bitfields!(Kind, "_kindNr", kindBits,
                         Size, "_index", 8*Size.sizeof - kindBits));
        Size rawWord;           // for comparsion
    }

    static assert(this.sizeof == Size.sizeof,
                  `This should haven't any memory overhead compared to size_t`);
}

@safe pure nothrow unittest
{
    alias R = VariantRef!(int, float);
    R r;

    import std.array : Appender;
    Appender!(const(R)[]) app;

    const R x;
    R mx = x;

    // TODO: app ~= x;

    const y = [R.init, R.init];
    // TODO: app ~= y;
}

unittest
{
    alias R = VariantRef!(int, float);
    R r;
    import std.conv : to;
    assert(r.to!string == `VariantRef!(int, float)(null)`);
}

// private mixin template VariantArrayOf(Type)
// {
//     import nxt.dynamic_array : DynamicArray;
//     DynamicArray!Type store;
// }

/** Stores set of variants.

    Enables lightweight storage of polymorphic objects.

    Each element is indexed by a corresponding `VariantRef`.
 */
struct VariantArrays(Types...)
{
    @safe:

    alias Ref = VariantRef!Types;

    import nxt.dynamic_array : DynamicArray;

    /// Returns: array type (as a string) of `Type`.
    private static immutable(string) arrayTypeStringOfIndex(uint typeIndex)()
    {
        pragma(inline, true);
        return `DynamicArray!(Types[` ~ typeIndex.stringof ~ `])`;
    }

    /** Returns: array instance (as a strinng) storing `SomeKind`.
     * TODO: make this a template mixin
     */
    private static immutable(string) arrayInstanceString(SomeKind)()
    if (Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        return `_store` ~ Ref.nrOfKind!(SomeKind).stringof; // previously `SomeKind.mangleof`
    }

    /// Make reference to type `SomeKind` at offset `index`.
    static Ref makeRef(SomeKind)(Ref.Size index)
    if (Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        return Ref(Ref.nrOfKind!SomeKind, index);
    }

    /** Insert `value` at back.
     */
    Ref insertBack(SomeKind)(SomeKind value) // TODO: add array type overload
    if (Ref.canReferenceType!SomeKind)
    {
        mixin(`alias arrayInstance = ` ~ arrayInstanceString!SomeKind ~ `;`);
        const currentIndex = arrayInstance.length;
        arrayInstance.insertBackMove(value);
        return Ref(Ref.nrOfKind!SomeKind,
                   currentIndex);
    }
    alias put = insertBack;     // polymorphic `OutputRange` support

    /** Move (emplace) `value` into back.
     */
    Ref insertBackMove(SomeKind)(ref SomeKind value) // TODO: add array type overload
    if (Ref.canReferenceType!SomeKind)
    {
        version(DigitalMars) pragma(inline, false); // DMD cannot inline
        mixin(`alias arrayInstance = ` ~ arrayInstanceString!SomeKind ~ `;`);
        const currentIndex = arrayInstance.length;
        arrayInstance.insertBackMove(value);
        return Ref(Ref.nrOfKind!SomeKind,
                   currentIndex);
    }

    /// ditto
    void opOpAssign(string op, SomeKind)(SomeKind value) // TODO: add array type overload
    if (op == "~" &&
        Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        insertBackMove(value);  // move enables uncopyable types
    }

    /// Get reference to element of type `SomeKind` at `index`.
    scope ref inout(SomeKind) at(SomeKind)(in size_t index) inout return
    if (Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        mixin(`return ` ~ arrayInstanceString!SomeKind ~ `[index];`);
    }

    /// Get reference to element of type `SomeKind` at `ref_`.
    scope ref inout(SomeKind) at(SomeKind)(in Ref ref_) inout return
    if (Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        assert(Ref.nrOfKind!SomeKind == ref_._kindNr,
               "Ref is not of expected template type " ~ SomeKind.stringof);
        mixin(`return ` ~ arrayInstanceString!SomeKind ~ `[ref_.index];`);
    }

    /// Peek at element of type `SomeKind` at `ref_`.
    scope inout(SomeKind)* peek(SomeKind)(in Ref ref_) inout return @system
    if (Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        if (Ref.nrOfKind!SomeKind == ref_._kindNr)
        {
            return &at!SomeKind(ref_._index);
        }
        else
        {
            return null;
        }
    }

    /// Constant access to all elements of type `SomeKind`.
    scope inout(SomeKind)[] allOf(SomeKind)() inout return
    if (Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        mixin(`return ` ~ arrayInstanceString!SomeKind ~ `[];`);
    }

    /// Reserve space for `newCapacity` elements of type `SomeKind`.
    void reserve(SomeKind)(size_t newCapacity)
    if (Ref.canReferenceType!SomeKind)
    {
        pragma(inline, true);
        mixin(`alias arrayInstance = ` ~ arrayInstanceString!SomeKind ~ `;`);
        arrayInstance.reserve(newCapacity);
    }

    /** Returns: length of store. */
    @property size_t length() const
    {
        pragma(inline, true);
        typeof(return) lengthSum = 0;
        foreach (Type; Types)
        {
            mixin(`lengthSum += ` ~ arrayInstanceString!Type ~ `.length;`);
        }
        return lengthSum;
    }

    /** Check if empty. */
    @property bool empty() const
    {
        pragma(inline, true);
        return length == 0;
    }

private:
    static if (false/*__VERSION__ >= 2076*/)
    {
        // static foreach (const typeIndex, Type; Types)
        // {
        //     // TODO: is it better to use?: mixin VariantArrayOf!(Type);
        //     mixin(arrayTypeStringOfIndex!typeIndex ~ ` ` ~ arrayInstanceString!Type ~ `;`);
        // }
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
    import nxt.minimal_fixed_array : MinimalFixedArray;

    alias Chars(uint capacity) = MinimalFixedArray!(char, capacity);
    alias Chars7 = Chars!7;
    alias Chars15 = Chars!15;
    alias VA = VariantArrays!(ulong,
                              Chars7,
                              Chars15);

    VA data;
    assert(data.length == 0);
    assert(data.empty);

    const i0 = data.put(ulong(13));
    assert(cast(size_t)i0 == 1);

    assert(i0.isA!ulong);
    assert(data.at!ulong(0) == ulong(13));
    assert(data.length == 1);
    assert(!data.empty);
    assert(data.allOf!ulong == [ulong(13)].s);

    const i1 = data.put(Chars7(`1234567`));
    assert(cast(size_t)i1 == 2);

    // same order as in `Types`
    assert(i0 < i1);

    assert(i1.isA!(Chars7));
    assert(data.at!(Chars7)(0) == Chars7(`1234567`));
    assert(data.allOf!(Chars7) == [Chars7(`1234567`)].s);
    assert(data.length == 2);

    const i2 = data.put(Chars15(`123`));
    assert(cast(size_t)i2 == 3);

    // same order as in `Types`
    assert(i0 < i2);
    assert(i1 < i2);

    assert(i2.isA!(Chars15));
    assert(data.at!(Chars15)(0) == Chars15(`123`));
    // TODO: assert(data.allOf!(Chars15) == [Chars15(`123`)].s);
    assert(data.length == 3);

    const i3 = data.put(Chars15(`1234`));
    assert(cast(size_t)i3 == 7);

    // same order as in `Types`
    assert(i0 < i3);
    assert(i1 < i3);
    assert(i2 < i3);            // same type, i2 added before i3

    assert(i3.isA!(Chars15));
    assert(data.at!(Chars15)(1) == Chars15(`1234`));
    assert(data.allOf!(Chars15) == [Chars15(`123`), Chars15(`1234`)].s);
    assert(data.length == 4);
}

version = extraTests;

version(extraTests)
{
static private:
    alias I = VariantRef!(Rel1, Rel2, Int);

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

    const S.Ref top = s.put(Rel1(s.put(Rel1(s.put(Rel2([s.put(Int(42)),
                                                        s.put(Int(43))]))))));
    assert(s.allOf!Rel1.length == 2);
    assert(s.allOf!Rel2.length == 1);
    assert(s.allOf!Int.length == 2);
    assert(s.length == 5);
}

/// put and peek
version(extraTests)
@system pure nothrow @nogc unittest
{
    S s;

    const n = 10;
    foreach (const i; 0 .. n)
    {
        S.Ref lone = s.put(Int(i));
        Int* lonePtr = s.peek!Int(lone);
        assert(lonePtr);
        assert(*lonePtr == Int(i));
    }
    assert(s.length == 10);
}

version(unittest)
{
    import nxt.array_help : s;
}

// version = benchmark;

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
