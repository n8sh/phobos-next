module variant_storage;

/** Polymorphic index into an element in `VariantStore`. */
private struct VariantIndex(Types...)
{
    import std.meta : staticIndexOf;
private:
    alias Kind = ubyte;              // kind code
    enum kindBits = 8 * Kind.sizeof; // bits needed to store kind code

    enum nrOfKind(SomeKind) = staticIndexOf!(SomeKind, Types); // TODO cast to ubyte if Types.length is <= 256

    /// Is `true` iff an index to a `SomeKind`-kind can be stored.
    enum canReferTo(SomeKind) = nrOfKind!SomeKind >= 0;

    /// Construct.
    this(Kind kind, size_t index) // TODO can ctor inferred by bitfields?
    {
        _kindNr = kind;
        _index = index;
    }

    /// Returns: `true` iff `this` targets a value of type `SomeKind`.
    bool isA(SomeKind)() const { return nrOfKind!(SomeKind) == _kindNr; }

    import std.bitmanip : bitfields;
    mixin(bitfields!(size_t, "_index", 64 - kindBits,
                     Kind, "_kindNr", kindBits));
}

/** Stores set of variants.

    Enables lightweight storage of polymorphic objects.

    Each element is indexed by a corresponding `VariantIndex`.
 */
struct VariantStorage(Types...)
{
    import std.meta : staticIndexOf;

    alias Index = VariantIndex!Types;

    import basic_copyable_array : CopyableArray; // TODO break out `BasicArray` from CopyableArray

    private static string typeStringOf(Type)()
    {
        static if (__traits(hasMember, Type, `typeString`))
        {
            return Type.typeString;
        }
        else
        {
            return Type.mangleof;
        }
    }

    /// Returns: array type (as a string) of `Type`.
    private static immutable(string) arrayTypeString(Type)()
    {
        return `CopyableArray!(` ~ Type.stringof ~ `)`;
    }

    /// Returns: array instance (as a strinng) storing `Type`.
    private static immutable(string) arrayInstanceString(Type)()
    {
        return `_values` ~ typeStringOf!Type;
    }

    /** Insert `value` at back. */
    Index insertBack(SomeKind)(SomeKind value)
        if (Index.canReferTo!SomeKind)
    {
        mixin(`const currentIndex = ` ~ arrayInstanceString!SomeKind ~ `.length;`);
        mixin(arrayInstanceString!SomeKind ~ `.insertBack(value);`);
        mixin(`const currentLength = ` ~ arrayInstanceString!SomeKind ~ `.length;`);
        return Index(Index.nrOfKind!SomeKind,
                     currentIndex);
    }
    alias put = insertBack;

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
    @property bool empty() const
    {
        return length == 0;
    }

private:
    // TODO this currently crashes
    // static if (__VERSION__ >= 2076)
    // {
    //     static foreach (alias Type; Types)
    //     {
    //         pragma(msg, Type);
    //         // mixin(arrayTypeString!Type ~ ` ` ~ arrayInstanceString!Type ~ `;`);
    //     }
    // }

    // storages
    mixin({
            string s = "";
            foreach (Type; Types)
            {
                s ~= arrayTypeString!Type ~ ` ` ~ arrayInstanceString!Type ~ `;`;
            }
            return s;
        }());
}

version(unittest)
{
    import fixed_array : FixedArray;
    alias Chars(uint capacity) = FixedArray!(char, capacity);
    alias Data = VariantStorage!(Chars!7,
                                 Chars!15,
                                 ulong);
}

/// test regular store
@safe pure nothrow @nogc unittest
{
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

version(unittest)
{
    alias S = VariantStorage!(Rel1, Rel2,
                              Int);

    // relations
    struct Rel1 { S.Index[1] args; }
    struct Rel2 { S.Index[2] args; }

    struct Int { int value; }
}

///
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
    import dbgio : dln;
}
