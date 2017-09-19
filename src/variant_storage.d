module variant_storage;

/** Polymorphic index into an element in `VariantStore`. */
private struct VariantIndex(Types...)
{
    import std.meta : staticIndexOf;
private:
    alias Kind = ubyte;              // kind code
    enum kindBits = 8 * Kind.sizeof; // bits needed to store kind code

    enum nrOfKind(AnyKind) = staticIndexOf!(AnyKind, Types); // TODO cast to ubyte if Types.length is <= 256

    /// Is `true` iff an index to a `AnyKind`-kind can be stored.
    enum canReferToType(AnyKind) = nrOfKind!AnyKind >= 0;

    /// Construct.
    this(Kind kind, size_t index) // TODO can ctor inferred by bitfields?
    {
        _kindNr = kind;
        _index = index;
    }

    /// Returns: `true` iff `this` targets a value of type `U`.
    bool isA(U)() const { return nrOfKind!(U) == _kindNr; }

    import std.bitmanip : bitfields;
    mixin(bitfields!(size_t, "_index", 64 - kindBits,
                     Kind, "_kindNr", kindBits));
}

/** Stores set of variants.

    Enables lightweight storage of polymorphic objects.

    Each element is indexed by a corresponding `VariantIndex`.

    - TODO Range this.filterOfType!U(variantIndexes) inout, Range element is of type `ref inout(U)`
    - TODO removeBack
    - TODO removeBackMaybe()
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
    Index insertBack(AnyKind)(AnyKind value)
        if (Index.canReferToType!AnyKind)
    {
        mixin(`const currentIndex = ` ~ arrayInstanceString!AnyKind ~ `.length;`);
        mixin(arrayInstanceString!AnyKind ~ `.insertBack(value);`);
        mixin(`const currentLength = ` ~ arrayInstanceString!AnyKind ~ `.length;`);
        return Index(Index.nrOfKind!AnyKind,
                     currentIndex);
    }
    alias put = insertBack;

    /// Get reference to element of type `U` at `index`.
    scope ref inout(U) at(U)(in size_t index) inout return
        if (Index.canReferToType!U)
    {
        mixin(`return ` ~ arrayInstanceString!U ~ `[index];`);
    }

    /// Peek at element of type `U` at `index`.
    scope inout(U)* peek(U)(in Index index) inout return @system
        if (Index.canReferToType!U)
    {
        dln(index._kindNr);
        if (Index.nrOfKind!U == index._kindNr)
        {
            dln(index._index);
            return &at!U(index._index);
        }
        else
        {
            return null;
        }
    }

    /// Constant access to all elements of type `U`.
    scope const(U)[] allOf(U)() const return
        if (Index.canReferToType!U)
    {
        mixin(`return ` ~ arrayInstanceString!U ~ `[];`);
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
    alias S = VariantStorage!(Rel1, Rel2, Rel3, Rel4, Rel5,
                              Pred1, Pred2, Pred3, Pred4, Pred5,
                              Fn1, Fn2, Fn3, Fn4,
                              UInt);

    // relations
    struct Rel1 { S.Index[1] args; }
    struct Rel2 { S.Index[2] args; }
    struct Rel3 { S.Index[3] args; }
    struct Rel4 { S.Index[4] args; }
    struct Rel5 { S.Index[5] args; }

    // predicates
    struct Pred1 { S.Index[1] args; }
    struct Pred2 { S.Index[2] args; }
    struct Pred3 { S.Index[3] args; }
    struct Pred4 { S.Index[4] args; }
    struct Pred5 { S.Index[5] args; }

    // functions
    struct Fn1 { S.Index[1] args; }
    struct Fn2 { S.Index[2] args; }
    struct Fn3 { S.Index[3] args; }
    struct Fn4 { S.Index[4] args; }

    struct UInt { uint value; }
}

@safe pure nothrow @nogc unittest
{
    S s;
    S.Index top = s.put(Rel1(s.put(Rel1(s.put(Rel2([s.put(UInt(42)),
                                                    s.put(UInt(43))]))))));
    assert(s.allOf!Rel1.length == 2);
    assert(s.allOf!Rel2.length == 1);
    assert(s.allOf!UInt.length == 2);
    assert(s.length == 5);
}

pure nothrow @nogc unittest
{
    S s;

    S.Index lone = s.put(Rel1());
    Rel1* lonePtr = s.peek!Rel1(lone);
    assert(lonePtr);
}

version(unittest)
{
    import array_help : s;
    import dbgio : dln;
}
