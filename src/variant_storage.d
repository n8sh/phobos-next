module variant_storage;

/** Polymorphic index into an element in `VariantStore`. */
private struct VariantIndex(Types...)
{
    import std.meta : staticIndexOf;

private:
    alias Kind = ubyte; // kind code
    enum kindBits = 8 * Kind.sizeof;
    enum maxTypesCount = 2^^(kindBits) - 1; // maximum number of allowed type parameters

    enum indexOf(U) = staticIndexOf!(U, Types); // TODO cast to ubyte if Types.length is <= 256
    enum canStore(U) = indexOf!U >= 0;

    /// Construct.
    this(Kind kind, size_t index) // TODO can ctor inferred by bitfields?
    {
        _kind = kind;
        _index = index;
    }

    /// Returns: `true` iff `this` targets a value of type `U`.
    bool isOfType(U)() const { return indexOf!(U) == _kind; }

    import std.bitmanip : bitfields;
    mixin(bitfields!(size_t, "_index", 64 - kindBits,
                     Kind, "_kind", kindBits));
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
    private static string arrayTypeString(Type)()
    {
        return `CopyableArray!(` ~ Type.stringof ~ `)`;
    }

    /// Returns: array instance (as a strinng) storing `Type`.
    private static string arrayInstanceString(Type)()
    {
        return `_values` ~ typeStringOf!Type;
    }

    /** Insert `value` at back. */
    Index insertBack(U)(U value)
        if (Index.canStore!U)
    {
        mixin(arrayInstanceString!U ~ `.insertBack(value);`);
        mixin(`const currentLength = ` ~ arrayInstanceString!U ~ `.length;`);
        return typeof(return)(Index.indexOf!U, currentLength);
    }

    /// Peek at element of type `U` at `index`.
    scope ref inout(U) ofTypeAt(U)(in size_t index) inout return
        if (Index.canStore!U)
    {
        mixin(`return ` ~ arrayInstanceString!U ~ `[index];`);
    }

    /// Get all elements of type `U`.
    scope inout(U)[] allOfType(U)() inout return
        if (Index.canStore!U)
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

private:
    version(LDC)
    {
        static if (__VERSION__ >= 2076)
        {
            pragma(msg, __FILE__ ~ ":" ~ __LINE__.stringof ~
                   ": info: both DMD and LDC now has foreach, so use it to generate storages below");
        }
    }

    // storages
    mixin({
            string s = "";
            foreach (i, Type; Types)
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

    assert(data.insertBack(ulong(13)).isOfType!ulong);
    assert(data.ofTypeAt!ulong(0) == ulong(13));
    assert(data.length == 1);
    assert(data.allOfType!ulong == [ulong(13)].s);

    assert(data.insertBack(Chars!7(`1234567`)).isOfType!(Chars!7));
    assert(data.ofTypeAt!(Chars!7)(0) == Chars!7(`1234567`));
    assert(data.allOfType!(Chars!7) == [Chars!7(`1234567`)].s);
    assert(data.length == 2);

    assert(data.insertBack(Chars!15(`123`)).isOfType!(Chars!15));
    assert(data.ofTypeAt!(Chars!15)(0) == Chars!15(`123`));
    assert(data.allOfType!(Chars!15) == [Chars!15(`123`)].s);
    assert(data.length == 3);

    assert(data.insertBack(Chars!15(`1234`)).isOfType!(Chars!15));
    assert(data.ofTypeAt!(Chars!15)(1) == Chars!15(`1234`));
    assert(data.allOfType!(Chars!15) == [Chars!15(`123`),
                                            Chars!15(`1234`)].s);
    assert(data.length == 4);
}

version(unittest)
{
    alias S = VariantStorage!(Rel1, Rel2, Rel3, Rel4, Rel5,
                              Pred1, Pred2, Pred3, Pred4, Pred5,
                              Fn1, Fn2, Fn3, Fn4,
                              UInt);

    // relations
    struct Rel1 { S.Index a; }
    struct Rel2 { S.Index a, b; }
    struct Rel3 { S.Index a, b, c; }
    struct Rel4 { S.Index a, b, c, d; }
    struct Rel5 { S.Index a, b, c, d, e; }

    // predicates
    struct Pred1 { S.Index a; }
    struct Pred2 { S.Index a, b; }
    struct Pred3 { S.Index a, b, c; }
    struct Pred4 { S.Index a, b, c, d; }
    struct Pred5 { S.Index a, b, c, d, e; }

    // functions
    struct Fn1 { S.Index a; }
    struct Fn2 { S.Index a, b; }
    struct Fn3 { S.Index a, b, c; }
    struct Fn4 { S.Index a, b, c, d; }

    struct UInt { uint value; }
}

@safe pure nothrow @nogc unittest
{
    S s;

    s.insertBack(Rel1(s.insertBack(Rel1(s.insertBack(Rel1(s.insertBack(UInt(42))))))));
    assert(s.length == 4);
}

version(unittest)
{
    import array_help : s;
    import dbgio : dln;
}
