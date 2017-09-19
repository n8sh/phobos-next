module variant_storage;

struct VariantIndex(Types...)
{
    import std.meta : staticIndexOf;

    alias Type = ubyte; // type index type
    enum typeBits = 8 * Type.sizeof;
    enum maxTypesCount = 2^^(typeBits) - 1; // maximum number of allowed type parameters

    enum indexOf(U) = staticIndexOf!(U, Types); // TODO cast to ubyte if Types.length is <= 256
    enum canStore(U) = indexOf!U >= 0;

    enum typeCount = Types.length;

    private enum N = typeCount; // useful local shorthand

    this(Type type, size_t index) // TODO can ctor inferred by bitfields?
    {
        _type = type;
        _index = index;
    }

    /// Returns: `true` iff `this` targets a value of type `U`.
    bool isOfType(U)() const { return indexOf!(U) == _type; }

    import std.bitmanip : bitfields;
    mixin(bitfields!(size_t, "_index", 64 - typeBits,
                     Type, "_type", typeBits));
}

/** Stores set of variants.

    Enables lightweight storage of polymorphic objects.

    Each element is indexed by a corresponding `VariantIndex`.

    - TODO removeBack
    - TODO removeBackMaybe()
 */
struct VariantStorage(Types...)
{
    import std.meta : staticIndexOf;

    alias Index = VariantIndex!Types;

    enum indexOf(U) = staticIndexOf!(U, Types); // TODO cast to ubyte if Types.length is <= 256
    enum canStore(U) = indexOf!U >= 0;

    import basic_copyable_array : CopyableArray; // TODO break out `BasicArray` from CopyableArray

    static string typeStringOf(Type)()
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
    static string arrayTypeString(Type)()
    {
        return `CopyableArray!(` ~ Type.stringof ~ `)`;
    }

    /// Returns: array instance (as a strinng) storing `Type`.
    static string arrayInstanceString(Type)()
    {
        return `_values` ~ typeStringOf!Type;
    }

    version(LDC)
    {
        static if (__VERSION__ >= 2076)
        {
            pragma(msg, __FILE__ ~ ":" ~ __LINE__.stringof ~ ": info: LDC now has foreach, so use it to generate arrays");
        }
    }

    /** Insert `value` at back. */
    Index insertBack(U)(U value)
        if (canStore!U)
    {
        mixin(arrayInstanceString!U ~ `.insertBack(value);`);
        mixin(`const currentLength = ` ~ arrayInstanceString!U ~ `.length;`);
        return typeof(return)(indexOf!U, currentLength);
    }

    /// Peek at element of type `U` at `index`.
    scope ref inout(U) ofTypeAt(U)(in size_t index) inout return
        if (canStore!U)
    {
        mixin(`return ` ~ arrayInstanceString!U ~ `[index];`);
    }

    /// Get all elements of type `U`.
    scope inout(U)[] allOfType(U)() inout return
        if (canStore!U)
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
    import chars : FewChars;
    alias Data = VariantStorage!(FewChars!7,
                                 FewChars!15,
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

    assert(data.insertBack(FewChars!7(`1234567`)).isOfType!(FewChars!7));
    assert(data.ofTypeAt!(FewChars!7)(0) == FewChars!7(`1234567`));
    assert(data.length == 2);

    assert(data.insertBack(FewChars!15(`123`)).isOfType!(FewChars!15));
    assert(data.ofTypeAt!(FewChars!15)(0) == FewChars!15(`123`));
    assert(data.length == 3);

    assert(data.insertBack(FewChars!15(`1234`)).isOfType!(FewChars!15));
    assert(data.ofTypeAt!(FewChars!15)(1) == FewChars!15(`1234`));
    assert(data.length == 4);
}

version(unittest)
{
    alias VS = VariantStorage!(Rel1, Rel2, Rel3, Rel4, Rel5,
                               Pred1, Pred2, Pred3, Pred4, Pred5,
                               Fn1, Fn2, Fn3, Fn4);

    // relations
    struct Rel1 { VS.Index a; }
    struct Rel2 { VS.Index a, b; }
    struct Rel3 { VS.Index a, b, c; }
    struct Rel4 { VS.Index a, b, c, d; }
    struct Rel5 { VS.Index a, b, c, d, e; }

    // predicates
    struct Pred1 { VS.Index a; }
    struct Pred2 { VS.Index a, b; }
    struct Pred3 { VS.Index a, b, c; }
    struct Pred4 { VS.Index a, b, c, d; }
    struct Pred5 { VS.Index a, b, c, d, e; }

    // functions
    struct Fn1 { VS.Index a; }
    struct Fn2 { VS.Index a, b; }
    struct Fn3 { VS.Index a, b, c; }
    struct Fn4 { VS.Index a, b, c, d; }
}

@safe pure nothrow @nogc unittest
{
    VS vs;
}

version(unittest)
{
    import array_help : s;
}
