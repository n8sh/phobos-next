module variant_storage;

struct VariantIndex(Types...)
{
    alias Ix = ubyte; // type index type
    enum maxTypesCount = 2^^(Ix.sizeof * 8) - 1; // maximum number of allowed type parameters

    enum typeCount = Types.length;

    private enum N = typeCount; // useful local shorthand

    import std.bitmanip : bitfields;
    mixin(bitfields!(Ix, "_type", 1,
                     size_t, "_index", 7));
}

/** Stores set of variants.

    Enables lightweight storage of polymorphic objects.

    Each element is indexed by a corresponding `VariantIndex`.

    - TODO insertBack
    - TODO removeBack
    - TODO removeBackMaybe()
 */
struct VariantStorage(Types...)
{
    alias Index = VariantIndex!Types;

    // TODO this crashes. Make this work when LDC is at 2.076
    // import std.meta : AliasSeq;
    // static foreach (Type; Types)
    // {
    // }

    import basic_array : Array = CopyableArray;

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
        return `Array!(` ~ Type.stringof ~ `)`;
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

    /// Peek at element of type `PeekedValueType` at `peekedIndex`.
    version(none)
    auto ref peek(PeekedValueType)(in Index peekedIndex)
    {
        import std.conv : to;
        const peekedIndexString = peekedIndex._index.to!string;
        mixin(`return ` ~ arrayInstanceString!PeekedValueType ~ `[peekedIndexString];`);
    }

    /// Peek at element of type `PeekedValueType` at `peekedIndex`.
    void print(PeekedValueType)(in Index peekedIndex)
    {
        import std.conv : to;
        const peekedIndexString = peekedIndex._index.to!string;
        final switch (peekedIndex._type)
        {
            foreach (const typeIx, Type; Types)
            {
            case typeIx:
                mixin(`return ` ~ arrayInstanceString!PeekedValueType ~ `[peekedIndexString];`);
            }
        }
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

    // auto node = vs.peek!Fn1(0);
}

version(unittest)
{
    alias Data = VariantStorage!(Chars7,
                                 Chars15,
                                 Chars23,
                                 Chars31,
                                 string,
                                 ulong);

    import arrayn : ArrayN, Checking;

    // small strings
    alias Chars7  = ArrayN!(char, 7);
    alias Chars15 = ArrayN!(char, 15);
    alias Chars23 = ArrayN!(char, 23);
    alias Chars31 = ArrayN!(char, 31);

    static assert(Chars7.sizeof == 8);
    static assert(Chars15.sizeof == 16);
    static assert(Chars23.sizeof == 24);
    static assert(Chars31.sizeof == 32);
}
