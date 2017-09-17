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
 */
struct VariantStorage(Types...)
{
    alias Index = VariantIndex!Types;

    // TODO this crashes. Make this work when LDC is at 2.076
    // import std.meta : AliasSeq;
    // static foreach (Type; Types)
    // {
    // }

    import basic_array : Array = UniqueBasicArray;

    /// Returns: array type (as a string) of `Type`.
    static string arrayTypeString(Type)()
    {
        return `Array!` ~ Type.stringof;
    }

    /// Returns: array instance (as a strinng) storing `Type`.
    static string arrayInstanceString(Type)()
    {
        return `_values` ~ Type.stringof;
    }

    /// Peek at element of type `ValueType` at `index`.
    ValueType peek(ValueType)(in Index index)
    {
        final switch (index._type)
        {
            foreach (const typeIx, Type; Types)
            {
            case typeIx:
                // return xxx[index._index];
                break;
            }
        }
        return typeof(return).init;
    }

private:
    // storages
    mixin({
            string s = "";
            foreach (i, Type; Types)
            {
                s ~= arrayTypeString!Type ~ ` ` ~ `_values` ~ arrayInstanceString!Type ~ `;`;
            }
            return s;
        }());
}

version(unittest)
{
    alias VS = VariantStorage!(Node,
                               Fn1, Fn2,
                               Rel1, Rel2, Rel3,
                               Pred1, Pred2, Pred3, Pred4, Pred5);
    struct Node {}

    struct Fn1 { VS.Index a; }
    struct Fn2 { VS.Index a, b; }

    struct Rel1 { VS.Index a; }
    struct Rel2 { VS.Index a, b; }
    struct Rel3 { VS.Index a, b, c; }

    struct Pred1 { VS.Index a; }
    struct Pred2 { VS.Index a, b; }
    struct Pred3 { VS.Index a, b, c; }
    struct Pred4 { VS.Index a, b, c, d; }
    struct Pred5 { VS.Index a, b, c, d, e; }
}

@safe pure nothrow @nogc unittest
{


    VS vs;

    auto node = vs.peek!Node(VS.Index.init);
}
