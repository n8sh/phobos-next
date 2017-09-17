module variant_storage;

struct VariantIndex(Types...)
{
    alias Ix = ubyte; // type index type
    enum maxTypesCount = 2^^(Ix.sizeof * 8) - 1; // maximum number of allowed type parameters

    enum typeCount = Types.length;

    private enum N = typeCount; // useful local shorthand

    // import std.bitmanip : bitfields;
    // mixin(bitfields!(Ix, "_type", 1,
    //                  size_t, "_index", 7));
}

/** Stores set of variants.

    Enables lightweight storage of polymorphic objects.

    Each element is indexed by a corresponding `VariantIndex`.
 */
struct VariantStorage(Types...)
{
    alias IndexType = VariantIndex!Types;

    // TODO this crashes. Make this work when LDC is at 2.076
    // import std.meta : AliasSeq;
    // static foreach (Type; Types)
    // {
    // }

    import basic_array : Array = UniqueBasicArray;
    import std.ascii : toLower;

private:
    // storages
    mixin({
            string s = "";
            foreach (i, Type; Types)
            {
                s ~= `Array!` ~ Type.stringof ~ ` ` ~ `_values` ~ Type.stringof ~ `;`;
            }
            return s;
        }());
}

struct Node {}

struct Fn1 {}
struct Fn2 {}

struct Rel1 {}
struct Rel2 {}
struct Rel3 {}

struct Pred1 {}
struct Pred2 {}
struct Pred3 {}
struct Pred4 {}
struct Pred5 {}

@safe pure nothrow @nogc unittest
{
    VariantStorage!(Node,
                    Fn1, Fn2,
                    Rel1, Rel2, Rel3,
                    Pred1, Pred2, Pred3, Pred4, Pred5) vs;
}
