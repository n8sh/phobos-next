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
    alias IndexType = VariantIndex!Types;

    // TODO this crashes
    // import std.meta : AliasSeq;
    // static foreach (Type; Types)
    // {
    // }

    import basic_array : UniqueBasicArray;
    import std.ascii : toLower;

    mixin({
            string s = "";
            foreach (i, Type; Types)
            {
                s ~= `UniqueBasicArray!` ~ Type.stringof ~ ` ` ~ `values` ~ Type.stringof ~ `;`;
            }
            return s;
        }());
}

@safe pure nothrow @nogc unittest
{
    VariantStorage!(ubyte, ushort, uint, ulong, float, double, string) vs;
}
