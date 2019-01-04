/** Traits used by containers.
 *
 * TODO add `isUnorderedContainer` and `isUnorderedRange` traits and used to
 * forbid hash algorithms to operate on unordered containers (such as
 * `open_hashmap_or_hashset` and `open_hashmap_or_hashset`) and their ranges.
 */
module container_traits;

public import gc_traits;

@safe:

/** True if a `T` needs to be passed by move instead of value.
 *
 * See_Also:
 */
template needsMove(T)
{
    import std.traits : hasElaborateDestructor, isCopyable;
    enum needsMove = hasElaborateDestructor!T || !isCopyable!T;
}

// TODO this can be simplified for faster compilation
template ContainerElementType(ContainerType,
                              ElementType)
{
    import std.traits : isMutable, hasIndirections, PointerTarget, isPointer,
        Unqual;

    template ET(bool isConst, T)
    {
        static if (isPointer!ElementType)
        {
            enum PointerIsConst = is(ElementType == const);
            enum PointerIsImmutable = is(ElementType == immutable);
            enum DataIsConst = is(PointerTarget!ElementType == const);
            enum DataIsImmutable = is(PointerTarget!ElementType == immutable);
            static if (isConst)
            {
                static if (PointerIsConst)
                {
                    alias ET = ElementType;
                }
                else static if (PointerIsImmutable)
                {
                    alias ET = ElementType;
                }
                else
                {
                    alias ET = const(PointerTarget!ElementType)*;
                }
            }
            else
            {
                static assert(DataIsImmutable,
                              "An immutable container cannot reference const or mutable data");
                static if (PointerIsConst)
                {
                    alias ET = immutable(PointerTarget!ElementType)*;
                }
                else
                {
                    alias ET = ElementType;
                }
            }
        }
        else
        {
            static if (isConst)
            {
                static if (is(ElementType == immutable))
                {
                    alias ET = ElementType;
                }
                else
                {
                    alias ET = const(Unqual!ElementType);
                }
            }
            else
            {
                alias ET = immutable(Unqual!ElementType);
            }
        }
    }

    static if (isMutable!ContainerType)
    {
        alias ContainerElementType = ElementType;
    }
    else
    {
        static if (hasIndirections!ElementType)
        {
            alias ContainerElementType = ET!(is(ContainerType == const), ElementType);
        }
        else
        {
            alias ContainerElementType = ElementType;
        }
    }
}

/// Returns: `true` iff `T` is a template instance, `false` otherwise.
private template isTemplateInstance(T)
{
    import std.traits : TemplateOf;
    enum isTemplateInstance = is(typeof(TemplateOf!(T)));
}

/** Is `true` iff `T` can be put in a hashset or hashmap. */
template isHashable(T)
{
    import std.traits : hasAliasing;
    enum isHashable = !hasAliasing!T;
}

@safe pure unittest
{
    static assert(isHashable!int);
    static assert(isHashable!string);
    static assert(!isHashable!(char[]));
    static assert(!isHashable!(const(char)[]));
}

/** Is `true` iff `T` is a set like container. */
template isSet(T)
{
    import std.range : hasLength;
    enum isSet = (__traits(hasMember, T, "insert") && // TODO assert O(1)
                  __traits(hasMember, T, "remove") && // TODO assert O(1)
                  __traits(compiles, { auto _ = T.init.byElement; }));
}

/** Is `true` iff `T` is a set like container with elements of type `E`. */
template isSetOf(T, E)
{
    import std.range : hasLength;
    enum isSetOf = (is(typeof(T.init.insert(E.init))) && // TODO assert O(1)
                    is(typeof(T.init.remove(E.init))) && // TODO assert O(1)
                    __traits(compiles, { auto _ = T.init.byElement; }));
}

/** Is `true` iff `T` is a type with a standardized null (zero address) value.
 */
template hasStandardNullValue(T)
{
    import std.traits : isPointer, isDynamicArray;
    import traits_ex : isAddress;
    enum hasStandardNullValue = (isAddress!T ||
                                 isDynamicArray!T ||
                                 is(T == typeof(null)));
}

///
@safe pure nothrow @nogc unittest
{
    class C {}
    static assert( hasStandardNullValue!(C));
    static assert( hasStandardNullValue!(int*));
    static assert( hasStandardNullValue!(int[]));
    static assert( hasStandardNullValue!(const(int)[]));
    static assert(!hasStandardNullValue!(int[3]));
    static assert( hasStandardNullValue!(string));
    static assert(!hasStandardNullValue!(int));
}

/** Is `true` iff `T` is a type with a member null value.
 */
template hasMemberNullValue(T)
{
    enum hasMemberNullValue = __traits(compiles, { T _; _ = T.nullValue; });
}

///
@safe pure nothrow @nogc unittest
{
    class S1
    {
        int x;
        int* xp;
        static nullValue = typeof(this).init;
    }
    static assert(hasMemberNullValue!S1);
}

/** Is `true` iff `T` is a type with a standardized null (zero address) value.
 */
template hasNullValue(T)
{
    import std.traits : isPointer, isDynamicArray;
    enum hasNullValue = (hasStandardNullValue!T ||
                         hasMemberNullValue!T);
}

///
@safe pure nothrow @nogc unittest
{
    static assert(!hasNullValue!int);
    static assert(!hasNullValue!float);
    struct S
    {
        int value;
        static immutable nullValue = typeof(this).init;
    }
    static assert(hasNullValue!S);
}

/** Is `true` iff `T` is type with a predefined undefined (`null`) value.
 */
template isNullable(T)
{
    /* TODO remove this two first cases and rely solely on
     * is(typeof(T.init.nullify()) == void) and
     * is(typeof(T.init.isNull()) == bool)
     */
    // use static if's for full lazyness of trait evaluations in order of likelyhood
    static if (hasStandardNullValue!T)
    {
        enum isNullable = true;
    }
    else static if (hasMemberNullValue!T)
    {
        enum isNullable = true;
    }
    else static if (__traits(hasMember, T, "nullifier"))
    {
        enum isNullable = isNullable!(typeof(T.nullifier)); // TODO require it to be an alias?
    }
    else static if ((__traits(hasMember, T, "isNull") && // fast
                     __traits(hasMember, T, "nullify"))) // fast
    {
        // lazy: only try semantic analysis when members exists
        enum isNullable = (is(typeof(T.init.isNull()) == bool)  &&
                           is(typeof(T.init.nullify()) == void));
    }
    else
    {
        // TODO remove this later on
        // import std.meta : anySatisfy;
        // static if ((is(T == struct) && // unions excluded for now
        //             anySatisfy!(isNullable, typeof(T.init.tupleof))))
        // {
        //     enum isNullable = true;
        // }
        // else
        // {
            enum isNullable = false;
        // }
    }
}

///
@safe pure nothrow @nogc unittest
{
    class C {}

    static assert( isNullable!(C));
    static assert( isNullable!(int*));
    static assert( isNullable!(int[]));
    static assert( isNullable!(const(int)[]));
    static assert(!isNullable!(int[3]));
    static assert( isNullable!(string));
    static assert( isNullable!(Nullable!int));
    static assert(!isNullable!(int));

    struct S
    {
        int value;
        static immutable nullValue = typeof(this).init;
    }

    struct S2 { C x, y; }
    static assert(!isNullable!S2);

    struct S3 { int x, y; }
    static assert(!isNullable!S3);

    struct S4 { C x, y; alias nullifier = x; }
    static assert(isNullable!S4);
}

/** Default null key of type `T`,
 */
template defaultNullKeyConstantOf(T)
{
    static if (isNullable!T)
    {
        enum defaultNullKeyConstantOf = T.init;
    }
    else
    {
        static assert(0, "Unsupported type " ~ T.stringof);
    }
}

///
@safe pure nothrow @nogc unittest
{
    static assert(defaultNullKeyConstantOf!(void*) == null);

    alias Ni = Nullable!int;
    static assert(defaultNullKeyConstantOf!(Ni) == Ni.init);

    // alias cNi = const(Nullable!int);
    // static assert(defaultNullKeyConstantOf!(cNi) == cNi.init);

    alias NubM = Nullable!(ubyte, ubyte.max);
    assert(defaultNullKeyConstantOf!(NubM).isNull);

    alias NuiM = Nullable!(uint, uint.max);
    assert(defaultNullKeyConstantOf!(NuiM).isNull);

    const Nullable!(uint, uint.max) x = 13;
    assert(!x.isNull);
    const y = x;
    assert(!y.isNull);
    assert(!x.isNull);
}

/** Returns: `true` iff `x` has a null value.
 */
bool isNull(T)(const scope auto ref T x)
    @safe pure nothrow @nogc
if (isNullable!(T))
{
    pragma(inline, true);
    static if (hasStandardNullValue!T)
    {
        return x is T.init;
    }
    else static if (hasMemberNullValue!T)
    {
        return x is T.nullValue;
    }
    else static if (__traits(hasMember, T, "nullifier"))
    {
        return x.nullifier.isNull;
    }
    else
    {
        static assert(0, "Unsupported type " ~ T.stringof);
    }
}

void nullify(T)(scope ref T x)
    @safe pure nothrow @nogc
if (isNullable!(T))
{
    pragma(inline, true);
    static if (hasStandardNullValue!T)
    {
        x = T.init;
    }
    else static if (hasMemberNullValue!T)
    {
        x = T.nullValue;
    }
    else static if (__traits(hasMember, T, "nullifier"))
    {
        x.nullifier.nullify();
    }
    else
    {
        static assert(0, "Unsupported type " ~ T.stringof);
    }
}

///
@safe pure nothrow @nogc unittest
{
    assert(null.isNull);

    alias Ni = Nullable!int;
    assert(Ni.init.isNull);

    Ni ni = 3;
    assert(!ni.isNull);

    ni.nullify();
    assert(ni.isNull);

    const Ni ni2 = 3;
    assert(!ni2.isNull);

    struct S
    {
        uint value;
        static immutable nullValue = S(value.max);
    }
    S s;
    assert(!s.isNull);
    s.nullify();
    assert(s.isNull);
}

///
@safe pure nothrow unittest
{
    class C
    {
        @safe pure nothrow
        this(int value)
        {
            this.value = value;
        }
        int value;
    }

    static assert(isNullable!C);

    const x = C.init;
    assert(x.isNull);

    const y = new C(42);
    assert(!y.isNull);
}

/** Allocate an array of `T`-elements of length `length` using `Allocator`.
 */
T[] makeInitZeroArray(T, alias Allocator)(const size_t length) @trusted
{
    static if (__VERSION__ >= 2085)
    {
        // See: https://github.com/dlang/phobos/pull/6411
        import std.experimental.allocator.gc_allocator : GCAllocator;
        static if (__traits(hasMember, GCAllocator, "allocateZeroed"))
        {
            static assert(0, "Use std.experimental.allocator.package.make!(T) instead because it makes use of allocateZeroed.");
        }
    }
    immutable byteCount = T.sizeof * length;
    /* when possible prefer call to calloc before malloc+memset:
     * https://stackoverflow.com/questions/2688466/why-mallocmemset-is-slower-than-calloc */
    static if (__traits(hasMember, Allocator, "allocateZeroed"))
    {
        pragma(inline, true);
        return cast(typeof(return))Allocator.allocateZeroed(byteCount);
    }
    else
    {
        auto array = cast(typeof(return))Allocator.allocate(byteCount);
        import core.stdc.string : memset;
        memset(array.ptr, 0, byteCount);
        return array;
    }
}

/** Variant of `hasElaborateDestructor` that also checks for destructor when `S`
 * is a `class`.
 *
 * See_Also: https://github.com/dlang/phobos/pull/4119
 */
template hasElaborateDestructorNew(S)
{
    import std.traits : isStaticArray;
    static if (isStaticArray!S && S.length)
    {
        enum bool hasElaborateDestructorNew = hasElaborateDestructorNew!(typeof(S.init[0]));
    }
    else static if (is(S == struct) ||
                    is(S == class)) // check also class
    {
        static if (__traits(hasMember, S, "__dtor"))
        {
            enum bool hasElaborateDestructorNew = true;
        }
        else
        {
            import std.traits : FieldTypeTuple;
            import std.meta : anySatisfy;
            enum hasElaborateDestructorNew = anySatisfy!(.hasElaborateDestructorNew, FieldTypeTuple!S);
        }
    }
    else
    {
        enum bool hasElaborateDestructorNew = false;
    }
}

version(unittest)
{
    import std.typecons : Nullable;
}
