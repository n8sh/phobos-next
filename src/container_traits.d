/** Traits used by containers.
 *
 * TODO add `isUnorderedContainer` and `isUnorderedRange` traits and used to
 * forbid hash algorithms to operate on unordered containers (such as
 * `open_hashmap_or_hashset` and `open_hashmap_or_hashset`) and their ranges.
 */
module container_traits;

@safe:

/// True if a `T` needs to be passed by move instead of value.
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

/** Used as an UDA to mark a variable of a type that looks like GC-managed but
 * that is actually not GC-managed, because its allocated by `malloc`, `calloc`
 * or some other non-GC allocator.
 */
enum NoGc;

/**
 * When this enum is used as UDA on aggregate types whose instances are
 * created with construct() a compile time message indicates if a GC range
 * will be added for the members.
 */
enum TellRangeAdded;

/// Returns: `true` iff `T` is a template instance, `false` otherwise.
private template isTemplateInstance(T)
{
    import std.traits : TemplateOf;
    enum isTemplateInstance = is(typeof(TemplateOf!(T)));
}

/**
 * Indicates if an aggregate contains members that might be collected by the
 * garbage collector. This is used in constructors to determine if the content
 * of a manually allocated aggregate must be declared to the GC.
 */
template mustAddGCRange(T)
{
    import std.traits : isPointer, isArray, isStaticArray, isScalarType;
    static if (is(T == class) ||
               isPointer!T)
    {
        enum mustAddGCRange = true;
    }
    else static if (isScalarType!T)
    {
        enum mustAddGCRange = false;
    }
    else static if (is(T == struct) ||
                    is(T == union))
    {
        enum mustAddGCRange = mustAddGCRangeOfStructOrUnion!T;
    }
    else static if (isArray!T)
    {
        static if (isStaticArray!T)
        {
            static if (T.length == 0)
            {
                enum mustAddGCRange = false;
            }
            else
            {
                enum mustAddGCRange = mustAddGCRange!(typeof(T.init[0]));
            }
        }
        else
        {
            enum mustAddGCRange = true;
        }
    }
    else
    {
        static assert(0, "Handle type " ~ T.stringof);
    }
}

private template mustAddGCRangeOfMember(alias member)
{
    import std.traits : hasUDA;
    enum mustAddGCRangeOfMember = !hasUDA!(member, NoGc) && mustAddGCRange!(typeof(member));
}

/// Helper for `mustAddGCRange`.
private template mustAddGCRangeOfStructOrUnion(T)
    if (is(T == struct) ||
        is(T == union))
{
    import std.traits : hasUDA;
    import std.meta : anySatisfy;
    enum mustAddGCRangeOfStructOrUnion = anySatisfy!(mustAddGCRangeOfMember, T.tupleof);
}

///
@safe pure nothrow @nogc unittest
{
    static assert(!mustAddGCRange!int);
    static assert(mustAddGCRange!(int*));
    static assert(mustAddGCRange!(int*[1]));
    static assert(!mustAddGCRange!(int*[0]));
    static assert(mustAddGCRange!(int[]));
}

///
@safe pure nothrow @nogc unittest
{
    struct SmallBin
    {
        string[1] s;
    }
    static assert(mustAddGCRange!SmallBin);

    union HybridBin
    {
        SmallBin small;
    }
    static assert(mustAddGCRange!HybridBin);
}

///
@safe pure nothrow @nogc unittest
{
    struct S
    {
        @NoGc int[] a;
    }
    static assert(!mustAddGCRange!S);
}

///
version(none)
@safe pure nothrow @nogc unittest
{
    class Foo
    {
        @NoGc int[] a;
        @NoGc void* b;
    }
    static assert(!mustAddGCRange!Foo);

    class Bar
    {
        int[] a;
        @NoGc void* b;
    }
    static assert(mustAddGCRange!Bar);

    class Baz : Bar
    {
        @NoGc void* c;
    }
    static assert(mustAddGCRange!Baz);

    struct S
    {
        int x;
    }
    static assert(!mustAddGCRange!S);

    struct T
    {
        int* x;
    }
    static assert(mustAddGCRange!T);
    static assert(mustAddGCRange!(T[1]));

    struct U
    {
        @NoGc int* x;
    }
    static assert(!mustAddGCRange!U);
    static assert(!mustAddGCRange!(U[1]));

    union N
    {
        S s;
        U u;
    }
    static assert(!mustAddGCRange!N);
    static assert(!mustAddGCRange!(N[1]));

    union M
    {
        S s;
        T t;
    }
    static assert(mustAddGCRange!M);
    static assert(mustAddGCRange!(M[1]));
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
    enum hasStandardNullValue = (is(T == class) ||
                                 isPointer!T ||
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

/** Is `true` iff `T` is a nullable type.
 */
template isNullable(T)
{
    enum isNullable = (hasStandardNullValue!T ||
                       // std.traits.Nullable interface:
                       (__traits(hasMember, T, "nullify") &&
                        __traits(hasMember, T, "isNull") &&
                        is(typeof(T.init.isNull()) == bool)//  &&
                        // TODO is(typeof(T.init.nullify()) == void)
                           ));
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

    alias cNi = const(Nullable!int);
    static assert(defaultNullKeyConstantOf!(cNi) == cNi.init);

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
pragma(inline, true)
bool isNull(T)(const scope auto ref T x)
    @safe pure nothrow @nogc
    if (isNullable!(T))
{
    import std.traits : isPointer, isDynamicArray;
    static if (is(T == class) ||
               isPointer!T ||
               isDynamicArray!T ||
               is(T == typeof(null)))
    {
        return x is T.init;
    }
    else
    {
        static assert(0, "Unsupported type " ~ T.stringof);
    }
}

pragma(inline, true)
void nullify(T)(ref T x)
    @safe pure nothrow @nogc
    if (isNullable!(T))
{
    import std.traits : isPointer, isDynamicArray;
    static if (is(T == class) ||
               isPointer!T ||
               isDynamicArray!T ||
               is(T == typeof(null)))
    {
        x = T.init;
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

version(unittest)
{
    import std.typecons : Nullable;
}
