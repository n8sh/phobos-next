/** Traits used by containers.
 *
 * TODO add `isUnorderedContainer` and `isUnorderedRange` traits and used to
 * forbid hash algorithms to operate on unordered containers (such as
 * `open_hashmap` and `open_hashmap`) and their ranges.
 */
module nxt.container_traits;

public import nxt.gc_traits;

@safe:

/** True if a `T` needs to be passed by move instead of value.
 *
 * See_Also:
 */
template needsMove(T)
{
    import core.internal.traits : hasElaborateDestructor;
    enum needsMove = hasElaborateDestructor!T || !__traits(isCopyable, T); // TODO is this ok?
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
    import std.range.primitives : hasLength;
    enum isSet = (__traits(hasMember, T, "insert") && // TODO assert O(1)
                  __traits(hasMember, T, "remove") && // TODO assert O(1)
                  __traits(compiles, { auto _ = T.init.byElement; }));
}

/** Is `true` iff `T` is a set like container with elements of type `E`. */
template isSetOf(T, E)
{
    import std.range.primitives : hasLength;
    enum isSetOf = (is(typeof(T.init.insert(E.init))) && // TODO assert O(1)
                    is(typeof(T.init.remove(E.init))) && // TODO assert O(1)
                    __traits(compiles, { auto _ = T.init.byElement; }));
}

/** Allocate an array of `T`-elements of length `length` using `Allocator`.
 */
T[] makeInitZeroArray(T, alias Allocator)(const size_t length) @trusted
{
    version(none)               // TODO activate
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
    static if (is(S == struct) ||
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
        import std.traits : isStaticArray;
        static if (isStaticArray!S && S.length)
        {
            enum bool hasElaborateDestructorNew = hasElaborateDestructorNew!(typeof(S.init[0]));
        }
        else
        {
            enum bool hasElaborateDestructorNew = false;
        }
    }
}

/** Is `true` iff `T` is repesented as a memory address. */
template isAddress(T)
{
    static if (is(T == class))
    {
        enum isAddress = true;  // a class is memory-wise just a pointer
    }
    else
    {
        import std.traits : isPointer;
        enum isAddress = isPointer!T;
    }
}

///
@safe pure nothrow @nogc unittest
{
    static assert( isAddress!(int*));
    static assert(!isAddress!(int));

    class C {}
    static assert( isAddress!(C));

    struct S {}
    static assert(!isAddress!(S));
    static assert( isAddress!(S*));
}
