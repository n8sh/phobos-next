module gc_traits;

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

/** Indicates if an aggregate contains members that might be collected by the
 * garbage collector. This is used in constructors to determine if the content
 * of a manually allocated aggregate must be declared to the GC.
 */
template mustAddGCRange(T)
{
    import std.traits : isPointer, isArray, isStaticArray, isScalarType;

    private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                                 isPointer!T);     // just a pointer, consistent with opCmp

    static if (isAddress!T)
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

///
@safe pure nothrow @nogc unittest
{
    static assert(!mustAddGCRange!int);
    static assert(mustAddGCRange!(int*));
    static assert(mustAddGCRange!(int*[1]));
    static assert(!mustAddGCRange!(int*[0]));
    static assert(mustAddGCRange!(int[]));
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
    struct S
    {
        @disable this(this);
        int* _ptr;
    }
    pragma(msg, typeof(S.tupleof));
    // See https://forum.dlang.org/post/dkohvpbmakbdbhnmnmbg@forum.dlang.org
    // TODO static assert(!mustAddGCRangeOfStructOrUnion!S);
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
