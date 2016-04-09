module typecons_ex;

// TODO Add to Phobos and refer to http://forum.dlang.org/thread/lzyqywovlmdseqgqfvun@forum.dlang.org#post-ibvkvjwexdafpgtsamut:40forum.dlang.org
// TODO Better with?:
/* inout(Nullable!T) nullable(T)(inout T a) */
/* { */
/*     return typeof(return)(a); */
/* } */
/* inout(Nullable!(T, nullValue)) nullable(alias nullValue, T)(inout T value) */
/* if (is (typeof(nullValue) == T)) */
/* { */
/*     return typeof(return)(value); */
/* } */

public import ties;

import std.typecons: Nullable, NullableRef;

/** Instantiator for $(D Nullable).
 */
auto nullable(T)(T a)
{
    return Nullable!T(a);
}
unittest
{
    auto x = 42.5.nullable;
    assert(is(typeof(x) == Nullable!double));
}

/** Instantiator for $(D Nullable).
*/
auto nullable(alias nullValue, T)(T value)
    if (is (typeof(nullValue) == T))
{
    return Nullable!(T, nullValue)(value);
}
unittest
{
    auto x = 3.nullable!(int.max);
    assert(is (typeof(x) == Nullable!(int, int.max)));
}

/** Instantiator for $(D NullableRef).
 */
auto nullableRef(T)(T* a) @safe pure nothrow
{
    return NullableRef!T(a);
}
unittest
{
    auto x = 42.5;
    auto xr = nullableRef(&x);
    assert(!xr.isNull);
    xr.nullify;
    assert(xr.isNull);
}

/** See also: http://forum.dlang.org/thread/jwdbjlobbilowlnpdzzo@forum.dlang.org
 */
template New(T) if (is(T == class))
{
    T New(Args...) (Args args) {
        return new T(args);
    }
}

import std.traits: isArray, isUnsigned, isInstanceOf, isSomeString;
import std.range.primitives: hasSlicing;

/** Check if $(D T) is castable to $(D U).
 */
enum isCastableTo(T, U) = __traits(compiles, { T i = 0; cast(U)i; });

enum isIndex(I) = (is(I == enum) ||
                   isUnsigned!I || // TODO should we allow isUnsigned here?
                   isCastableTo!(I, size_t));

/** Check if $(D R) is indexable by $(D I). */
enum isIndexableBy(R, I) = (isArray!R && // TODO generalize to RandomAccessContainers. Ask on forum for hasIndexing!R.
                            isIndex!I);

unittest
{
    static assert(isIndexableBy!(int[3], ubyte));
}

/**
   Check if $(D R) is indexable by $(D I).
 */
enum isIndexableBy(R, alias I) = (isArray!R && // TODO generalize to RandomAccessContainers. Ask on forum for hasIndexing!R.
                                  (isSomeString!(typeof(I))));

unittest
{
    static assert(isIndexableBy!(int[], "I"));
}

mixin template genOps(T)
{
    auto ref opIndex(T i) inout
    {
        assert(cast(size_t)i < _r.length, "Range violation with index " ~ T.stringof);
        return _r[cast(size_t)i];
    }
    auto ref opIndexAssign(V)(V value, T i)
    {
        assert(cast(size_t)i < _r.length, "Range violation with index " ~ T.stringof);
        return _r[cast(size_t)i] = value;
    }

    static if (hasSlicing!R)
    {
        auto ref opSlice(T i, T j) inout             { return _r[cast(size_t)i ..
                                                                 cast(size_t)j]; }
        auto ref opSliceAssign(V)(V value, T i, T j) { return _r[cast(size_t)i ..
                                                                 cast(size_t)j] = value; }
    }
}

/** Wrapper for $(D R) with Type-Safe $(D I)-Indexing.
    See also: http://forum.dlang.org/thread/gayfjaslyairnzrygbvh@forum.dlang.org#post-gayfjaslyairnzrygbvh:40forum.dlang.org

    TODO Merge with https://github.com/rcorre/enumap

    TODO Use std.range.indexed when I is an enum with non-contigious
    enumerators. Perhaps use among aswell.

    TODO Rename to something more concise such as [Bb]y.

    TODO Allow $(D I) to be a string and if so derive $(D Index) to be that string.

    TODO Support R being a static array:
         - If I is an enum its number of elements should match R.length
   */
struct IndexedBy(R, I)
    if (isIndexableBy!(R, I))
{
    alias Index = I;        /// indexing type
    mixin genOps!I;
    R _r;
    alias _r this; // TODO Use opDispatch instead; to override only opSlice and opIndex
}

/** Instantiator for $(D IndexedBy).
 */
auto indexedBy(I, R)(R range)
    if (isIndexableBy!(R, I))
{
    return IndexedBy!(R, I)(range);
}

struct IndexedBy(R, string I_ = "Index")
    if (isArray!R &&
        I_ != "I_") // prevent name lookup failure
{
    mixin(q{ struct } ~ I_ ~
          q{ {
                  alias T = size_t;
                  this(T ix) { this._ix = ix; }
                  T opCast(U : T)() const { return _ix; }
                  private T _ix = 0;
              }
          });
    mixin genOps!(mixin(I_));
    R _r;
    alias _r this; // TODO Use opDispatch instead; to override only opSlice and opIndex
}

/** Instantiator for $(D IndexedBy).
 */
auto indexedBy(string I, R)(R range)
    if (isArray!R &&
        I != "I_") // prevent name lookup failure
{
    return IndexedBy!(R, I)(range);
}

/** Instantiator for $(D IndexedBy).
 */
auto indexed(R)(R range)
    if (isArray!R)
{
    return IndexedBy!(R)(range);
}

@safe pure nothrow unittest
{
    int[3] x = [1, 2, 3];

    // sample index
    struct Index(T = size_t)
        if (isUnsigned!T)
    {
        this(T i) { this._i = i; }
        T opCast(U : T)() const { return _i; }
        private T _i = 0;
    }
    alias J = Index!size_t;

    enum E { e0, e1, e2 }

    with (E)
    {
        auto xb = x.indexedBy!ubyte;
        auto xi = x.indexedBy!uint;
        auto xj = x.indexedBy!J;
        auto xe = x.indexedBy!E;
        auto xf = x.indexed;

        auto xs = x.indexedBy!"I";
        alias XS = typeof(xs);
        XS xs_;

        // indexing with correct type
        xb[  0 ] = 11; assert(xb[  0 ] == 11);
        xi[  0 ] = 11; assert(xi[  0 ] == 11);
        xj[J(0)] = 11; assert(xj[J(0)] == 11);
        xe[ e0 ] = 11; assert(xe[ e0 ] == 11);
        xs[XS.I(0)] = 11; assert(xs[XS.I(0)] == 11);
        xs_[XS.I(0)] = 11; assert(xs_[XS.I(0)] == 11);

        // indexing with wrong type
        static assert(!__traits(compiles, { xb[J(0)] = 11; }));
        static assert(!__traits(compiles, { xi[J(0)] = 11; }));
        static assert(!__traits(compiles, { xj[  0 ] = 11; }));
        static assert(!__traits(compiles, { xe[  0 ] = 11; }));
        static assert(!__traits(compiles, { xs[  0 ] = 11; }));
        static assert(!__traits(compiles, { xs_[  0 ] = 11; }));

        import std.algorithm.comparison: equal;
        import std.algorithm.iteration: filter;

        assert(equal(xb[].filter!(a => a < 11), [2, 3]));
        assert(equal(xi[].filter!(a => a < 11), [2, 3]));
        assert(equal(xj[].filter!(a => a < 11), [2, 3]));
        assert(equal(xe[].filter!(a => a < 11), [2, 3]));
        assert(equal(xs[].filter!(a => a < 11), [2, 3]));
    }
}

@safe pure nothrow unittest
{
    auto x = [1, 2, 3];

    // sample index
    struct Index(T = size_t)
        if (isUnsigned!T)
    {
        this(T ix) { this._ix = ix; }
        T opCast(U : T)() const { return _ix; }
        private T _ix = 0;
    }
    alias J = Index!size_t;

    enum E { e0, e1, e2 }

    with (E)
    {
        auto xb = x.indexedBy!ubyte;
        auto xi = x.indexedBy!uint;
        auto xj = x.indexedBy!J;
        auto xe = x.indexedBy!E;

        // indexing with correct type
        xb[  0 ] = 11; assert(xb[  0 ] == 11);
        xi[  0 ] = 11; assert(xi[  0 ] == 11);
        xj[J(0)] = 11; assert(xj[J(0)] == 11);
        xe[ e0 ] = 11; assert(xe[ e0 ] == 11);

        // slicing with correct type
        xb[  0  ..   1 ] = 12; assert(xb[  0  ..   1 ] == [12]);
        xi[  0  ..   1 ] = 12; assert(xi[  0  ..   1 ] == [12]);
        xj[J(0) .. J(1)] = 12; assert(xj[J(0) .. J(1)] == [12]);
        xe[ e0  ..  e1 ] = 12; assert(xe[ e0  ..  e1 ] == [12]);

        // indexing with wrong type
        static assert(!__traits(compiles, { xb[J(0)] = 11; }));
        static assert(!__traits(compiles, { xi[J(0)] = 11; }));
        static assert(!__traits(compiles, { xj[  0 ] = 11; }));
        static assert(!__traits(compiles, { xe[  0 ] = 11; }));

        // slicing with wrong type
        static assert(!__traits(compiles, { xb[J(0) .. J(0)] = 11; }));
        static assert(!__traits(compiles, { xi[J(0) .. J(0)] = 11; }));
        static assert(!__traits(compiles, { xj[  0  ..   0 ] = 11; }));
        static assert(!__traits(compiles, { xe[  0  ..   0 ] = 11; }));

        import std.algorithm.comparison: equal;
        import std.algorithm.iteration: filter;

        assert(equal(xb.filter!(a => a < 11), [2, 3]));
        assert(equal(xi.filter!(a => a < 11), [2, 3]));
        assert(equal(xj.filter!(a => a < 11), [2, 3]));
        assert(equal(xe.filter!(a => a < 11), [2, 3]));
    }
}

@safe pure nothrow unittest
{
    auto x = [1, 2, 3];
    struct I(T = size_t)
    {
        this(T ix) { this._ix = ix; }
        T opCast(U : T)() const { return _ix; }
        private T _ix = 0;
    }
    alias J = I!size_t;
    auto xj = x.indexedBy!J;
}

@safe pure nothrow unittest
{
    auto x = [1, 2, 3];
    struct I(T = size_t)
    {
        this(T ix) { this._ix = ix; }
        private T _ix = 0;
    }
    alias J = I!size_t;
    static assert(!__traits(compiles, { auto xj = x.indexedBy!J; }));
}

@safe pure nothrow unittest
{
    auto x = [1, 2, 3];
    import bound: Bound;
    alias B = Bound!(ubyte, 0, 2);
    B b;
    auto c = cast(size_t)b;
    auto y = x.indexedBy!B;
}

/** TODO shorter name */
enum StaticArrayOfElementTypeIndexedBy(E, I) = IndexedBy!(E[I.elementCountOf!E], I);
