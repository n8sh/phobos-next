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

/** Instantiator for `Nullable`.
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

/** Instantiator for `Nullable`.
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

/** Instantiator for `NullableRef`.
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

import std.traits: isArray, isStaticArray, isUnsigned, isInstanceOf, isSomeString;
import std.range.primitives: hasSlicing;

/** Check if `T` is castable to `U`.
 */
enum isCastableTo(T, U) = __traits(compiles, { T i = 0; cast(U)i; });

enum isIndex(I) = (is(I == enum) ||
                   isUnsigned!I || // TODO should we allow isUnsigned here?
                   isCastableTo!(I, size_t));

/** Check if `R` is indexable by `I`. */
enum isIndexableBy(R, I) = (isArray!R && // TODO generalize to RandomAccessContainers. Ask on forum for hasIndexing!R.
                            isIndex!I);

unittest
{
    static assert(isIndexableBy!(int[3], ubyte));
}

/**
   Check if `R` is indexable by `I`.
 */
enum isIndexableBy(R, alias I) = (isArray!R && // TODO generalize to RandomAccessContainers. Ask on forum for hasIndexing!R.
                                  (isSomeString!(typeof(I))));

unittest
{
    static assert(isIndexableBy!(int[], "I"));
}

/** Generate `opIndex` and `opSlice`. */
mixin template genOps(I)
{
    auto ref at(size_t i)() inout
    {
        assert(cast(size_t)i < _r.length, "Index " ~ i ~ " must be smaller than array length" ~ _r.length.stringof);
        return _r[i];
    }
    auto ref opIndex(I i) inout
    {
        assert(cast(size_t)i < _r.length, "Range violation with index type " ~ I.stringof);
        return _r[cast(size_t)i];
    }
    auto ref opIndexAssign(V)(V value, I i)
    {
        assert(cast(size_t)i < _r.length, "Range violation with index type " ~ I.stringof);
        return _r[cast(size_t)i] = value;
    }
    static if (hasSlicing!R)
    {
        auto ref opSlice(I i, I j) inout             { return _r[cast(size_t)i ..
                                                                 cast(size_t)j]; }
        auto ref opSliceAssign(V)(V value, I i, I j) { return _r[cast(size_t)i ..
                                                                 cast(size_t)j] = value; }
    }
}

/** Generate `opIndex` and `opSlice`. */
mixin template genTrustedUncheckedOps(I)
{
    @trusted:
    auto ref at(size_t i)() inout
    {
        static assert(i < _r.length, "Index " ~ i.stringof ~ " must be smaller than array length " ~ _r.length.stringof);
        return _r.ptr[i];
    }
    auto ref opIndex(I i) inout
    {
        return _r.ptr[cast(size_t)i]; // safe to avoid range checking
    }
    auto ref opIndexAssign(V)(V value, I i)
    {
        return _r.ptr[cast(size_t)i] = value;
    }
    static if (hasSlicing!R)
    {
        auto ref opSlice(I i, I j) inout             { return _r.ptr[cast(size_t)i ..
                                                                     cast(size_t)j]; }
        auto ref opSliceAssign(V)(V value, I i, I j) { return _r.ptr[cast(size_t)i ..
                                                                     cast(size_t)j] = value; }
    }
}

/** Wrapper for `R` with Type-Safe `I`-Indexing.
    See also: http://forum.dlang.org/thread/gayfjaslyairnzrygbvh@forum.dlang.org#post-gayfjaslyairnzrygbvh:40forum.dlang.org

    TODO Merge with https://github.com/rcorre/enumap

    TODO Use std.range.indexed when I is an enum with non-contigious
    enumerators. Perhaps use among aswell.

    TODO Rename to something more concise such as [Bb]y.

    TODO Allow `I` to be a string and if so derive `Index` to be that string.

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

/** Static Array of ElementType `E` indexed by `I`.
    TODO assert that `I` is continuous if it is a `enum`.
*/
struct IndexedArray(E, I)
    if (isIndex!I)
{
    static assert(I.min == 0, "Index type I is currently limited to start at 0 and be continuous");
    alias Index = I;            /// indexing type
    mixin genOps!I;
    alias R = E[I.max + 1];     // needed by mixins
    R _r;                       // static array
    alias _r this; // TODO Use opDispatch instead; to override only opSlice and opIndex
}

///
@safe pure nothrow unittest
{
    enum N = 7;
    enum E { x = 0, y = 1, z = 2}
    alias A = IndexedArray!(size_t, E);
    static assert(A.sizeof == 3*size_t.sizeof);
    A x;
    x[E.x] = 1;
    x[E.y] = 2;
    x[E.z] = 3;
    static assert(!__traits(compiles, { x[1] = 3; })); // no integer indexing
}

/** Instantiator for `IndexedBy`.
 */
auto indexedBy(I, R)(R range)
    if (isIndexableBy!(R, I))
{
    return IndexedBy!(R, I)(range);
}

struct IndexedBy(R, string IndexTypeName)
    if (isArray!R &&
        IndexTypeName != "IndexTypeName") // prevent name lookup failure
{
    static if (isStaticArray!R) // if length is known at compile-time
    {
        import modulo : Mod;
        mixin(`alias ` ~ IndexTypeName ~ ` = Mod!(R.length);`);

        // dummy variable needed for symbol argument to `genTrustedUncheckedOps`
        mixin(`private static alias I__ = ` ~ IndexTypeName ~ `;`);

        mixin genTrustedUncheckedOps!(I__); // no range checking needed because I is always < R.length

        /** Get index of element `E` wrapped in a bool-convertable struct. */
        auto findIndex(E)(E e) @safe pure nothrow @nogc
        {
            static struct Result
            {
                Index index;    // index if exists is `true', 0 otherwise
                bool exists;  // `true` if `index` is defined, `false` otherwise
                bool opCast(T : bool)() const @safe @nogc pure nothrow { return exists; }
            }
            import std.algorithm : countUntil;
            const ix = _r[].countUntil(e); // is safe
            if (ix >= 0)
            {
                return Result(Index(ix), true);
            }
            return Result(Index(0), false);
        }
    }
    else
    {
        mixin(q{ struct } ~ IndexTypeName ~
              q{ {
                      alias T = size_t;
                      this(T ix) { this._ix = ix; }
                      T opCast(U : T)() const { return _ix; }
                      private T _ix = 0;
                  }
              });
        mixin genOps!(mixin(IndexTypeName));
    }
    R _r;
    alias _r this; // TODO Use opDispatch instead; to override only opSlice and opIndex
}

/** Construct wrapper type for `R' which is strictly indexed with type
    `R.Index`. */
template StrictlyIndexed(R)
    if (isArray!R) // prevent name lookup failure
{
    alias StrictlyIndexed = IndexedBy!(R, "Index");
}

/** Instantiator for `IndexedBy`.
 */
auto indexedBy(string I, R)(R range)
    if (isArray!R &&
        I != "IndexTypeName") // prevent name lookup failure
{
    return IndexedBy!(R, I)(range);
}

/** Instantiator for `StrictlyIndexed`.
 */
auto strictlyIndexed(R)(R range)
    if (isArray!R)
{
    return StrictlyIndexed!(R)(range);
}

///
@safe pure nothrow unittest
{
    enum m = 3;
    int[m] x = [11, 22, 33];
    auto y = x.strictlyIndexed;

    alias Y = typeof(y);

    static assert(is(typeof(y.findIndex(11).index) == Y.Index));

    assert(y.findIndex(11).exists);
    assert(y.findIndex(22).exists);
    assert(y.findIndex(33).exists);

    if (auto hit = y.findIndex(11)) { assert(hit.index == 0); } else { assert(false); }
    if (auto hit = y.findIndex(22)) { assert(hit.index == 1); } else { assert(false); }
    if (auto hit = y.findIndex(33)) { assert(hit.index == 2); } else { assert(false); }

    assert(!y.findIndex(44));
    assert(!y.findIndex(55));

    assert(!y.findIndex(44).exists);
    assert(!y.findIndex(55).exists);
}

///
@safe pure nothrow unittest
{
    enum N = 7;
    alias T = StrictlyIndexed!(size_t[N]); // static array
    static assert(T.sizeof == N*size_t.sizeof);
    import modulo : Mod, mod;

    T x;

    x[Mod!N(1)] = 11;
    x[1.mod!N] = 11;
    assert(x[1.mod!N] == 11);

    x.at!1 = 12;
    static assert(!__traits(compiles, { x.at!N; }));
    assert(x.at!1 == 12);
}

///
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
        auto xf = x.strictlyIndexed;

        auto xs = x.indexedBy!"I";
        alias XS = typeof(xs);
        XS xs_;

        // indexing with correct type
        xb[  0 ] = 11; assert(xb[  0 ] == 11);
        xi[  0 ] = 11; assert(xi[  0 ] == 11);
        xj[J(0)] = 11; assert(xj[J(0)] == 11);
        xe[ e0 ] = 11; assert(xe[ e0 ] == 11);

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
        // assert(equal(xs[].filter!(a => a < 11), [2, 3]));
    }
}

///
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

///
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

///
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

///
@safe pure nothrow unittest
{
    auto x = [1, 2, 3];
    import bound: Bound;
    alias B = Bound!(ubyte, 0, 2);
    B b;
    auto c = cast(size_t)b;
    auto y = x.indexedBy!B;
}

/** Returns: a `string` containing the definition of an `enum` named `name` and
    with enumerator names given by `Es`, optionally prepended with `prefix` and
    appended with `suffix`.

    TODO Move to Phobos std.typecons
*/
template makeEnumFromSymbolNames(string prefix = `_`,
                                 string suffix = ``,
                                 bool firstUndefined = true,
                                 Es...)
    if (Es.length >= 1)
{
    enum members =
    {
        string s = firstUndefined ? `undefined, ` : ``;
        foreach (E; Es)
        {
            static if (E.stringof[$ - 1] == '*') enum E_ = E.stringof[0 .. $ - 1] ~ "Ptr";
            else                                 enum E_ = E.stringof;
            s ~= prefix ~ E_ ~ suffix ~ `, `;
        }
        return s;
    }();
    mixin("enum makeEnumFromSymbolNames : ubyte {" ~ members ~ "}");
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    alias Types = AliasSeq!(byte, short, int*);
    alias Type = makeEnumFromSymbolNames!(`_`, `_`, true, Types);
    static assert(is(Type == enum));
    static assert(Type.undefined.stringof == `undefined`);
    static assert(Type._byte_.stringof == `_byte_`);
    static assert(Type._short_.stringof == `_short_`);
    static assert(Type._intPtr_.stringof == `_intPtr_`);
}
