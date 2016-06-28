/** Tries and Prefix Trees.

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree

    TODO Add Branch-hint allocation flag and re-benchmark construction of radixTreeSet with 10000000 uints

    TODO Replace toNode with to!Node

    TODO Add sortedness to `IxsN` and make `IxsN.contains()` use `binarySearch()`. Make use of `sortn`.

    TODO Make the GC aware of all Value scalars and arrays:
    static if (shouldAddGCRange!Value)
    {
    import core.memory : GC;
    GC.addRange(_values, length * Value.sizeof);
    }

    TODO Use variadic list of `Sub`s in constructors for SparseBranch and DenseBranch

    TODO Make `Key` and Ix[]-array of `immutable Ix` like `string`
    TODO Allow NodeType-constructors to take const and immutable prefixes

    TODO Check for case when expanding to bit-branch instead of SparseBranch in all `expand()` overloads

    TODO Make array indexing/slicing as @trusted and use .ptr[] instead of [] when things are stable. Especially in IxsN

    TODO Is std.algorithm.countUntil the most suitable function to use in setSub(SparseBranch*, ...)

    TODO Should opBinaryRight return void* instead of bool for set-case?

    TODO Add `struct Range`. Use same construct as in `containers-em/src/containers/ttree.d`.

    - Members:
    - Iterator front()
    - void popFront()
    - bool empty()
    - Iterator it; // Iterator is defined below
    - Reuse RefCounted reference to _root. Add checks with `isSorted`.

    Prefix:
    - `set.prefix("alpha")`                           => `Range` of `Tuple!(string, Lang, PoT, Sense)`.
    - `set.prefix(tuple("alpha"))`                    => `Range` of `Tuple!(Lang, PoT, Sense)`.
    - `set.prefix(tuple("alpha", Lang.en))`           => `Range` of `Tuple!(PoT, Sense)`.
    - `set.prefix(tuple("alpha", Lang.en, PoT.noun))` => `Range` of `Tuple!(Sense)`.

    Returns: a range of elements which are equivalent (though not necessarily equal) to value.
    auto equalRange(this This)(inout T value)

    Returns: a range of elements which are greater than low and smaller than highValue.
    auto bound(this This)(inout T lowValue, inout T highValue)

    Returns: a range of elements which are less than value.
    auto lowerBound(this This)(inout T value)

    Returns: a range of elements which are greater than value.
    auto upperBound(this This)(inout T value)
*/
module trie;

import std.traits : isIntegral, isFloatingPoint, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy, isPointer;
import std.typecons : tuple, Tuple, Unqual;
import std.range : isInputRange, ElementType;
import std.range.primitives : hasLength;

import bijections : isIntegralBijectableType, bijectToUnsigned;
import variant_ex : WordVariant;
import typecons_ex : IndexedArray, StrictlyIndexed;
import modulo : Mod, mod;

// version = enterSingleInfiniteMemoryLeakTest;
// version = debugAllocations;
version = benchmark;
version = print;

import dbg;

alias isFixedTrieableKeyType = isIntegralBijectableType;

enum isTrieableKeyType(T) = (isFixedTrieableKeyType!T ||
                             (isInputRange!T &&
                              isFixedTrieableKeyType!(ElementType!T)));

extern(C) pure nothrow @system @nogc
{
    void* malloc(size_t size);
    void* calloc(size_t nmemb, size_t size);
    void* realloc(void* ptr, size_t size);
    void free(void* ptr);
}

/** Mutable Raw Key. */
alias Key(size_t span) = Mod!(2^^span)[]; // TODO use bitset to more naturally support span != 8.
/** Immutable Raw Key. */
alias IKey(size_t span) = immutable(Mod!(2^^span))[]; // TODO use bitset to more naturally support span != 8.
/** Fixed-Length Raw Key. */
alias KeyN(size_t span, size_t N) = Mod!(2^^span)[N];

/** Size of a CPU cache line in bytes.

    Container layouts should be adapted to make use of at least this many bytes
    in its nodes.
*/
enum cacheLineSize = 64;

shared static this()
{
    import core.cpuid : dataCaches;
    assert(cacheLineSize == dataCaches()[0].lineSize, "Cache line is not 64 bytes");
}

enum keySeparator = ',';

/** Statically allocated `Ix`-array of fixed pre-allocated length `capacity` of
    Ix-elements in chunks of elementLength. `ElementType` is
    `Ix[elementLength]`.
*/
struct IxsN(uint capacity,
            uint elementLength = 1,
            uint span = 8)
    if (capacity*elementLength >= 2) // no use storing less than 2 bytes
    {
        enum L = elementLength;
        enum M = 2^^span;   // branch-multiplicity, typically either 2, 4, 16 or 256
        alias Ix = Mod!M;

        /// Element type `E`.
        static if (L == 1)
            alias E = Ix;
        else
            alias E = Ix[L];

        this(Es...)(Es ixs)
        if (Es.length >= 1 &&
            Es.length <= capacity)
        {
            foreach (const i, const ix; ixs)
            {
                static assert(!is(typeof(ix) == int));
                _ixs[i] = ix;
            }
            _length = ixs.length;
        }

        static if (L == 1)
        {
            this(const Ix[] ixs)
            {
                assert(ixs.length <= capacity);
                _ixs[0 .. ixs.length] = ixs;
                _length = ixs.length;
            }
        }

        @property auto toString() const
        {
            string s;
            foreach (const i, const ix; chunks)
            {
                if (i != 0) { s ~= keySeparator; } // separator
                import std.string : format;
                static if (elementLength == 1)
                {
                    s ~= format("%.2X", ix); // in hexadecimal
                }
                else
                {
                    foreach (const j, const subIx; ix[])
                    {
                        if (j != 0) { s ~= '_'; } // separator
                        s ~= format("%.2X", subIx); // in hexadecimal
                    }
                }
            }
            return s;
        }

        @safe pure nothrow @nogc:

        /** Get first element. */
        auto front() inout          // TODO should throw?
        {
            assert(!empty);
        return _ixs[0];
    }

    /** Get last element. */
    auto back() inout           // TODO should throw?
    {
        assert(!empty);
        return _ixs[_length - 1];
    }

    /** Returns: `true` if `this` is empty, `false` otherwise. */
    bool empty() const { return _length == 0; }

    /** Returns: `true` if `this` is full, `false` otherwise. */
    bool full() const { return _length == capacity; }

    /** Pop one front element. */
    auto ref popFront()
    {
        assert(!empty);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - 1)
        {
            _ixs[i] = _ixs[i + 1]; // TODO move construct?
        }
        _length = _length - 1;
        return this;
    }

    /** Pop `n` front elements. */
    auto ref popFrontN(size_t n)
    {
        assert(length >= n);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - n)
        {
            _ixs[i] = _ixs[i + n]; // TODO move construct?
        }
        _length = _length - n;
        return this;
    }

    /** Pop last element. */
    auto ref popBack()
    {
        assert(!empty);
        _length = _length - 1;
        return this;
    }

    /** Push/Add elements `moreEs` at back. */
    auto ref pushBack(Es...)(Es moreEs)
        if (Es.length <= capacity)
    {
        assert(length + Es.length <= capacity);
        foreach (const i, const ix; moreEs)
        {
            _ixs[_length + i] = ix;
        }
        _length = _length + Es.length;
        return this;
    }

    /** Returns: `true` if `key` is contained in `this`. */
    bool contains(const Ix[] key) const @nogc
    {
        import std.algorithm.searching : canFind;
        if (key.length != L) { return false; }
        return (chunks.canFind(key)); // TODO use binarySearch
    }
    static if (L == 1)
    {
        /** Returns: `true` if `ix` is contained in `this`. */
        bool contains(const Ix ix) const @nogc
        {
            import std.algorithm.searching : canFind;
            return (chunks.canFind(ix)); // TODO use binarySearch
        }
    }

    auto chunks() inout { return _ixs[0 .. _length]; }
    alias chunks this;

    /** Variant of `opIndex` with compile-time range checking. */
    auto ref at(uint ix)() inout @trusted
        if (ix < capacity)      // assert below memory allocation bound
    {
        assert(ix < _length);   // assert accessing initialized elements
        return _ixs.ptr[ix];
    }

    /** Get length. */
    auto length() const { return _length; }

private:
    static if (L == 1)
    {
        Ix[capacity] _ixs;     // byte indexes
    }
    else
    {
        Ix[L][capacity] _ixs;  // byte indexes
    }

    static if (_ixs.sizeof == 6)
    {
        ubyte _padding;
    }

    enum typeBits = 4;
    import std.bitmanip : bitfields;
    mixin(bitfields!(size_t, "_length", 4, // maximum length of 15
                     ubyte, "_mustBeIgnored", typeBits)); // must be here and ignored because it contains `WordVariant` type of `Node`
}

static assert(IxsN!(7, 1, 8).sizeof == 8);
static assert(IxsN!(3, 2, 8).sizeof == 8);
static assert(IxsN!(2, 3, 8).sizeof == 8);

///
@safe pure nothrow unittest
{
    import std.algorithm : equal;
    import modulo : mod;

    enum span = 8;
    enum M = 2^^span;

    alias Ix = Mod!(M, ubyte);
    Ix[] ixs = [11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M];
    enum capacity = 7;

    auto x = IxsN!(capacity, 1)(ixs);
    auto y = IxsN!(capacity, 1)(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M);

    assert(x == y);

    assert(x.length == 4);
    assert(!x.empty);

    assert(x.equal([11, 22, 33, 44]));
    assert(x.front == 11);
    assert(x.back == 44);
    assert(!x.full);
    x.popFront;
    assert(x.equal([22, 33, 44]));
    assert(x.front == 22);
    assert(x.back == 44);
    assert(!x.full);
    x.popBack;
    assert(x.equal([22, 33]));
    assert(x.front == 22);
    assert(x.back == 33);
    assert(!x.full);
    x.popFront;
    assert(x.equal([33]));
    assert(x.front == 33);
    assert(x.back == 33);
    assert(!x.full);
    x.popFront;
    assert(x.empty);
    assert(!x.full);
    assert(x.length == 0);

    x.pushBack(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M, 55.mod!M, 66.mod!M, 77.mod!M);
    assert(x.equal([11, 22, 33, 44, 55, 66, 77]));
    assert(!x.empty);
    assert(x.full);
}

/** Returns: `true` if `r` and all `ss` all have equal length.
 */
bool equalLength(R, Ss...)(const R r, const Ss ss) @safe pure nothrow @nogc
    if (Ss.length >= 1 &&
        allSatisfy!(hasLength, R, Ss))
{
    foreach (const ref s; ss)
    {
        if (r.length != s.length) { return false; }
    }
    return true;
}

///
@safe pure nothrow unittest
{
    assert(equalLength([1], [2], [3]));
    assert(!equalLength([1, 1], [2], [3]));
    assert(!equalLength([1], [2, 2], [3]));
    assert(!equalLength([1], [2], [3, 3]));
}

/// Binary power of radix, typically either 1, 2, 4 or 8.
private enum span = 8;
private enum radix = 2^^span; // branch-multiplicity, typically either 2, 4, 16 or 256
alias order = radix;          // tree order

static assert(span == 8, "Radix is currently limited to 8");
static assert(size_t.sizeof == 8, "Currently requires a 64-bit CPU (size_t.sizeof == 8)");

/** Radix Modulo Index */
alias Ix = Mod!radix; // restricted index type avoids range checking in array indexing below

/// Single/1-Key Leaf with maximum key-length 7.
struct OneLeafMax7
{
    @safe pure:
    enum capacity = 7;

    this(Ix[] key) nothrow @nogc
    {
        assert(key.length != 0);
        this.key = key;
    }

    pragma(inline) bool contains(Key!span key) const nothrow @nogc { return this.key == key; }

    @property string toString() const @safe pure
    {
        import std.string : format;
        string s;
        foreach (const i, const ix; key)
        {
            const first = i == 0; // first iteration
            if (!first) { s ~= '_'; }
            s ~= format("%.2X", ix); // in hexadecimal
        }
        return s;
    }

    IxsN!(capacity, 1) key;
}

/// Binary/2-Key Leaf with key-length 3.
struct TwoLeaf3
{
    enum keyLength = 3; // fixed length key
    enum capacity = 2; // maximum number of keys stored

    @safe pure:

    this(Keys...)(Keys keys)
        if (Keys.length >= 1 &&
            Keys.length <= capacity)
    {
        this.keys = keys;
    }

    inout(Ix)[] prefix() inout nothrow
    {
        final switch (keys.length)
        {
        case 1:
            return keys.at!0[];
        case 2:
            import std.algorithm : commonPrefix;
            return commonPrefix(keys.at!0[], keys.at!1[]);
        }
    }

    pragma(inline) bool contains(Key!span key) const nothrow @nogc { return keys.contains(key); }

    IxsN!(capacity, keyLength) keys;
}

/// Ternary/3-Key Leaf with key-length 2.
struct TriLeaf2
{
    enum keyLength = 2; // fixed length key
    enum capacity = 3; // maximum number of keys stored

    @safe pure:

    this(Keys...)(Keys keys)
        if (Keys.length >= 1 &&
            Keys.length <= capacity)
    {
        this.keys = keys;
    }

    inout(Ix)[] prefix() inout nothrow
    {
        final switch (keys.length)
        {
        case 1:
            return keys.at!0[];
        case 2:
            import std.algorithm : commonPrefix;
            return commonPrefix(keys.at!0[], keys.at!1[]);
        case 3:
            import std.algorithm : commonPrefix;
            return commonPrefix(keys.at!0[],
                                commonPrefix(keys.at!1[], keys.at!2[])); // TODO make and reuse variadic commonPrefix
        }
    }

    pragma(inline) bool contains(Key!span key) const nothrow @nogc { return keys.contains(key); }

    IxsN!(capacity, keyLength) keys;
}

/// Hepa/7-Key Leaf with key-length 1.
struct HeptLeaf1
{
    enum keyLength = 1;
    enum capacity = 7; // maximum number of elements

    @safe pure:

    this(Keys...)(Keys keys)
        if (Keys.length >= 1 &&
            Keys.length <= capacity)
    {
        this.keys = keys;
    }

    pragma(inline) bool contains(Ix key) const nothrow @nogc { return keys.contains(key); }
    pragma(inline) bool contains(Key!span key) const nothrow @nogc { return key.length == 1 && keys.contains(key[0]); }

    IxsN!(capacity, 1) keys;
}

/** Sparsely coded leaves with values of type `Value`. */
static private struct SparseLeaf1(Value)
{
    enum hasValue = !is(Value == void);
    alias Length = Mod!(radix + 1);
    alias Capacity = Mod!(radix + 1);

    /// Constructor Parameter Element type `E`.
    static if (hasValue) { alias E = Tuple!(Ix, Value); }
    else                 { alias E = Ix; }

    pure nothrow /* TODO @nogc */:

    this(Ix[] es...) // @nogc
    @trusted
    {
        assert(es.length <= radix);

        if (es.length != 0)
        {
            import std.math : nextPow2;

            _length = es.length;
            _capacity = _length == 1 ? 1 : nextPow2(_length - 1);
            assert(_capacity >= _length);

            // allocate
            _keys = cast(typeof(_keys))malloc(_capacity*Ix.sizeof);
            static if (hasValue)
            {
                _values = cast(typeof(_values))malloc(_capacity*Value.sizeof);
            }

            // initialize
            foreach (const i, const e; es)
            {
                static if (hasValue)
                {
                    _keys[i] = e;
                    // _keys[i] = e[0];
                    // _values[i] = e[1];
                }
                else
                {
                    _keys[i] = e;
                }
            }
        }
    }

    ~this() @trusted
    {
        free(_keys); debug _keys = null;
        static if (hasValue)
        {
            free(_values); debug _values = null;
        }
    }

    /** Insert `key` in linear time. */
    bool linearInsert(Ix key) @trusted /* TODO @nogc */
    {
        if (!contains(key))
        {
            reserve(Capacity(length + 1));
            _keys[length] = key;
            ++_length;
            return true;
        }
        return false;
    }

    void pushBack(Ix key) @trusted /* TODO @nogc */
    {
        reserve(Capacity(length + 1));
        _keys[length] = key;
        ++_length;
    }

    pragma(inline) Length length() const @safe @nogc { return _length; }
    pragma(inline) Capacity capacity() const @safe @nogc { return _capacity; }

    /** Reserve room for `newSubCapacity` number of elements. */
    void reserve(Capacity newSubCapacity) @trusted // TODO @nogc
    {
        assert(!full);
        if (_capacity < newSubCapacity)
        {
            import std.math : nextPow2;

            _capacity = nextPow2(newSubCapacity - 1); // need minus one here
            assert(_capacity >= newSubCapacity);
            _keys = cast(typeof(_keys))realloc(_keys, _capacity*Ix.sizeof);
            static if (hasValue)
            {
                _values = cast(typeof(_values))realloc(_values, _capacity*Value.sizeof);
            }
        }
    }

    pragma(inline) bool empty() const @safe @nogc { return _length == 0; }
    pragma(inline) bool full() const @safe @nogc { return _length == radix; }

    pragma(inline) bool contains(Ix key) const @trusted @nogc
    {
        import std.algorithm.searching : canFind;
        return (_keys[0 .. _length].canFind(key)); // TODO binarySearch
    }

    pragma(inline) auto ref keys() inout @trusted @nogc
    {
        return _keys[0 .. _length];
    }
    static if (hasValue)
    {
        pragma(inline) auto ref values() inout @trusted @nogc
        {
            return _values[0 .. _length];
        }

        pragma(inline) bool setValue(Ix key, in Value value) @trusted @nogc
        {
            import std.algorithm.searching : find;
            const hit = (_keys[0 .. _length].find(key)); // TODO binarySearch
            if (hit.length != 0)
            {
                _values[hit.ptr - _keys] = value;
                return true;
            }
            return false;
        }
    }

private:
    Length _length;
    Capacity _capacity;
    Ix* _keys;
    Value* _values;
}

/** Densely coded leaves with values of type `Value`. */
static private struct DenseLeaf1(Value)
{
    enum hasValue = !is(Value == void);
    enum maxCount = radix;

    @safe pure nothrow:

    this(Ixs...)(Ixs subIxs)
    if (Ixs.length <= maxCount)
    {
        foreach (subIx; subIxs)
        {
            _keyBits[subIx] = true;
        }
    }

    @nogc:

    pragma(inline) bool hasSubAt(Ix ix) const { return _keyBits[ix]; }
    pragma(inline) bool empty() const { return _keyBits.empty; }
    pragma(inline) bool full() const { return _keyBits.full; }
    pragma(inline) size_t count() const { return _keyBits.countOnes; }

    pragma(inline) bool contains(Ix key) const { return _keyBits[key]; }
    pragma(inline) bool insert(Ix key)
    {
        if (!contains(key)) { return _keyBits[key] = true; }
        return false;
    }

    static if (hasValue)
    {
        /// Get value at index `ix`.
        auto ref getValue(Ix ix) inout { return _values[ix]; }
        /// Set value at index `ix` to `value`.
        void setValue(Ix ix, in Value value) { _values[ix] = value; }
    }

private:
    import bitset : BitSet;
    BitSet!radix _keyBits;  // 32 bytes
    static if (hasValue)
    {
        static if (is(Value == bool))
        {
            BitSet!radix _values; // packed values
        }
        else
        {
            Value[radix] _values;
        }
    }
}

static assert((DenseLeaf1!void).sizeof == 32);

/** Raw adaptive radix tree (ART) container storing untyped variable-length `Key`.

    In set-case (`Value` is `void`) this container is especially suitable for
    representing a set of 32 or 64 integers/pointers.

    Radix-trees are suitable for storing variable-keys and provide completion of
    all keys matching a given key prefix. This enables efficient storage of long
    URLs sharing a common prefix, typically a domain and path.

    Branch packing of leaves is more efficiently when `Key.sizeof` is fixed,
    that is `hasFixedKeyLength` returns `true`.

    For optimal performance, the individual bit-chunks should be arranged
    starting with most sparse chunks first. For integers this means most
    significant chunk (byte) first.

    For a good introduction to adaptive radix trees (ART) see also:
    https://infosys.cs.uni-saarland.de/publications/ARCD15.pdf

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree
    See also: https://github.com/npgall/concurrent-trees
    See also: http://code.dogmap.org/kart/
    See also: http://cr.yp.to/critbit.html
    See also: https://gcc.gnu.org/onlinedocs/libstdc++/ext/pb_ds/trie_based_containers.html
    See also: https://github.com/npgall/concurrent-trees
*/
struct RawRadixTree(Value = void)
{
    alias ValueType = Value;

    import std.bitmanip : bitfields;
    import std.conv : to;
    import std.algorithm : filter;
    import std.meta : AliasSeq, staticMap;
    import std.typecons : ConstOf;
    import bitset : BitSet;

    /** Is `true` if this tree stores values of type `Value` along with keys. In
        other words: `this` is a $(I map) rather than a $(I set).
    */
    enum hasValue = !is(Value == void);

    /// `true` if tree has binary branch.
    enum isBinary = span == 2;

    // TODO make these run-time arguments at different key depths and map to statistics of typed-key
    alias DefaultBranch = SparseBranch*; // either SparseBranch*, DenseBranch*
    alias DefaultLeaf = SparseLeaf1!Value*; // either SparseLeaf1*, DenseLeaf1*

    /** Leaf Pointer node. */
    alias LeafPtrNode = WordVariant!(DenseLeaf1!Value*,
                                     SparseLeaf1!Value*);
    static assert(LeafPtrNode.typeBits <= IxsN!(7, 1, 8).typeBits);

    /** Mutable leaf node of 1-Ix leaves. */
    alias Leaf = WordVariant!(HeptLeaf1,
                              SparseLeaf1!Value*,
                              DenseLeaf1!Value*);
    static assert(Leaf.typeBits <= IxsN!(7, 1, 8).typeBits);

    /** Mutable node. */
    alias Node = WordVariant!(OneLeafMax7,
                              TwoLeaf3,
                              TriLeaf2,
                              HeptLeaf1,
                              DenseBranch*,
                              SparseBranch*,
                              DenseLeaf1!Value*,
                              SparseLeaf1!Value*);
    static assert(Node.typeBits <= IxsN!(7, 1, 8).typeBits);

    /** Constant node. */
    // TODO make work with indexNaming
    // alias ConstNodePtr = WordVariant!(staticMap!(ConstOf, Node));

    static assert(span <= 8*Ix.sizeof, "Need more precision in Ix");

    /** Element (`Key` plus optional `Value`) Reference. */
    struct ElementRef
    {
        Node node;
        Ix ix;
    }

    /** Tree Iterator. */
    alias Iterator = ElementRef[];

    /** Tree Range. Is `isBidirectionalRange`. */
    struct Range
    {
        Iterator front;
        Iterator back;
    }

    /** Sparse-Branch population histogram.
    */
    alias SparseLeaf1_PopHist = size_t[radix];

    /** 256-Branch population histogram.
     */
    alias DenseLeaf1_PopHist = size_t[radix];

    /** 4-Branch population histogram.
        Index maps to population with value range (0 .. 4).
    */
    alias SparseBranch_PopHist = size_t[SparseBranch.subCapacityMax + 1];

    /** radix-Branch population histogram.
        Index maps to population with value range (0 .. `radix`).
    */
    alias DenseBranch_PopHist = size_t[radix + 1];

    /** Tree Population and Memory-Usage Statistics. */
    struct Stats
    {
        SparseLeaf1_PopHist popHist_SparseLeaf1;
        DenseLeaf1_PopHist popHist_DenseLeaf1;
        SparseBranch_PopHist popHist_SparseBranch; // packed branch population histogram
        DenseBranch_PopHist popHist_DenseBranch; // full branch population histogram

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));

        IndexedArray!(size_t, Leaf.Ix) popByLeafType;
        static assert(is(typeof(popByLeafType).Index == Leaf.Ix));

        /// Number of heap-allocated `Node`s. Should always equal `heapNodeAllocationBalance`.
        size_t heapNodeCount;

        /// Number of heap-allocated `Leaf`s. Should always equal `heapLeafAllocationBalance`.
        size_t heapLeafCount;

        size_t sparseBranchSizeSum;
    }

    /** Sparse/Packed/Partial dynamically sized branch implemented as
        variable-length struct.
    */
    static private struct SparseBranch
    {
        import std.algorithm.sorting : isSorted;

        enum subCapacityMin = 0; // minimum number of preallocated sub-indexes and sub-nodes
        enum subCapacityMax = 48; // maximum number of preallocated sub-indexes and sub-nodes
        enum prefixCapacity = 5; // 5, 13, 21, ...

        alias Count = Mod!(subCapacityMax + 1);

        @safe pure nothrow:

        /// Element type `Sub`.
        alias Sub = Tuple!(Ix, Node);

        this(size_t subCapacity)
        {
            initialize(subCapacity);
        }

        this(size_t subCapacity, const Ix[] prefix, Leaf leaf)
        {
            initialize(subCapacity);
            this.prefix = prefix;
            this.leaf = leaf;
        }

        this(size_t subCapacity, const Ix[] prefix)
        {
            initialize(subCapacity);
            this.prefix = prefix;
        }

        this(size_t subCapacity, Leaf leaf)
        {
            initialize(subCapacity);
            this.leaf = leaf;
        }

        this(size_t subCapacity, const Ix[] prefix,
             Ix subIx, Node subNode)
        {
            assert(subCapacity >= 1);

            initialize(subCapacity);

            this.prefix = prefix;
            this.subCount = 1;
            this.subIxSlots[0] = subIx;
            this.subNodeSlots[0] = subNode;
        }

        this(size_t subCapacity, const typeof(this)* rhs)
        {
            assert(subCapacity > rhs.subCapacity);

            assert(rhs);

            // these two must be in this order:
            this = *rhs;             // 1. copy basic stuff
            initialize(subCapacity); // 2. copy rest

            // copy variable length part. TODO optimize:
            this.subIxSlots[0 .. rhs.subCount] = rhs.subIxSlots[0 .. rhs.subCount];
            this.subNodeSlots[0 .. rhs.subCount] = rhs.subNodeSlots[0 .. rhs.subCount];

            assert(this.subCapacity > rhs.subCapacity);
        }

        private void initialize(size_t subCapacity)
        {
            this.subCapacity = subCapacity;
            debug
            {
                // zero-initialize variable-length part
                subIxSlots = Ix.init;
                subNodeSlots = Node.init;
            }
        }

        void linearInsert(Sub sub) // TODO Rename this to logInsert aswell as in array_ex.
        out
        {
            // assert(subIxs.isSorted);
        }
        body
        {
            assert(!full);
            if (empty)
            {
                subIxSlots[0] = sub[0];
                subNodeSlots[0] = sub[1];
                subCount = 1;
            }
            else
            {
                auto hit = subIxs.upperBound(sub[0]); // find index where insertion should be made
                if (hit.length) // we need to insert
                {
                    const ix = &hit[0] - subIxSlots.ptr; // insertion index. TODO this is kind of ugly. Why doesn't hit.ptr work?
                    // TODO functionize this loop or reuse memmove:
                    foreach (i; 0 .. subCount - ix)
                    {
                        const iD = subCount - i;
                        const iS = iD - 1;
                        // dln("i:", i, " Moving from ", iS, " to ", iD);
                        subIxSlots[iD] = subIxSlots[iS];
                        subNodeSlots[iD] = subNodeSlots[iS];
                    }
                    subIxSlots[ix] = sub[0];
                    subNodeSlots[ix] = sub[1];
                }
                else            // insert at the end
                {
                    subIxSlots[subCount] = sub[0];
                    subNodeSlots[subCount] = sub[1];
                }
                ++subCount;
            }
        }

        inout(Node) containsSub(Ix ix) inout
        in
        {
            // assert(subIxSlots[0 .. subCount].isSorted);
        }
        body
        {
            import searching_ex : binarySearch; // need this instead of `SortedRange.contains` because we need the index
            const hitIndex = subIxSlots[0 .. subCount].binarySearch(ix); // find index where insertion should be made
            return (hitIndex != typeof(hitIndex).max) ? subNodeSlots[hitIndex] : Node.init;
        }

        pragma(inline) bool empty() const @nogc { return subCount == 0; }
        pragma(inline) bool full()  const @nogc { return subCount == subCapacity; }

        pragma(inline) auto ref subIxs()   inout @nogc
        {
            import std.algorithm.sorting : assumeSorted;
            return subIxSlots[0 .. subCount].assumeSorted;
        }

        pragma(inline) auto ref subNodes() inout @nogc { return subNodeSlots[0 .. subCount]; }

        /** Get all sub-`Ix` slots, possible both defined and undefined. */
        auto ref subIxSlots() inout @trusted pure nothrow
        {
            return (cast(Ix*)(cast(void*)_subNodeSlots.ptr + subCapacity * Node.sizeof))[0 .. subCapacity];
        }
        /** Get all sub-`Node` slots, possible both defined and undefined. */
        auto ref subNodeSlots() inout @trusted pure nothrow
        {
            return _subNodeSlots.ptr[0 .. subCapacity];
        }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) const
        {
            size_t count = 0; // number of non-zero sub-nodes
            foreach (const sub; subNodes)
            {
                ++count;
                sub.calculate!(Value)(stats);
            }
            assert(count <= radix);
            ++stats.popHist_SparseBranch[count]; // TODO type-safe indexing

            stats.sparseBranchSizeSum += allocatedSize;

            if (leaf)
            {
                leaf.calculate!(Value)(stats);
            }
        }

        /** Get allocation size (in bytes) needed to hold `length` number of
            sub-indexes and sub-nodes. */
        static size_t allocationSize(size_t subCapacity) @safe pure nothrow @nogc
        {
            return (this.sizeof + // base plus
                    Node.sizeof*subCapacity + // actual size of `_subNodeSlots`
                    Ix.sizeof*subCapacity);   // actual size of `_subIxSlots`
        }

        /** Get allocated size (in bytes) of `this` including the variable-length part. */
        size_t allocatedSize() const @safe pure nothrow @nogc
        {
            return allocationSize(subCapacity);
        }

    private:

        // members in order of decreasing `alignof`:
        Leaf leaf;
        static assert(leaf.alignof == 8); // assert alignment

        IxsN!prefixCapacity prefix; // prefix common to all `subNodes` (also called edge-label)
        Count subCount;
        Count subCapacity;
        static assert(prefix.sizeof + subCount.sizeof + subCapacity.sizeof == 8); // assert alignment

        // variable-length part
        Node[0] _subNodeSlots;
        Ix[0] _subIxSlots;
    }

    static if (!hasValue) static assert(SparseBranch.sizeof == 16);

    /** Dense/Unpacked `radix`-branch with `radix` number of sub-nodes. */
    static private struct DenseBranch
    {
        enum subCapacityMax = 256;
        enum prefixCapacity = 15; // 7, 15, 23, ..., we can afford larger prefix here because DenseBranch is so large

        @safe pure nothrow:

        this(const Ix[] prefix)
        {
            this.prefix = prefix;
        }

        this(const Ix[] prefix, Ix subIx, Node subNode)
        {
            this(prefix);
            this.subNodes[subIx] = subNode;
        }

        this(const Ix[] prefix,
             Ix subIx0, Node subNode0,
             Ix subIx1, Node subNode1)
        {
            assert(subIx0 != subIx1);
            assert(subNode0 != subNode1);

            this.subNodes[subIx0] = subNode0;
            this.subNodes[subIx1] = subNode1;
        }

        this(SparseBranch* rhs)
        {
            this.prefix = rhs.prefix;

            // move leaf
            this.leaf = rhs.leaf;
            debug rhs.leaf = null; // make reference unique, to be on the safe side

            foreach (const i; 0 .. rhs.subCount) // each sub node. TODO use iota!(Mod!N)
            {
                const iN = (cast(ubyte)i).mod!(SparseBranch.subCapacityMax);
                const subIx = rhs.subIxSlots[iN];
                this.subNodes[subIx] = rhs.subNodes[iN];
                debug rhs.subNodes[iN] = null; // make reference unique, to be on the safe side
            }
        }

        /// Number of non-null sub-Nodes.
        Mod!(radix + 1) subCount() const
        {
            typeof(return) count = 0; // number of non-zero sub-nodes
            foreach (const subNode; subNodes) // TODO why can't we use std.algorithm.count here?
            {
                if (subNode) { ++count; }
            }
            assert(count <= radix);
            return count;
        }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats)  /* TODO @nogc */ const
        {
            size_t count = 0; // number of non-zero sub-nodes
            foreach (const subNode; subNodes)
            {
                if (subNode)
                {
                    ++count;
                    subNode.calculate!(Value)(stats);
                }
            }
            assert(count <= radix);
            ++stats.popHist_DenseBranch[count]; // TODO type-safe indexing

            if (leaf)
            {
                leaf.calculate!(Value)(stats);
            }
        }

    private:
        // members in order of decreasing `alignof`:
        Leaf leaf;
        IxsN!prefixCapacity prefix; // prefix (edge-label) common to all `subNodes`
        StrictlyIndexed!(Node[radix]) subNodes;
    }

    static if (false)
    {
        pragma(msg, "SparseBranch.sizeof:", SparseBranch.sizeof, " SparseBranch.alignof:", SparseBranch.alignof);
        pragma(msg, "SparseBranch.subNodes.sizeof:", SparseBranch.subNodes.sizeof, " SparseBranch.subNodes.alignof:", SparseBranch.subNodes.alignof);
        pragma(msg, "SparseBranch.prefix.sizeof:", SparseBranch.prefix.sizeof, " SparseBranch.prefix.alignof:", SparseBranch.prefix.alignof);
        pragma(msg, "SparseBranch.subIxs.sizeof:", SparseBranch.subIxs.sizeof, " SparseBranch.subIxs.alignof:", SparseBranch.subIxs.alignof);
    }

    /** Set sub-`Node` of branch `Node curr` at index `ix` to `subNode`. */
    pragma(inline) Node setSub(Node curr, Ix subIx, Node subNode) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SparseBranchPtr: return setSub(curr.as!(SparseBranch*), subIx, subNode);
        case Node.Ix.ix_DenseBranchPtr: return setSub(curr.as!(DenseBranch*), subIx, subNode);
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }
    /// ditto
    Node setSub(SparseBranch* curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        import std.algorithm : countUntil;
        const ix = curr.subIxs.countUntil(subIx);
        if (ix >= 0)            // if `subIx` already stored in `curr.subIxs` at offset `ix`
        {
            curr.subNodes[ix] = subNode; // reuse
        }
        else if (!curr.full)     // if room left in curr
        {
            curr.linearInsert(tuple(subIx, subNode)); // add one to existing
        }
        else                    // if no room left in curr we need to expand
        {
            debug const previousHeapNodeAllocationBalance = heapNodeAllocationBalance;

            // curr is full
            Node next;
            if (curr.empty)     // if curr also empty length capacity must be zero
            {
                next = constructWithCapacity!(SparseBranch*)(1, curr); // so allocate one
            }
            else if (curr.subCapacity < DefaultBranch.subCapacityMax) // if we can expand to curr
            {
                import std.math : nextPow2;
                import std.algorithm : min;
                const nextSubCapacity = min(nextPow2(cast(uint)curr.subCapacity),
                                            DefaultBranch.subCapacityMax);
                assert(nextSubCapacity > curr.subCapacity);
                next = constructWithCapacity!(SparseBranch*)(nextSubCapacity, curr);
            }
            else
            {
                next = construct!(DenseBranch*)(curr);
            }
            freeNode(curr);
            assert(!getSub(next, subIx)); // key slot should be free

            debug assert(previousHeapNodeAllocationBalance == heapNodeAllocationBalance);
            return setSub(next, subIx, subNode); // fast, because directly calls setSub(DenseBranch*, ...)
        }
        return Node(curr);
    }
    /// ditto
    pragma(inline) Node setSub(DenseBranch* curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        try
        {
            debug assert(!curr.subNodes[subIx],
                         "sub-Node at index " ~ subIx.to!string ~
                         " already set to " ~ subNode.to!string);
        }
        catch (Exception e) {}
        curr.subNodes[subIx] = subNode;
        return Node(curr);
    }

    /** Get sub-`Node` of branch `Node curr` at index `subIx`. */
    pragma(inline) Node getSub(Node curr, Ix subIx) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SparseBranchPtr: return getSub(curr.as!(SparseBranch*), subIx);
        case Node.Ix.ix_DenseBranchPtr: return getSub(curr.as!(DenseBranch*), subIx);
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }
    /// ditto
    pragma(inline) Node getSub(SparseBranch* curr, Ix subIx) @safe pure nothrow
    {
        if (auto subNode = curr.containsSub(subIx))
        {
            return subNode;
        }
        return Node.init;
    }
    /// ditto
    pragma(inline) Node getSub(DenseBranch* curr, Ix subIx) @safe pure nothrow
    {
        auto sub = curr.subNodes[subIx];
        curr.subNodes[subIx] = Node.init; // zero it to prevent multiple references
        return sub;
    }

    /** Get leaves of node `curr`. */
    pragma(inline) inout(Leaf) getLeaf(inout Node curr) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SparseBranchPtr: return curr.as!(SparseBranch*).leaf;
        case Node.Ix.ix_DenseBranchPtr: return curr.as!(DenseBranch*).leaf;
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Set leaves node of node `curr` to `leaf`. */
    pragma(inline) Node setLeaf(Node curr, Leaf leaf) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SparseBranchPtr: curr.as!(SparseBranch*).leaf = leaf; return curr;
        case Node.Ix.ix_DenseBranchPtr: curr.as!(DenseBranch*).leaf = leaf; return curr;
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Get prefix of node `curr`. */
    pragma(inline) auto getPrefix(inout Node curr) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SparseBranchPtr: return curr.as!(SparseBranch*).prefix[];
        case Node.Ix.ix_DenseBranchPtr: return curr.as!(DenseBranch*).prefix[];
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Set prefix of branch node `curr` to `prefix`. */
    pragma(inline) void setPrefix(Node curr, const Ix[] prefix) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SparseBranchPtr: curr.as!(SparseBranch*).prefix = typeof(curr.as!(SparseBranch*).prefix)(prefix); break;
        case Node.Ix.ix_DenseBranchPtr: curr.as!(DenseBranch*).prefix = typeof(curr.as!(DenseBranch*).prefix)(prefix); break;
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Pop `n`  from prefix. */
    pragma(inline) void popFrontNPrefix(Node curr, size_t n) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SparseBranchPtr: curr.as!(SparseBranch*).prefix.popFrontN(n); break;
        case Node.Ix.ix_DenseBranchPtr: curr.as!(DenseBranch*).prefix.popFrontN(n); break;
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    Stats usageHistograms() const
    {
        typeof(return) stats;
        _root.calculate!(Value)(stats);
        return stats;
    }

    this(this)
    {
        if (!_root) return;
        auto rhsRoot = _root;
        debug const oldLength = _length;
        if (rhsRoot)
        {
            _root = null;       // reset
            _length = 0;        // needs reset because insert updates
            // TODO insert(rhsRoot[]);
        }
        assert(false, "TODO calculate tree by branches and leafs and make copies of them");
    }

    ~this()
    {
        if (_root) { release(_root); }
        debug
        {
            try
            {
                if (_heapNodeAllocationBalance != 0)
                {
                    dln("warning: Memory leak, heap Node allocation balance is not zero, but " ~
                        _heapNodeAllocationBalance.to!string ~
                        ", nodeCountsByIx is " ~ nodeCountsByIx.to!string);
                }
            }
            catch (Exception e) {}
        }
    }

    const @safe pure nothrow /* TODO @nogc */
    {
        /** Returns: `true` if `key` is stored, `false` otherwise. */
        pragma(inline) bool contains(Key!span key)
        {
            return containsAt(_root, key);
        }

        /** Returns: `true` if `key` is stored under `curr`, `false` otherwise. */
        pragma(inline) bool containsAt(Leaf curr, Key!span key)
        {
            final switch (curr.typeIx) with (Leaf.Ix)
            {
            case undefined: return false;
            case ix_HeptLeaf1: return curr.as!(HeptLeaf1).contains(key);
            case ix_SparseLeaf1Ptr: return key.length == 1 && curr.as!(SparseLeaf1!Value*).contains(key[0]);
            case ix_DenseLeaf1Ptr:  return key.length == 1 && curr.as!(DenseLeaf1!Value*).contains(key[0]);
            }
        }
        /// ditto
        pragma(inline) bool containsAt(Node curr, Key!span key)
        {
            import std.algorithm : skipOver;
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: return false;
            case ix_OneLeafMax7: return curr.as!(OneLeafMax7).contains(key);
            case ix_TwoLeaf3: return curr.as!(TwoLeaf3).contains(key);
            case ix_TriLeaf2: return curr.as!(TriLeaf2).contains(key);
            case ix_HeptLeaf1: return curr.as!(HeptLeaf1).contains(key);
            case ix_SparseLeaf1Ptr: return key.length == 1 && curr.as!(SparseLeaf1!Value*).contains(key[0]);
            case ix_DenseLeaf1Ptr:  return key.length == 1 && curr.as!(DenseLeaf1!Value*).contains(key[0]);
            case ix_SparseBranchPtr:
                auto curr_ = curr.as!(SparseBranch*);
                return (key.skipOver(curr_.prefix) &&        // matching prefix
                        ((key.length == 1 && containsAt(curr_.leaf, key)) || // either in leaf
                         (key.length >= 1 && containsAt(curr_.containsSub(key[0]), key[1 .. $])))); // or recurse
            case ix_DenseBranchPtr:
                auto curr_ = curr.as!(DenseBranch*);
                return (key.skipOver(curr_.prefix) &&        // matching prefix
                        ((key.length == 1 && containsAt(curr_.leaf, key)) || // either in leaf
                         (key.length >= 1 && containsAt(curr_.subNodes[key[0]], key[1 .. $])))); // recurse
            }
        }

        pragma(inline) size_t countHeapNodes()
        {
            return countHeapNodesAt(_root);
        }

        pragma(inline) size_t countHeapNodesAt(Node curr)
        {
            size_t count = 0;
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: break;
            case ix_OneLeafMax7: break;
            case ix_TwoLeaf3: break;
            case ix_TriLeaf2: break;
            case ix_HeptLeaf1: break;

            case ix_SparseLeaf1Ptr:
            case ix_DenseLeaf1Ptr:
                ++count;
                break;

            case ix_SparseBranchPtr:
                auto curr_ = curr.as!(SparseBranch*);
                ++count;
                foreach (subNode; curr_.subNodeSlots[0 .. curr_.subCount])
                {
                    if (subNode) { count += countHeapNodesAt(subNode); }
                }
                break;

            case ix_DenseBranchPtr:
                ++count;
                auto curr_ = curr.as!(DenseBranch*);
                foreach (subNode; curr_.subNodes)
                {
                    if (subNode) { count += countHeapNodesAt(subNode); }
                }
                break;
            }
            return count;
        }
    }

    @safe pure nothrow /* TODO @nogc */
    {
        /** Insert `key` into `this` tree. */
        pragma(inline) Node insert(Key!span key, out Node insertionNode)
        {
            return _root = insertAt(_root, key, insertionNode);
        }

        Node insertNew(Key!span key, out Node insertionNode)
        {
            debug if (willFail) { dln("WILL FAIL: curr:", key); }
            switch (key.length)
            {
            case 0: assert(false, "key must not be empty"); // return insertionNode = Node(construct!(OneLeafMax7)());
            case 1: return insertionNode = Node(construct!(HeptLeaf1)(key[0]));
            case 2: return insertionNode = Node(construct!(TriLeaf2)(key));
            case 3: return insertionNode = Node(construct!(TwoLeaf3)(key));
            default:
                if (key.length <= OneLeafMax7.capacity)
                {
                    return insertionNode = Node(construct!(OneLeafMax7)(key));
                }
                else                // key doesn't fit in a `OneLeafMax7`
                {
                    import std.algorithm : min;
                    const prefixLength = min(key.length - 1, // all but last Ix of key
                                             DefaultBranch.prefixCapacity); // as much as possible of key in branch prefix
                    auto prefix = key[0 .. prefixLength];
                    auto next = insertAtBranchBelowPrefix(Node(constructWithCapacity!(DefaultBranch)(1, prefix)),
                                                          key[prefix.length .. $], insertionNode);
                    assert(insertionNode);
                    return next;
                }
            }
        }

        Node insertNewBranch(Key!span key, out size_t skippedSuperBranchPrefixLength)
        {
            skippedSuperBranchPrefixLength = 0;
            return Node.init;
        }

        pragma(inline) Node toNode(Leaf curr) inout
        {
            final switch (curr.typeIx) with (Leaf.Ix)
            {
            case undefined: return Node.init;
            case ix_HeptLeaf1: return Node(curr.as!(HeptLeaf1));
            case ix_SparseLeaf1Ptr: return Node(curr.as!(SparseLeaf1!Value*));
            case ix_DenseLeaf1Ptr: return Node(curr.as!(DenseLeaf1!Value*));
            }
        }

        /** Insert `key` into sub-tree under root `curr`. */
        pragma(inline) Node insertAt(Node curr, Key!span key, out Node insertionNode)
        {
            debug if (willFail) { dln("WILL FAIL: key:", key, " curr:", curr); }
            assert(key.length);

            if (!curr)          // if no existing `Node` to insert at
            {
                auto next = insertNew(key, insertionNode);
                assert(insertionNode); // must be added to new Node
                return next;
            }
            else
            {
                final switch (curr.typeIx) with (Node.Ix)
                {
                case undefined:
                    return typeof(return).init;
                case ix_OneLeafMax7:
                    return insertAt(curr.as!(OneLeafMax7), key, insertionNode);
                case ix_TwoLeaf3:
                    return insertAt(curr.as!(TwoLeaf3), key, insertionNode);
                case ix_TriLeaf2:
                    return insertAt(curr.as!(TriLeaf2), key, insertionNode);
                case ix_HeptLeaf1:
                    return insertAt(curr.as!(HeptLeaf1), key, insertionNode);
                case ix_SparseLeaf1Ptr:
                    return insertAtLeaf(Leaf(curr.as!(SparseLeaf1!Value*)), key, insertionNode); // TODO use toLeaf(curr)
                case ix_DenseLeaf1Ptr:
                    return insertAtLeaf(Leaf(curr.as!(DenseLeaf1!Value*)), key, insertionNode); // TODO use toLeaf(curr)
                case ix_SparseBranchPtr:
                case ix_DenseBranchPtr:
                    return insertAtBranchAbovePrefix(curr, key, insertionNode);
                }
            }
        }

        /** Insert `key` into sub-tree under branch `curr` above prefix, that is
            the prefix of `curr` is stripped from `key` prior to insertion. */
        Node insertAtBranchAbovePrefix(Node curr, Key!span key, out Node insertionNode)
        {
            assert(key.length);

            import std.algorithm : commonPrefix;
            auto currPrefix = getPrefix(curr);
            auto matchedKeyPrefix = commonPrefix(key, currPrefix);

            debug if (willFail) { dln("WILL FAIL: key:", key,
                                " curr:", curr,
                                " currPrefix:", getPrefix(curr),
                                " matchedKeyPrefix:", matchedKeyPrefix); }

            if (matchedKeyPrefix.length == 0) // no prefix key match
            {
                if (currPrefix.length == 0) // no current prefix
                {
                    // NOTE: prefix:"", key:"cd"
                    return insertAtBranchBelowPrefix(curr, key, insertionNode);
                }
                else  // if (currPrefix.length >= 1) // non-empty current prefix
                {
                    // NOTE: prefix:"ab", key:"cd"
                    const currSubIx = currPrefix[0]; // subIx = 'a'
                    popFrontNPrefix(curr, 1);
                    auto next = constructWithCapacity!(DefaultBranch)(2, null,
                                                                      currSubIx, curr);
                    return insertAtBranchAbovePrefix(Node(next), key, insertionNode);
                }
            }
            else if (matchedKeyPrefix.length < key.length)
            {
                if (matchedKeyPrefix.length == currPrefix.length)
                {
                    // NOTE: key is an extension of prefix: prefix:"ab", key:"abcd"
                    return insertAtBranchBelowPrefix(curr, key[currPrefix.length .. $], insertionNode);
                }
                else
                {
                    // NOTE: prefix and key share beginning: prefix:"ab11", key:"ab22"
                    const currSubIx = currPrefix[matchedKeyPrefix.length]; // need index first before we modify curr.prefix
                    popFrontNPrefix(curr, matchedKeyPrefix.length + 1);
                    auto next = constructWithCapacity!(DefaultBranch)(2, matchedKeyPrefix,
                                                                      currSubIx, curr);
                    return insertAtBranchBelowPrefix(Node(next), key[matchedKeyPrefix.length .. $], insertionNode);
                }
            }
            else // if (matchedKeyPrefix.length == key.length)
            {
                assert(matchedKeyPrefix.length == key.length);
                if (matchedKeyPrefix.length < currPrefix.length)
                {
                    // NOTE: prefix is an extension of key: prefix:"abcd", key:"ab"
                    const nextPrefixLength = matchedKeyPrefix.length - 1;
                    const currSubIx = currPrefix[nextPrefixLength]; // need index first
                    popFrontNPrefix(curr, matchedKeyPrefix.length); // drop matchedKeyPrefix plus index to next super branch
                    auto next = constructWithCapacity!(DefaultBranch)(2, matchedKeyPrefix[0 .. $ - 1],
                                                                      currSubIx, curr);
                    return insertAtBranchBelowPrefix(Node(next), key[nextPrefixLength .. $], insertionNode);
                }
                else /* if (matchedKeyPrefix.length == currPrefix.length) and in turn
                        if (key.length == currPrefix.length */
                {
                    // NOTE: prefix equals key: prefix:"abcd", key:"abcd"
                    const currSubIx = currPrefix[matchedKeyPrefix.length - 1]; // need index first
                    popFrontNPrefix(curr, matchedKeyPrefix.length); // drop matchedKeyPrefix plus index to next super branch
                    auto next = constructWithCapacity!(DefaultBranch)(2, matchedKeyPrefix[0 .. $ - 1],
                                                                      currSubIx, curr);
                    return insertAtLeafOfBranch(Node(next), key[$ - 1], insertionNode);
                }
            }
        }

        /** Like `insertAtBranchAbovePrefix` but also asserts that `key` is
            currently not stored under `curr`. */
        pragma(inline) Node insertNewAtBranchAbovePrefix(Node curr, Key!span key)
        {
            Node insertionNode;
            auto next = insertAtBranchAbovePrefix(curr, key, insertionNode);
            assert(insertionNode);
            return next;
        }

        /** Insert `key` into sub-tree under branch `curr` below prefix, that is
            the prefix of `curr` is not stripped from `key` prior to
            insertion. */
        Node insertAtBranchBelowPrefix(Node curr, Key!span key, out Node insertionNode)
        {
            assert(key.length);
            debug if (willFail) { dln("WILL FAIL: key:", key,
                                " curr:", curr,
                                " currPrefix:", getPrefix(curr)); }
            if (key.length == 1)
            {
                return insertAtLeafOfBranch(curr, key[0], insertionNode);
            }
            else
            {
                const subIx = key[0];
                return setSub(curr, subIx,
                              insertAt(getSub(curr, subIx), // recurse
                                       key[1 .. $],
                                       insertionNode));
            }
        }

        pragma(inline) Node insertNewAtBranchBelowPrefix(Node curr, Key!span key)
        {
            Node insertionNode;
            auto next = insertAtBranchBelowPrefix(curr, key, insertionNode);
            assert(insertionNode);
            return next;
        }

        Leaf insertIxAtLeaftoLeaf(Leaf curr, Ix key, out Node insertionNode)
        {
            switch (curr.typeIx) with (Leaf.Ix)
            {
            case undefined:
                return typeof(return).init;
            case ix_HeptLeaf1:
                return insertAt(curr.as!(HeptLeaf1), key, insertionNode); // possibly expanded to other Leaf
            case ix_SparseLeaf1Ptr:
                auto curr_ = curr.as!(SparseLeaf1!Value*);
                if (curr_.linearInsert(key))
                {
                    insertionNode = Node(curr_);
                }
                break;
            case ix_DenseLeaf1Ptr:
                auto curr_ = curr.as!(DenseLeaf1!Value*);
                if (curr_.insert(key))
                {
                    insertionNode = Node(curr_);
                }
                break;
            default:
                assert(false, "Unsupported Leaf type " ~ curr.typeIx.to!string);
            }
            return curr;
        }

        Node insertAtLeafOfBranch(Node curr, Ix key, out Node insertionNode)
        {
            if (auto leaf = getLeaf(curr))
            {
                setLeaf(curr, insertIxAtLeaftoLeaf(leaf, key, insertionNode));
            }
            else
            {
                static if (hasValue) // TODO Add check if key + plus fit in 7 bytes (Value.sizeof <= 6) and use special node for that
                {
                    auto leaf_ = construct!(SparseLeaf1!Value*)(key); // needed for values
                }
                else
                {
                    auto leaf_ = construct!(HeptLeaf1)(key); // can pack more efficiently when no value
                }
                setLeaf(curr, Leaf(leaf_));
                insertionNode = leaf_;
            }
            return curr;
        }

        Node insertAtLeaf(Leaf curr, Key!span key, out Node insertionNode)
        {
            assert(key.length);
            if (key.length == 1)
            {
                return toNode(insertIxAtLeaftoLeaf(curr, key[0], insertionNode));
            }
            else
            {
                assert(key.length >= 2);
                const prefixLength = key.length - 2; // >= 0
                const nextPrefix = key[0 .. prefixLength];
                auto next = constructWithCapacity!(DefaultBranch)(1, nextPrefix, curr); // one sub-node and one leaf
                return insertAtBranchBelowPrefix(Node(next), key[prefixLength .. $], insertionNode);
            }
        }

        Node insertAt(OneLeafMax7 curr, Key!span key, out Node insertionNode)
        {
            assert(curr.key.length);
            debug if (willFail) { dln("WILL FAIL: key:", key, " curr.key:", curr.key); }

            import std.algorithm : commonPrefix;
            auto matchedKeyPrefix = commonPrefix(key, curr.key);
            if (curr.key.length == key.length)
            {
                if (matchedKeyPrefix.length == key.length) // curr.key, key and matchedKeyPrefix all equal
                {
                    return Node(curr); // already stored in `curr`
                }
                else if (matchedKeyPrefix.length + 1 == key.length) // key and curr.key are both matchedKeyPrefix plus one extra
                {
                    Node next;
                    switch (matchedKeyPrefix.length)
                    {
                    case 0:
                        next = construct!(HeptLeaf1)(curr.key[0], key[0]);
                        break;
                    case 1:
                        next = construct!(TriLeaf2)(curr.key, key);
                        break;
                    case 2:
                        next = construct!(TwoLeaf3)(curr.key, key);
                        break;
                    default:
                        import std.algorithm : min;
                        const nextPrefix = matchedKeyPrefix[0 .. min(matchedKeyPrefix.length,
                                                                     DefaultBranch.prefixCapacity)]; // limit prefix branch capacity
                        next = constructWithCapacity!(DefaultBranch)(1 + 1, // `curr` and `key`
                                                                     nextPrefix);
                        next = insertNewAtBranchBelowPrefix(next, curr.key[nextPrefix.length .. $]);
                        next = insertAtBranchBelowPrefix(next, key[nextPrefix.length .. $], insertionNode);
                        break;
                    }
                    freeNode(curr);
                    if (!insertionNode)
                    {
                        insertionNode = next;
                    }
                    return next;
                }
            }

            return insertAtBranchAbovePrefix(expand(curr), key, insertionNode);
        }

        Node insertAt(TwoLeaf3 curr, Key!span key, out Node insertionNode)
        {
            assert(hasVariableKeyLength || curr.keyLength == key.length);

            if (curr.keyLength == key.length)
            {
                if (curr.contains(key)) { return Node(curr); }
                if (!curr.keys.full)
                {
                    curr.keys.pushBack(key);
                    return insertionNode = Node(curr);
                }
            }
            return insertAt(expand(curr), key, insertionNode); // NOTE stay at same (depth)
        }

        Node insertAt(TriLeaf2 curr, Key!span key, out Node insertionNode)
        {
            assert(hasVariableKeyLength || curr.keyLength == key.length);
            if (curr.keyLength == key.length)
            {
                if (curr.contains(key)) { return Node(curr); }
                if (!curr.keys.full)
                {
                    curr.keys.pushBack(key);
                    return insertionNode = Node(curr);
                }
            }
            return insertAt(expand(curr),
                            key, insertionNode); // NOTE stay at same (depth)
        }

        Leaf insertAt(HeptLeaf1 curr, Ix key, out Node insertionNode)
        {
            if (curr.contains(key)) { return Leaf(curr); }
            if (!curr.keys.full)
            {
                curr.keys.pushBack(key);
                insertionNode = Node(curr);
                return Leaf(curr);
            }

            auto next = construct!(SparseLeaf1!Value*)(curr.keys); // TODO construct using (curr.keys, key[0])
            next.pushBack(key); // pushBack instead of insert because we know that `key` is distinct from `curr.keys` from above
            freeNode(curr);
            insertionNode = Node(next);
            return Leaf(next);
        }

        Node insertAt(HeptLeaf1 curr, Key!span key, out Node insertionNode)
        {
            assert(hasVariableKeyLength || curr.keyLength == key.length);
            if (curr.keyLength == key.length)
            {
                return toNode(insertAt(curr, key[0], insertionNode)); // use `Ix key`-overload
            }
            return insertAt(Node(constructWithCapacity!(DefaultBranch)(1, Leaf(curr))), // current `key`
                            key, insertionNode); // NOTE stay at same (depth)
        }

        /** Split `curr` using `prefix`. */
        Node split(OneLeafMax7 curr, Key!span prefix, Key!span key) // TODO key here is a bit malplaced
        {
            if (key.length == 0) { dln("TODO key shouldn't be empty when curr:", curr); } assert(key.length);
            assert(hasVariableKeyLength || curr.key.length == key.length);

            if (curr.key.length == key.length) // balanced tree possible
            {
                switch (curr.key.length)
                {
                case 1:
                    if (prefix.length == 0)
                    {
                        freeNode(curr);
                        return Node(construct!(HeptLeaf1)(curr.key)); // TODO removing parameter has no effect. why?
                    }
                    break;
                case 2:
                    freeNode(curr);
                    return Node(construct!(TriLeaf2)(curr.key));
                case 3:
                    freeNode(curr);
                    return Node(construct!(TwoLeaf3)(curr.key));
                default:
                    break;
                }
            }

            // default case
            Node next = constructWithCapacity!(DefaultBranch)(1 + 1, prefix); // current plus one more
            next = insertNewAtBranchBelowPrefix(next, curr.key[prefix.length .. $]);
            freeNode(curr);   // remove old current

            return next;
        }

        /** Destructively expand `curr` to make room for `capacityIncrement`
            more keys and return it. */
        Node expand(OneLeafMax7 curr, size_t capacityIncrement = 1)
        {
            assert(curr.key.length >= 2);
            typeof(return) next;

            if (curr.key.length <= DefaultBranch.prefixCapacity + 1) // if `key` fits in `prefix` of `DefaultBranch`
            {
                next = constructWithCapacity!(DefaultBranch)(1 + capacityIncrement, curr.key[0 .. $ - 1], // all but last
                                                             Leaf(construct!(HeptLeaf1)(curr.key[$ - 1]))); // last as a leaf
            }
            else                // curr.key.length > DefaultBranch.prefixCapacity + 1
            {
                next = constructWithCapacity!(DefaultBranch)(1 + capacityIncrement, curr.key[0 .. DefaultBranch.prefixCapacity]);
                next = insertNewAtBranchBelowPrefix(next, curr.key[DefaultBranch.prefixCapacity .. $]);
            }

            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` to make room for `capacityIncrement`
            more keys and return it. */
        Node expand(TwoLeaf3 curr, size_t capacityIncrement = 1)
        {
            Node next;
            if (curr.keys.length == 1) // only one key
            {
                next = insertNewAtBranchAbovePrefix(Node(constructWithCapacity!(DefaultBranch)(1 + capacityIncrement)), // current keys plus one more
                                                    curr.keys.at!0);
            }
            else
            {
                next = constructWithCapacity!(DefaultBranch)(curr.keys.length + capacityIncrement, curr.prefix);
                // TODO functionize to insertNewAtBranchAbovePrefix(next, curr.keys)
                foreach (key; curr.keys)
                {
                    next = insertNewAtBranchBelowPrefix(next, key[curr.prefix.length .. $]);
                }
            }
            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` and return it. */
        Node expand(TriLeaf2 curr, size_t capacityIncrement = 1)
        {
            // TODO functionize:
            Node next;
            if (curr.keys.length == 1) // only one key
            {
                next = constructWithCapacity!(DefaultBranch)(1 + capacityIncrement); // current keys plus one more
                next = insertNewAtBranchAbovePrefix(next, curr.keys.at!0);
            }
            else
            {
                next = constructWithCapacity!(DefaultBranch)(curr.keys.length + capacityIncrement, curr.prefix);
                // TODO functionize to insertNewAtBranchAbovePrefix(next, curr.keys)
                foreach (key; curr.keys)
                {
                    next = insertNewAtBranchBelowPrefix(next, key[curr.prefix.length .. $]);
                }
            }
            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` making room for `nextKey` and return it. */
        Node expand(HeptLeaf1 curr, size_t capacityIncrement = 1)
        {
            auto next = construct!(SparseLeaf1!Value*)(curr.keys);
            freeNode(curr);
            return Node(next);
        }

    }

    /** Returns: `true` iff tree is empty (no elements stored). */
    pragma(inline) bool empty() const @safe pure nothrow /* TODO @nogc */ { return !_root; }

    /** Returns: number of elements store. */
    pragma(inline) size_t length() const @safe pure nothrow /* TODO @nogc */ { return _length; }

    private:

    /** Allocate (if pointer) and Construct a `Node`-type of value type `NodeType`
        using constructor arguments `args` of `Args`.
    */
    auto construct(NodeType, Args...)(Args args) @trusted
        if (!is(NodeType == SparseBranch*))
    {
        version(debugAllocations) { dln("constructing ", NodeType.stringof, " from ", args); }
        debug ++nodeCountsByIx[NodeType.stringof];
        static if (isPointer!NodeType)
        {
            debug ++_heapNodeAllocationBalance;
            import std.conv : emplace;
            return emplace(cast(NodeType)malloc((*NodeType.init).sizeof), args);
            // TODO ensure alignment of node at least that of NodeType.alignof
        }
        else
        {
            return NodeType(args);
        }
    }

    auto constructWithCapacity(NodeType, Args...)(size_t subCapacity, Args args) @trusted
        if (is(NodeType == SparseBranch*))
    {
        version(debugAllocations) { dln("constructing ", NodeType.stringof, " from ", args); }
        debug ++nodeCountsByIx[NodeType.stringof];
        debug ++_heapNodeAllocationBalance;
        import std.conv : emplace;
        import std.algorithm : max;
        subCapacity = max(SparseBranch.subCapacityMin, subCapacity);
        return emplace(cast(NodeType)malloc(NodeType.allocationSize(subCapacity)), subCapacity, args);
        // TODO ensure alignment of node at least that of NodeType.alignof
    }

    void freeNode(NodeType)(NodeType nt) @trusted
    {
        version(debugAllocations) { dln("freeing ", NodeType.stringof, " ", nt); }
        static if (isPointer!NodeType)
        {
            free(cast(void*)nt);  // TODO Allocator.free
            debug --_heapNodeAllocationBalance;
        }
        debug --nodeCountsByIx[NodeType.stringof];
    }

    @safe pure nothrow /* TODO @nogc */
    {
        pragma(inline) void release(SparseLeaf1!Value* curr)
        {
            freeNode(curr);
        }
        pragma(inline) void release(DenseLeaf1!Value* curr)
        {
            freeNode(curr);
        }

        void release(SparseBranch* curr)
        {
            foreach (sub; curr.subNodes[0 .. curr.subCount])
            {
                release(sub); // recurse branch
            }
            if (curr.leaf)
            {
                release(curr.leaf); // recurse leaf
            }
            freeNode(curr);
        }

        void release(DenseBranch* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse branch
            }
            if (curr.leaf)
            {
                release(curr.leaf); // recurse leaf
            }
            freeNode(curr);
        }

        void release(OneLeafMax7 curr) { freeNode(curr); }
        void release(TwoLeaf3 curr) { freeNode(curr); }
        void release(TriLeaf2 curr) { freeNode(curr); }
        void release(HeptLeaf1 curr) { freeNode(curr); }

        /// Release `Leaf curr`.
        void release(Leaf curr)
        {
            final switch (curr.typeIx) with (Leaf.Ix)
            {
            case undefined: break; // ignored
            case ix_HeptLeaf1: return release(curr.as!(HeptLeaf1));
            case ix_SparseLeaf1Ptr: return release(curr.as!(SparseLeaf1!Value*));
            case ix_DenseLeaf1Ptr: return release(curr.as!(DenseLeaf1!Value*));
            }
        }

        /// Release `Node curr`.
        void release(Node curr)
        {
            version(debugAllocations) { dln("releasing Node ", curr); }
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: break; // ignored
            case ix_OneLeafMax7: return release(curr.as!(OneLeafMax7));
            case ix_TwoLeaf3: return release(curr.as!(TwoLeaf3));
            case ix_TriLeaf2: return release(curr.as!(TriLeaf2));
            case ix_HeptLeaf1: return release(curr.as!(HeptLeaf1));
            case ix_SparseLeaf1Ptr: return release(curr.as!(SparseLeaf1!Value*));
            case ix_DenseLeaf1Ptr: return release(curr.as!(DenseLeaf1!Value*));
            case ix_SparseBranchPtr: return release(curr.as!(SparseBranch*));
            case ix_DenseBranchPtr: return release(curr.as!(DenseBranch*));
            }
        }

        bool isHeapAllocatedNode(Node curr) const
        {
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: return false;
            case ix_OneLeafMax7: return false;
            case ix_TwoLeaf3: return false;
            case ix_TriLeaf2: return false;
            case ix_HeptLeaf1: return false;
            case ix_SparseLeaf1Ptr: return true;
            case ix_DenseLeaf1Ptr: return true;
            case ix_SparseBranchPtr: return true;
            case ix_DenseBranchPtr: return true;
            }
        }
    }

    /** Returns: `true` if all keys in tree are of fixed length/size, `false` otherwise. */
    pragma(inline) bool hasFixedKeyLength() const @safe pure nothrow @nogc
    {
        return (fixedKeyLength !=
                fixedKeyLengthUndefined);
    }
    /** Returns: `true` if keys in tree may be of variable length/size, `false` otherwise. */
    pragma(inline) bool hasVariableKeyLength() const @safe pure nothrow @nogc
    {
        return !hasFixedKeyLength;
    }

    /// Returns: number of nodes used in `this` tree. Should always equal `Stats.heapNodeCount`.
    pragma(inline) debug size_t heapNodeAllocationBalance() @safe pure nothrow /* TODO @nogc */
    {
        return _heapNodeAllocationBalance;
    }

    pragma(inline) void print() @safe const
    {
        printAt(_root, 0);
    }

    void printAt(Node curr, size_t depth, uint subIx = uint.max) @safe const
    {
        import std.range : repeat;
        import std.stdio : write, writeln;

        if (!curr) { return; }

        foreach (const i; 0 .. depth) { write('-'); } // prefix
        if (subIx != uint.max)
        {
            import std.string : format;
            write(format("%.2X ", subIx));
        }

        final switch (curr.typeIx) with (Node.Ix)
        {
        case undefined:
            dln("TODO: Trying to print undefined Node");
            break;
        case ix_OneLeafMax7:
            auto curr_ = curr.as!(OneLeafMax7);
            writeln(typeof(curr_).stringof, "#", curr_.key.length, ": ", curr_.to!string);
            break;
        case ix_TwoLeaf3:
            auto curr_ = curr.as!(TwoLeaf3);
            writeln(typeof(curr_).stringof, "#", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_TriLeaf2:
            auto curr_ = curr.as!(TriLeaf2);
            writeln(typeof(curr_).stringof, "#", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_HeptLeaf1:
            auto curr_ = curr.as!(HeptLeaf1);
            writeln(typeof(curr_).stringof, "#", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_SparseLeaf1Ptr:
            auto curr_ = curr.as!(SparseLeaf1!Value*);
            write(typeof(*curr_).stringof, "#", curr_.length, "/", curr_.capacity, " @", curr_);
            write(": ");
            bool other = false;
            foreach (const ix; curr_.keys)
            {
                string s;
                if (other)
                {
                    s ~= keySeparator;
                }
                else
                {
                    other = true;
                }
                import std.string : format;
                s ~= format("%.2X", ix);
                write(s);
            }
            writeln();
            break;
        case ix_DenseLeaf1Ptr:
            auto curr_ = curr.as!(DenseLeaf1!Value*);
            write(typeof(*curr_).stringof, "#", curr_.count, " @", curr_);
            write(": ");

            // keys
            size_t ix = 0;
            bool other = false;
            foreach (const keyBit; curr_._keyBits[])
            {
                string s;
                if (keyBit)
                {
                    if (other)
                    {
                        s ~= keySeparator;
                    }
                    else
                    {
                        other = true;
                    }
                    import std.string : format;
                    s ~= format("%.2X", ix);
                }
                ++ix;
                write(s);
            }

            writeln();
            break;
        case ix_SparseBranchPtr:
            auto curr_ = curr.as!(SparseBranch*);
            write(typeof(*curr_).stringof, "#", curr_.subCount, "/", curr_.subCapacity, " @", curr_);
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            writeln(":");
            if (curr_.leaf)
            {
                printAt(toNode(curr_.leaf), depth + 1);
            }
            foreach (const i, const subNode; curr_.subNodes)
            {
                printAt(subNode, depth + 1, cast(uint)curr_.subIxs[i]);
            }
            break;
        case ix_DenseBranchPtr:
            auto curr_ = curr.as!(DenseBranch*);
            write(typeof(*curr_).stringof, "#", curr_.subCount, "/", radix, " @", curr_);
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            writeln(":");
            if (curr_.leaf)
            {
                printAt(toNode(curr_.leaf), depth + 1);
            }
            foreach (const i, const subNode; curr_.subNodes)
            {
                printAt(subNode, depth + 1, cast(uint)i);
            }

            break;
        }
    }

    private:
    Node _root;                 ///< tree root node
    size_t _length = 0; ///< number of elements (keys or key-value-pairs) currently stored under `_root`
    immutable fixedKeyLength = fixedKeyLengthUndefined; ///< maximum length of key if fixed, otherwise 0
    enum fixedKeyLengthUndefined = 0;

    // debug stuff
    debug long _heapNodeAllocationBalance = 0;
    debug size_t[string] nodeCountsByIx;
    debug bool willFail;

}

/** Append statistics of tree under `Node` `curr.` into `stats`.
 */
static private void calculate(Value)(RawRadixTree!(Value).Node curr,
                                     ref RawRadixTree!(Value).Stats stats)
    @safe pure nothrow /* TODO @nogc */
{
    alias RT = RawRadixTree!(Value);
    ++stats.popByNodeType[curr.typeIx];

    final switch (curr.typeIx) with (RT.Node.Ix)
    {
    case undefined: break;
    case ix_OneLeafMax7: break; // TODO calculate()
    case ix_TwoLeaf3: break; // TODO calculate()
    case ix_TriLeaf2: break; // TODO calculate()
    case ix_HeptLeaf1: break; // TODO calculate()
    case ix_SparseLeaf1Ptr:
        ++stats.heapNodeCount;
        auto curr_ = curr.as!(SparseLeaf1!Value*);
        ++stats.popHist_SparseLeaf1[curr_.length - 1]; // TODO type-safe indexing
        break;
    case ix_DenseLeaf1Ptr:
        auto curr_ = curr.as!(DenseLeaf1!Value*);
        ++stats.heapNodeCount;
        const count = curr_._keyBits.countOnes; // number of non-zero sub-nodes
        assert(count <= curr_.maxCount);
        ++stats.popHist_DenseLeaf1[count - 1]; // TODO type-safe indexing
        break;
    case ix_SparseBranchPtr:
        ++stats.heapNodeCount;
        curr.as!(RT.SparseBranch*).calculate(stats);
        break;
    case ix_DenseBranchPtr:
        ++stats.heapNodeCount;
        curr.as!(RT.DenseBranch*).calculate(stats);
        break;
    }
}

/** Append statistics of tree under `Leaf` `curr.` into `stats`.
 */
static private void calculate(Value)(RawRadixTree!(Value).Leaf curr,
                                     ref RawRadixTree!(Value).Stats stats)
    @safe pure nothrow /* TODO @nogc */
{
    alias RT = RawRadixTree!(Value);
    ++stats.popByLeafType[curr.typeIx];

    with (RT.Leaf.Ix)
    {
        final switch (curr.typeIx)
        {
        case undefined: break;
        case ix_HeptLeaf1: break; // TODO calculate()
        case ix_SparseLeaf1Ptr:
            ++stats.heapNodeCount;
            auto curr_ = curr.as!(SparseLeaf1!Value*);
            ++stats.popHist_SparseLeaf1[curr_.length - 1]; // TODO type-safe indexing
            break;
        case ix_DenseLeaf1Ptr:
            auto curr_ = curr.as!(DenseLeaf1!Value*);
            ++stats.heapNodeCount;
            const count = curr_._keyBits.countOnes; // number of non-zero curr-nodes
            assert(count <= curr_.maxCount);
            ++stats.popHist_DenseLeaf1[count - 1]; // TODO type-safe indexing
            break;
        }
    }
}

/** Remap typed key `typedKey` to untype key of `Key`. */
static private Key!span remapKey(TypedKey)(in TypedKey typedKey)
    @trusted pure nothrow /* TODO @nogc */
    if (allSatisfy!(isTrieableKeyType, TypedKey))
{
    import std.traits : isArray;
    enum radix = 2^^span;     // branch-multiplicity, typically either 2, 4, 16 or 256
    alias Ix = Mod!radix;

    static if (isFixedTrieableKeyType!TypedKey)
    {
        const ukey = typedKey.bijectToUnsigned;

        enum nbits = 8*ukey.sizeof; // bitsize of ukey
        enum chunkCount = nbits/span; // number of chunks in ukey
        static assert(chunkCount*span == nbits, "Bitsize of TypedKey must be a multiple of span:" ~ span.stringof);

        KeyN!(span, TypedKey.sizeof) key;

        static if (span == 8)
        {
            foreach (bix; 0 .. chunkCount)
            {
                const bitShift = (chunkCount - 1 - bix)*span; // most significant bit chunk first (MSBCF)
                key[bix] = (ukey >> bitShift) & (radix - 1); // part of value which is also an index
            }
        }

        return key.dup; // TODO avoid this GC-allocation
    }
    else static if (isArray!TypedKey &&
                    is(Unqual!(typeof(TypedKey.init[0])) == char))
    {
        import std.string : representation;
        const ubyte[] key = typedKey.representation; // lexical byte-order
        return cast(Ix[])key;
    }
    else static if (is(Unqual!TypedKey == wstring))
    {
        const ushort[] rKey = typedKey.representation; // lexical byte-order.
        // TODO MSByte-order of elements in rKey for ordered access and good branching performance
        const ubyte[] key = (cast(const ubyte*)rKey.ptr)[0 .. rKey[0].sizeof * rKey.length]; // TODO @trusted functionize. Reuse existing Phobos function?
        return key;
    }
    else static if (is(Unqual!TypedKey == dstring))
    {
        const uint[] rKey = typedKey.representation; // lexical byte-order
        // TODO MSByte-order of elements in rKey for ordered access and good branching performance
        const ubyte[] key = (cast(const ubyte*)rKey.ptr)[0 .. rKey[0].sizeof * rKey.length]; // TODO @trusted functionize. Reuse existing Phobos function?
        return key;
    }
    else
    {
        static assert(false, "TODO Handle typed key " ~ TypedKey.stringof);
    }
}

/// Radix-Tree with key of type `Key` and value of type `Value` (if non-`void`).
struct RadixTree(Key, Value)
    if (allSatisfy!(isTrieableKeyType, Key))
{
    this(bool unusedDummy)      // TODO how do we get rid of the need for `unusedDummy`?
    {
        this.fixedKeyLength = isFixedTrieableKeyType!Key ? Key.sizeof : fixedKeyLengthUndefined;
    }

    /** Insert `key`.
        Returns: `true` if `key` wasn't previously inserted, `false` otherwise.
     */
    bool insert(in Key key)
        @safe pure nothrow /* TODO @nogc */
    {
        _tree.Node insertionNode; // indicates that key was added
        _tree.insert(key.remapKey, insertionNode);
        _length += !insertionNode.isNull;
        return !insertionNode.isNull;
    }

    static if (!_tree.hasValue)
    {
        const nothrow:

        /** Returns: `true` if `key` is stored, `false` otherwise. */
        bool contains(in Key typedKey)
        {
            return _tree.contains(typedKey.remapKey);
        }
    }

    static if (_tree.hasValue)
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        bool insert(in Key key, in Value value)
        {
            _tree.Node insertionNode; // indicates that key was added
            auto rawKey = key.remapKey;
            _tree.insert(rawKey, insertionNode);
            if (insertionNode)  // if `key` was inserted at `insertionNode`
            {
                // set value
                final switch (insertionNode.typeIx) with (_tree.Node.Ix)
                {
                case undefined: break;
                case ix_OneLeafMax7:
                case ix_TwoLeaf3:
                case ix_TriLeaf2:
                case ix_HeptLeaf1:
                    dln("TODO Insertion of key:", key, " in a non-pointer node:", insertionNode);
                    break;
                case ix_SparseLeaf1Ptr:
                    assert(insertionNode.as!(SparseLeaf1!Value*).setValue(rawKey[$ - 1], value));
                    break;
                case ix_DenseLeaf1Ptr:
                    insertionNode.as!(DenseLeaf1!Value*).setValue(rawKey[$ - 1], value);
                    break;
                case ix_SparseBranchPtr: break;
                case ix_DenseBranchPtr: break;
                }
                ++_length;
                return true;
            }
            else
            {
                dln("TODO warning no insertionNode for key:", key, " rawKey:", rawKey);
                return false;
            }
        }

        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        Value* contains(in Key key) const
        {
            return null;
        }
    }

    /** Supports $(B `Key` in `this`) syntax. */
    auto opBinaryRight(string op)(in Key key) const if (op == "in") { return contains(key); }

    void print() @safe const
    {
        _tree.print();
    }

    private RawRadixTree!(Value) _tree;
    alias _tree this;
}
alias PatriciaTrie = RadixTree;
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator of set-version of `RadixTree` where value-type is `void` (unused).
auto radixTreeSet(Key)()
{
    static if (is(Key == string))
    {
        alias MutableKey = const(char)[];
    }
    else
    {
        alias MutableKey = Key;
    }
    return RadixTree!(MutableKey, void)(false);
}

/// Instantiator of map-version of `RadixTree` where value-type is `Value`.
auto radixTreeMap(Key, Value)() { return RadixTree!(Key, Value)(false); }

///
@safe pure nothrow /* TODO @nogc */ unittest
{
    alias Value = ulong;
    auto set = radixTreeSet!(Value);

    alias NodeType = SparseLeaf1!Value*;
    NodeType sl = null;
    // set.Node node = set.Node(sl);
    // static assert(node.canStore!(NodeType));
    // assert(!node.isNull);
}

/** Calculate and print statistics of `tree`. */
void showStatistics(RT)(const ref RT tree) // why does `in`RT tree` trigger a copy ctor here
{
    import std.stdio : writeln;
    auto stats = tree.usageHistograms;

    writeln("Population By Node Type: ", stats.popByNodeType);
    writeln("Population By Leaf Type: ", stats.popByLeafType);

    writeln("SparseBranch Population Histogram: ", stats.popHist_SparseBranch);
    writeln("DenseBranch Population Histogram: ", stats.popHist_DenseBranch);

    writeln("SparseLeaf1 Population Histogram: ", stats.popHist_SparseLeaf1);
    writeln("DenseLeaf1 Population Histogram: ", stats.popHist_DenseLeaf1);

    size_t totalBytesUsed = 0;

    // Node-usage
    foreach (RT.Node.Ix ix, pop; stats.popByNodeType) // TODO use stats.byPair when added to typecons_ex.d
    {
        size_t bytesUsed = 0;
        with (RT.Node.Ix)
        {
            final switch (ix)
            {
            case undefined: break;
            case ix_OneLeafMax7: bytesUsed = pop*OneLeafMax7.sizeof; break;
            case ix_TwoLeaf3: bytesUsed = pop*TwoLeaf3.sizeof; break;
            case ix_TriLeaf2: bytesUsed = pop*TriLeaf2.sizeof; break;
            case ix_HeptLeaf1: bytesUsed = pop*HeptLeaf1.sizeof; break;
            case ix_SparseLeaf1Ptr:
                bytesUsed = pop*SparseLeaf1!(RT.ValueType).sizeof;
                totalBytesUsed += bytesUsed;
                break;
            case ix_DenseLeaf1Ptr:
                bytesUsed = pop*DenseLeaf1!(RT.ValueType).sizeof;
                totalBytesUsed += bytesUsed;
                break;
            case ix_SparseBranchPtr:
                bytesUsed = stats.sparseBranchSizeSum; // must be used because SparseBranch.sizeof cannot be used because it's a variable length struct
                totalBytesUsed += bytesUsed;
                break;
            case ix_DenseBranchPtr:
                bytesUsed = pop*RT.DenseBranch.sizeof;
                totalBytesUsed += bytesUsed;
                break;
            }
        }
        if (bytesUsed)
        {
            writeln(pop, " number of ", ix, " uses ", bytesUsed/1e6, " megabytes");
        }
    }

    // Leaf-usage
    foreach (RT.Leaf.Ix ix, pop; stats.popByLeafType) // TODO use stats.byPair when added to typecons_ex.d
    {
        size_t bytesUsed = 0;
        with (RT.Leaf.Ix)
        {
            final switch (ix)
            {
            case undefined: break;
            case ix_HeptLeaf1: bytesUsed = pop*HeptLeaf1.sizeof; break;
            case ix_SparseLeaf1Ptr: bytesUsed = pop*SparseLeaf1!(RT.ValueType).sizeof; totalBytesUsed += bytesUsed; break;
            case ix_DenseLeaf1Ptr: bytesUsed = pop*DenseLeaf1!(RT.ValueType).sizeof; totalBytesUsed += bytesUsed; break;
            }
        }
        if (bytesUsed)
        {
            writeln(pop, " number of ", ix, " uses ", bytesUsed/1e6, " megabytes");
        }
    }

    writeln("Tree uses ", totalBytesUsed/1e6, " megabytes");
}

///
@safe pure nothrow /* TODO @nogc */
unittest
{
    auto set = radixTreeSet!(ulong);
    enum N = HeptLeaf1.capacity;

    foreach (const i; 0 .. N)
    {
        assert(!set.contains(i));

        assert(set.insert(i));
        assert(set.contains(i));

        assert(!set.insert(i));
        assert(set.contains(i));
    }

    foreach (const i; N .. 256)
    {
        assert(!set.contains(i));

        assert(set.insert(i));
        assert(set.contains(i));

        assert(!set.insert(i));
        assert(set.contains(i));
    }

    foreach (const i; 256 .. 256 + N)
    {
        assert(!set.contains(i));

        assert(set.insert(i));
        assert(set.contains(i));

        assert(!set.insert(i));
        assert(set.contains(i));
    }

    foreach (const i; 256 + N .. 256 + 256)
    {
        assert(!set.contains(i));

        assert(set.insert(i));
        assert(set.contains(i));

        assert(!set.insert(i));
        assert(set.contains(i));
    }
}

///
@safe pure nothrow /* TODO @nogc */ unittest
{
    auto set = radixTreeSet!(ubyte);
    alias Set = typeof(set);

    foreach (const i; 0 .. HeptLeaf1.capacity)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.contains(i));
        debug assert(set.heapNodeAllocationBalance == 0);
        const rootRef = set._root.peek!(HeptLeaf1);
        assert(rootRef);
    }

    foreach (const i; HeptLeaf1.capacity .. 256)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.contains(i));
        debug assert(set.heapNodeAllocationBalance == 1);
        const rootRef = set._root.peek!(SparseLeaf1!void*);
        assert(rootRef);
    }

    const rootRef = set._root.peek!(SparseLeaf1!void*);
    assert(rootRef);

    const root = *rootRef;
    assert(!root.empty);
    assert(root.full);
}

///
@safe pure nothrow /* TODO @nogc */ unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(ushort, uint))
    {
        auto set = radixTreeSet!(T);
        alias Set = typeof(set);

        foreach (const i; 0 .. 256)
        {
            assert(!set.contains(i));

            assert(set.insert(i));
            assert(set.contains(i));

            assert(!set.insert(i));
            assert(set.contains(i));
        }

        assert(!set.contains(256));
        assert(set.insert(256));
        assert(set.contains(256));
        assert(!set.insert(256));
        assert(set.contains(256));

        assert(!set.contains(257));
        assert(set.insert(257));
        assert(set.contains(257));
        assert(!set.insert(257));
        assert(set.contains(257));

        const rootRef = set._root.peek!(Set.DefaultBranch);
        assert(rootRef);
        const root = *rootRef;
        assert(root.prefix.length == T.sizeof - 2);

    }
}

/// Create a set of words from /usr/share/dict/words
unittest
{
    immutable path = "/usr/share/dict/words";

    auto set = radixTreeSet!(string);
    assert(set.empty);

    size_t count = 0;
    enum debugPrint = false;

    import std.datetime : StopWatch, AutoStart, Duration;
    auto sw = StopWatch(AutoStart.yes);

    import std.stdio : File;
    string[] firsts = [];
    import std.range : chain;
    foreach (const word; chain(firsts, File(path).byLine))
    {
        import std.algorithm.searching : endsWith;
        import std.range : empty;
        if (!word.empty &&
            !word.endsWith(`'s`)) // skip genitive forms
        {
            if (word.length <= 15)
            {
                assert(!set.contains(word));

                static if (debugPrint)
                {
                    import std.string : representation;
                    dln(`word:"`, word, `" of length:`, word.length, ` of representation:`, word.representation);
                    set.willFail = word == `amiable`;
                    if (set.willFail)
                    {
                        set.print();
                    }
                }

                assert(set.insert(word));

                assert(set.contains(word));

                assert(!set.insert(word));
                assert(set.contains(word));

                ++count;
            }
        }
    }
    sw.stop;
    version(print)
    {
        import std.conv : to;
        import std.stdio : writeln;
        writeln("Added ", count, " words from ", path, " in ", sw.peek().to!Duration);
        set.showStatistics();
    }
}

/** Generate `count` number of random unique strings of minimum length 1 and
    maximum length of `maxLength`.
 */
private static auto randomUniqueStrings(size_t count, uint maxLength)
    @trusted
{
    import std.random : Random, uniform;
    auto gen = Random();

    bool[string] stringSet;  // set of strings using D's builtin associative array
    while (stringSet.length < count)
    {
        const length = uniform(1, maxLength, gen);
        auto key = new char[length];
        foreach (ix; 0 .. length)
        {
            key[ix] = cast(char)('a' + 0.uniform(26, gen));
        }
        stringSet[key[].idup] = true;
    }
    import std.array : array;
    return stringSet.byKey.array;
}

/// Check string types in `Keys`.
auto checkString(Keys...)(size_t count, uint maxLength)
    if (Keys.length >= 1)
{
    void testContainsAndInsert(Set, Key)(ref Set set, Key key)
        if (isSomeString!Key)
    {
        import std.conv : to;
        immutable failMessage = `Failed for key: "` ~ key.to!string ~ `"`;
        debug set.willFail = key == "utsuj";
        assert(!set.contains(key), failMessage);
        assert(set.insert(key), failMessage);
        assert(set.contains(key), failMessage);
        assert(!set.insert(key), failMessage);
        assert(set.contains(key), failMessage);
    }

    foreach (Key; Keys)
    {
        auto set = radixTreeSet!(Key);
        assert(set.empty);

        foreach (const key; randomUniqueStrings(count,
                                                maxLength))
        {
            testContainsAndInsert(set, key);
        }
    }
}

///
@safe pure /* TODO @nogc */
unittest
{
    checkString!(string)(2^^18, 2^7);
}

/// Check correctness when span is `span` and for each `Key` in `Keys`.
auto checkNumeric(Keys...)()
    if (Keys.length >= 1)
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        import std.algorithm : equal;
        struct TestValueType { int i; float f; char ch; }
        alias Value = TestValueType;
        import std.meta : AliasSeq;
        foreach (Key; Keys)
        {
            // dln("Key: ", Key.stringof);
            alias Tree = radixTreeSet!(Key);
            auto set = Tree;
            assert(set.hasFixedKeyLength == isFixedTrieableKeyType!Key);
            assert(set.empty);

            static assert(!set.hasValue);

            import std.algorithm : min, max;

            const useContains = true;

            static if (isIntegral!Key ||
                       isFloatingPoint!Key)
            {
                static if (isIntegral!Key)
                {
                    const low = max(Key.min, -98900); // chosen to minimize number of lines of debug output before bug in contains happens
                    const high = min(Key.max, 100_000);
                    const length = high - low + 1;
                }
                else static if (isFloatingPoint!Key)
                {
                    const low = -100_000;
                    const high = 100_000;
                    const length = high - low + 1;
                }

                size_t cnt = 0;
                foreach (const uk; low.iota(high + 1))
                {
                    const Key key = cast(Key)uk;
                    // debug set.willFail = (key == -32639);
                    if (useContains)
                    {
                        debug if (set.willFail) dln("before check no contains yet");
                        assert(!set.contains(key)); // key should not yet be in set
                        assert(key !in set);        // alternative syntax
                    }

                    debug if (set.willFail) dln("before first insert()");
                    assert(set.insert(key));  // insert new value returns `true` (previously not in set)
                    if (useContains)
                    {
                        debug if (set.willFail) dln("before check passing contains()");
                        assert(set.contains(key)); // key should now be in set
                    }
                    debug if (set.willFail) dln("before second insert()");
                    assert(!set.insert(key)); // reinsert same value returns `false` (already in set)

                    if (useContains)
                    {
                        assert(set.contains(key)); // key should now be in set
                        assert(key in set);        // alternative syntax
                        if (key != Key.max)        // except last value
                        {
                            assert(!set.contains(cast(Key)(key + 1))); // next key is not yet in set
                        }
                    }
                    ++cnt;
                }
                assert(set.length == length);
            }
            else
            {
                static assert(false, `Handle type: "` ~ Key.stringof ~ `"`);
            }

            auto map = radixTreeMap!(Key, Value);
            assert(map.hasFixedKeyLength == isFixedTrieableKeyType!Key);
            static assert(map.hasValue);

            map.insert(Key.init, Value.init);
        }
    }
}

/// Benchmark performance and memory usage when span is `span`.
void benchmark()()
{
    import core.thread : sleep;
    import std.range : iota;
    import std.stdio : writeln;

    import std.algorithm : equal;
    struct TestValueType { int i; float f; char ch; }
    alias Value = TestValueType;
    import std.meta : AliasSeq;
    foreach (Key; AliasSeq!(uint)) // just benchmark uint for now
    {
        auto set = radixTreeSet!(Key);
        alias Set = set;
        assert(set.empty);

        static assert(!set.hasValue);

        import std.conv : to;
        import std.datetime : StopWatch, AutoStart, Duration;

        enum n = 10_000_000;

        import std.array : array;
        import std.random : randomShuffle;

        const useUniqueRandom = false;

        // TODO functionize to randomIota in range_ex.d
        auto randomIotaSamples = 0.iota(n).array; randomIotaSamples.randomShuffle;

        import std.range : generate, take;
        import std.random : uniform;
        auto randomSamples = generate!(() => uniform!Key).take(n);

        {
            auto sw = StopWatch(AutoStart.yes);

            foreach (Key k; randomSamples)
            {
                if (useUniqueRandom)
                {
                    assert(set.insert(k));
                }
                else
                {
                    set.insert(k);
                }

                /* second insert of same key should always return `false` to
                   indicate that key was already stored */
                static if (false) { assert(!set.insert(k)); }
            }

            writeln("trie: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");

            set.showStatistics();

            sleep(2);
            writeln("Sleeping done");
        }

        {
            auto sw = StopWatch(AutoStart.yes);
            bool[int] aa;

            foreach (Key k; randomSamples) { aa[k] = true; }

            writeln("D-AA: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            sleep(2);
            writeln("Sleeping done");
        }

        auto map = radixTreeMap!(Key, Value);
        assert(map.empty);
        static assert(map.hasValue);

        map.insert(Key.init, Value.init);
    }
}

/// test map with bool Values
@safe pure nothrow /* TODO @nogc */ unittest
{
    alias Key = uint;
    alias Value = bool;
    auto map = radixTreeMap!(Key, Value);
    assert(map.empty);
    static assert(map.hasValue);
    map.insert(Key.init, Value.init);
}

/// leak test
version(enterSingleInfiniteMemoryLeakTest)
@safe pure nothrow /* TODO @nogc */ unittest
{
    while (true)
    {
        checkNumeric!(float);
    }
}

///
@safe pure nothrow /* TODO @nogc */ unittest
{
    checkNumeric!(float, double,
                  long, int, short, byte,
                  ulong, uint, ushort, ubyte);
}

auto testPrint(uint span, Keys...)()
    if (Keys.length >= 1)
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        foreach (Key; Keys)
        {
            dln("Key: ", Key.stringof);
            alias Tree = radixTreeSet!(Key);
            auto set = Tree;

            import std.algorithm : min, max;

            static if (isIntegral!Key)
            {
                const low = max(Key.min, -100_000);
                const high = min(Key.max, 100_000);
            }
            else static if (isFloatingPoint!Key)
            {
                const low = -100_000;
                const high = 100_000;
            }

            foreach (const uk; low.iota(high + 1))
            {
                const Key key = cast(Key)uk;
                assert(set.insert(key));  // insert new value returns `true` (previously not in set)
                assert(!set.insert(key)); // reinsert same value returns `false` (already in set)
            }
        }
    }
}

version(print) @safe unittest
{
    testPrint!(8,
               double, float,
               long, int, short, byte,
               ulong, uint, ushort, ubyte);
}

/** Static Iota.
    TODO Move to Phobos std.range.
 */
template iota(size_t from, size_t to)
    if (from <= to)
{
        alias iota = iotaImpl!(to - 1, from);
}
private template iotaImpl(size_t to, size_t now)
{
    import std.meta : AliasSeq;
    static if (now >= to) { alias iotaImpl = AliasSeq!(now); }
    else                  { alias iotaImpl = AliasSeq!(now, iotaImpl!(to, now + 1)); }
}

version(benchmark)
void main(string[] args)
{
    benchmark;
}
