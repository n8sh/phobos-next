/** Tries and Prefix Trees.

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree

    TODO use expandVariableLength in reconstructingInsert that uses x.realloc(2*n) instead of x.free(n)-malloc(2*n)

    TODO Remove @trusted from members of vla and make their callers @trusted instead.

    TODO Assure that ~this() is run for argument `nt` in `freeNode`. Can we use `postblit()` for this?

    TODO Assure that no SparseBranch with no leaf can be converted to SparseLeaf1

    TODO Search for "functionize this loop or reuse memmove" and use move()

    TODO Add Branch-hint allocation flag and re-benchmark construction of radixTreeSet with 10000000 uints

    TODO Add sortedness to `IxsN` and make `IxsN.contains()` use `binarySearch()`. Make use of `sortn`.

    TODO Make `Key` and Ix[]-array of `immutable Ix` like `string`
    TODO Allow NodeType-constructors to take const and immutable prefixes

    TODO Check for case when expanding to bit-branch instead of SparseBranch in all `expand()` overloads

    TODO Make array indexing/slicing as @trusted and use .ptr[] instead of [] when things are stable. Especially in IxsN

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

import std.algorithm : move;
import std.traits : isIntegral, isFloatingPoint, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy, isPointer;
import std.typecons : tuple, Tuple, Unqual;
import std.range : isInputRange, isBidirectionalRange, ElementType;
import std.range.primitives : hasLength;

import bijections : isIntegralBijectableType, bijectToUnsigned;
import variant_ex : WordVariant;
import typecons_ex : StrictlyIndexed;
import modulo : Mod, mod;

// version = enterSingleInfiniteMemoryLeakTest;
// version = debugPrintAllocations;
version = benchmark;
version = print;

import dbg;

alias isFixedTrieableKeyType = isIntegralBijectableType;

enum isTrieableKeyType(T) = (isFixedTrieableKeyType!T ||
                             (isInputRange!T &&
                              isFixedTrieableKeyType!(ElementType!T)));
static assert(isTrieableKeyType!(const(char)[]));
template shouldAddGCRange(T)
{
    import std.traits : isPointer, hasIndirections;
    enum shouldAddGCRange = isPointer!T || hasIndirections!T || is (T == class);
}

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

alias UKey = Key!span;

/** Results of attempt at modification sub. */
enum ModStatus
{
    maxCapacityReached,         // no operation, max capacity reached
    unchanged,                  // no operation, element already stored
    added, inserted = added,    // new element was added (inserted)
    updated, modified = updated, // existing element was update/modified
}

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

    /// ElementType type `Element`.
    static if (L == 1)
        alias Element = Ix;
    else
        alias Element = Ix[L];

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

    @property auto toString(char separator = keySeparator) const
    {
        string s;
        foreach (const i, const ix; chunks)
        {
            if (i != 0) { s ~= separator; }
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

    /** Pop first (front) element. */
    auto ref popFront()
    {
        assert(!empty);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - 1)
        {
            move(_ixs[i + 1], _ixs[i]); // like `_ixs[i] = _ixs[i + 1];` but more generic
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
            move(_ixs[i + n], _ixs[i]); // like `_ixs[i] = _ixs[i + n];` but more generic
        }
        _length = _length - n;
        return this;
    }

    /** Pop last (back) element. */
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

static assert(IxsN!(3, 1, 8).sizeof == 4);
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

    pragma(inline) bool contains(UKey key) const nothrow @nogc { return this.key == key; }

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
        assert(!keys.empty);
        final switch (keys.length)
        {
        case 1:
            return keys.at!0[];
        case 2:
            import std.algorithm : commonPrefix;
            return commonPrefix(keys.at!0[], keys.at!1[]);
        }
    }

    pragma(inline) bool contains(UKey key) const nothrow @nogc
    {
        assert(!keys.empty);
        // final switch (keys.length)
        // {
        // case 1: return keys[0] == key;
        // case 2: return keys[0] == key || keys[1] == key;
        // }
        return keys.contains(key);
    }

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
        assert(!keys.empty);
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

    pragma(inline) bool contains(UKey key) const nothrow @nogc
    {
        assert(!keys.empty);
        // final switch (keys.length)
        // {
        // case 1: return keys[0] == key;
        // case 2: return keys[0] == key || keys[1] == key;
        // case 3: return keys[0] == key || keys[1] == key || keys[2] == key;
        // }
        return keys.contains(key);
    }

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

    pragma(inline) bool contains(Ix key) const nothrow @nogc
    {
        assert(!keys.empty);
        // final switch (keys.length)
        // {
        // case 1: return keys[0] == key;
        // case 2: return keys[0] == key || keys[1] == key;
        // case 3: return keys[0] == key || keys[1] == key || keys[2] == key;
        // case 4: return keys[0] == key || keys[1] == key || keys[2] == key || keys[3] == key;
        // case 5: return keys[0] == key || keys[1] == key || keys[2] == key || keys[3] == key || keys[4] == key;
        // case 6: return keys[0] == key || keys[1] == key || keys[2] == key || keys[3] == key || keys[4] == key || keys[5] == key;
        // case 7: return keys[0] == key || keys[1] == key || keys[2] == key || keys[3] == key || keys[4] == key || keys[5] == key || keys[6] == key;
        // }
        return keys.contains(key);
    }
    pragma(inline) bool contains(UKey key) const nothrow @nogc { return key.length == 1 && keys.contains(key[0]); }

    IxsN!(capacity, 1) keys;
}

/** Sparsely coded leaves with values of type `Value`. */
static private struct SparseLeaf1(Value)
{
    import searching_ex : containsStoreIndex;
    import std.algorithm.sorting : assumeSorted, isSorted;

    enum hasValue = !is(Value == void);

    enum minCapacity = 0;     // preferred minimum number of preallocated values

    // preferred maximum number of preallocated values, if larger use a DenseLeaf1 instead
    static if (hasValue) { enum maxCapacity = 128; }
    else                 { enum maxCapacity = 48; }

    alias Capacity = Mod!(maxCapacity + 1);
    alias Length = Capacity;

    static if (hasValue) alias IxElement = Tuple!(Ix, "ix", Value, "value");
    else                 alias IxElement = Ix;

    pure nothrow /* TODO @nogc */:

    /** Construct empty with `capacity`. */
    pragma(inline) this(size_t capacity)   /* TODO @nogc */
    {
        _capacity = capacity;
        _length = 0;
    }

    static if (hasValue)
    {
        static if (shouldAddGCRange!Value)
        {
            import core.memory : GC;
        }

        /** Construct with capacity `capacity`. */
        this(size_t capacity, Ix[] ixs, Value[] values) /* TODO @nogc */
        in
        {
            assert(ixs.length == values.length);
            assert(capacity >= ixs.length);
            assert(values.length <= maxCapacity);
            assert(ixs.isSorted);
        }
        body
        {
            _capacity = capacity;
            _length = ixs.length;
            foreach (const i, const ix; ixs) { ixsSlots[i] = ix; }
            foreach (const i, const value; values) { valuesSlots[i] = value; }
            static if (shouldAddGCRange!Value)
            {
                GC.addRange(valuesSlots.ptr, _capacity * Value.sizeof);
            }
        }
    }
    else
    {
        this(size_t capacity, Ix[] ixs) /* TODO @nogc */
        in
        {
            assert(capacity >= ixs.length);
            assert(ixs.length <= maxCapacity);
            assert(ixs.isSorted);
        }
        body
        {
            _capacity = capacity;
            _length = ixs.length;
            foreach (const i, const ix; ixs) { ixsSlots[i] = ix; }
        }
    }

    ~this()
    {
        deinit();
    }

    private pragma(inline) void deinit() @trusted
    {
        static if (shouldAddGCRange!Value)
        {
            GC.removeRange(valuesSlots.ptr, _capacity * Value.sizeof);
        }
    }

    auto makeRoom()
    {
        auto next = &this;
        if (full)
        {
            if (length < maxCapacity) // if we can expand more
            {
                import vla : constructVariableLength;
                static if (hasValue)
                {
                    next = constructVariableLength!(typeof(this))(length + 1, ixsSlots, valuesSlots); // make room
                }
                else
                {
                    next = constructVariableLength!(typeof(this))(length + 1, ixsSlots); // make room
                }
                this.deinit(); free(&this); // clear `this`. TODO reuse existing helper function in Phobos?
            }
            else
            {
                return null;    // indicate that max capacity has been reached
            }
        }
        return next;
    }

    /** Insert `key`, possibly self-reallocating `this` (into return). */
    typeof(this)* reconstructingInsert(IxElement elt,
                                       out ModStatus modStatus,
                                       out size_t index) @trusted /* TODO @nogc */
    {
        // get index
        static if (hasValue) { const ix = elt.ix; }
        else                 { const ix = elt; }

        // handle existing element
        if (ixs.assumeSorted.containsStoreIndex(ix, index))
        {
            static if (hasValue)
            {
                modStatus = valuesSlots[index] != elt.value ? ModStatus.updated : ModStatus.unchanged;
                valuesSlots[index] = elt.value;
            }
            else
            {
                modStatus = ModStatus.unchanged;
            }
            return &this;
        }

        // try making room for new element
        auto next = makeRoom();
        if (next is null)
        {
            modStatus = ModStatus.maxCapacityReached; // TODO expand to `DenseLeaf1`
            return &this;
        }

        // insert new element
        next.insertAt(index, elt);
        modStatus = ModStatus.added;

        return next;
    }

    pragma(inline) private void insertAt(size_t index, IxElement elt)
    {
        assert(index <= _length);

        foreach (i; 0 .. _length - index) // TODO functionize this loop or reuse memmove:
        {
            const iD = _length - i;
            const iS = iD - 1;
            ixsSlots[iD] = ixsSlots[iS];
        }

        static if (hasValue)
        {
            ixsSlots[index] = elt.ix;
            valuesSlots[index] = elt.value;
        }
        else
        {
            ixsSlots[index] = elt;
        }

        ++_length;
    }

    pragma(inline) Length length() const @safe @nogc { return _length; }
    pragma(inline) Capacity capacity() const @safe @nogc { return _capacity; }

    pragma(inline) bool empty() const @safe @nogc { return _length == 0; }
    pragma(inline) bool full() const @safe @nogc { return _length == _capacity; }

    /** Get all initialized keys. */
    pragma(inline) auto ixs() inout @trusted @nogc { return ixsSlots[0 .. _length]; }

    static if (hasValue)
    {
        /** Get all intialized values. */
        pragma(inline) auto values() inout @trusted @nogc { return valuesSlots[0 .. _length]; }

        pragma(inline) void setValue(Ix ix, in Value value) @trusted /* TODO @nogc */
        {
            size_t index;
            const hit = ixs.assumeSorted.containsStoreIndex(ix, index);
            assert(hit);        // assert hit for now
            assert(index < length);
            values[index] = value;
        }

        pragma(inline) inout(Value*) contains(Ix key) inout @nogc
        {
            size_t index;
            if (ixs.assumeSorted.containsStoreIndex(key, index))
            {
                return &(values[index]);
            }
            else
            {
                return null;
            }
        }
    }
    else
    {
        pragma(inline) bool contains(Ix key) const @nogc
        {
            return ixs.assumeSorted.contains(key);
        }
    }

    /** Get all reserved keys. */
    private pragma(inline) auto ixsSlots() inout @trusted @nogc
    {
        static if (hasValue) return (cast(Ix*)(_values.ptr + _capacity))[0 .. _capacity];
        else                 return _ixs.ptr[0 .. _capacity];
    }
    static if (hasValue)
    {
        /** Get all reserved values. */
        private pragma(inline) auto valuesSlots() inout @trusted @nogc
        {
            return _values.ptr[0 .. _capacity];
        }
    }

    /** Get allocation size (in bytes) needed to hold `length` number of
        elements (keys and optionally values). */
    static size_t allocationSizeOfCapacity(size_t capacity) @safe pure nothrow @nogc
    {
        static if (hasValue)
        {
            return (this.sizeof + // base plus
                    Value.sizeof*capacity + // actual size of `_values`
                    Ix.sizeof*capacity);   // actual size of `_ixs`
        }
        else
        {
            return (this.sizeof + // base plus
                    Ix.sizeof*capacity);   // actual size of `_ixs`
        }
    }

    /** Get allocated size (in bytes) of `this` including the variable-length part. */
    size_t allocatedSize() const @safe pure nothrow @nogc
    {
        return allocationSizeOfCapacity(_capacity);
    }

private:
    Length _length;
    const Capacity _capacity;
    static if (hasValue)
    {
        Value[0] _values;
    }
    Ix[0] _ixs;
}

/** Densely coded leaves with values of type `Value`. */
static private struct DenseLeaf1(Value)
{
    enum hasValue = !is(Value == void);

    static if (hasValue) alias IxElement = Tuple!(Ix, "ix", Value, "value");
    else                 alias IxElement = Ix;

    enum hasGCScannedValues = hasValue && !is(Value == bool) && shouldAddGCRange!Value;

    static if (hasGCScannedValues)
    {
        import core.memory : GC;
    }

    enum capacity = radix;

    @safe pure nothrow:

    this(Ix[] ixs)
    {
        assert(ixs.length <= capacity);
        foreach (ix; ixs)
        {
            _ixBits[ix] = true;
        }
        static if (hasGCScannedValues)
        {
            GC.addRange(_values.ptr, capacity * Value.size);
        }
    }

    ~this()
    {
        static if (hasGCScannedValues)
        {
            GC.removeRange(_values.ptr, capacity * Value.size);
        }
    }

    @nogc:

    pragma(inline) bool hasSubAt(Ix ix) const { return _ixBits[ix]; }
    pragma(inline) bool empty() const { return _ixBits.empty; }
    pragma(inline) bool full() const { return _ixBits.full; }
    pragma(inline) size_t count() const { return _ixBits.countOnes; }

    static if (hasValue)
    {
        pragma(inline) inout(Value*) contains(Ix ix) inout @nogc
        {
            return _ixBits[ix] ? &(_values[ix]) : null;
        }
    }
    else
    {
        pragma(inline) bool contains(Ix ix) const { return _ixBits[ix]; }
    }

    pragma(inline) ModStatus insert(IxElement elt)
    {
        ModStatus modStatus;

        static if (hasValue) { const ix = elt.ix; }
        else                 { const ix = elt; }

        if (contains(ix))
        {
            static if (hasValue)
            {
                modStatus = _values[ix] != elt.value ? ModStatus.updated : ModStatus.unchanged;
            }
            else
            {
                modStatus = ModStatus.unchanged;
            }
        }
        else
        {
            _ixBits[ix] = true;
            modStatus = ModStatus.added;
        }

        // set element
        static if (hasValue)
        {
            _values[ix] = elt.value;
        }

        return modStatus;
    }

    static if (hasValue)
    {
        /// Get value at index `ix`.
        auto ref getValue(Ix ix) inout { return _values[ix]; }
        /// Set value at index `ix` to `value`.
        void setValue(Ix ix, in Value value) { _values[ix] = value; }

        auto values() const { return _values; }
    }

private:
    import bitset : BitSet;
    BitSet!capacity _ixBits;  // 32 bytes
    static if (hasValue)
    {
        // static if (is(Value == bool))
        // {
        //     BitSet!capacity _values; // packed values
        // }
        // else
        // {
        //     Value[capacity] _values;
        // }
        Value[capacity] _values;
    }
}

/** Mutable leaf node of 1-Ix leaves. */
alias Leaf(Value) = WordVariant!(HeptLeaf1,
                                 SparseLeaf1!Value*,
                                 DenseLeaf1!Value*);

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
    import vla : hasVariableLength;

    /** Is `true` if this tree stores values of type `Value` along with keys. In
        other words: `this` is a $(I map) rather than a $(I set).
    */
    enum hasValue = !is(Value == void);

    static if (hasValue)
    {
        alias Element = Tuple!(UKey, "key", Value, "value");
        alias IxElement = Tuple!(Ix, "ix", Value, "value");
    }
    else
    {
        alias Element = UKey;
        alias IxElement = Ix;
    }

    auto elementIx(IxElement elt)
    {
        static if (hasValue)
            return elt.ix;
        else
            return elt;
    }

    auto elementKey(Element elt)
    {
        static if (hasValue)
            return elt.key;
        else
            return elt;
    }

    auto elementKeyDropExactly(Element elt, size_t n)
    {
        static if (hasValue)
            return Element(elt.key[n .. $], elt.value);
        else
            return elt[n .. $];
    }

    /// `true` if tree has binary branch.
    enum isBinary = span == 2;

    // TODO make these run-time arguments at different key depths and map to statistics of typed-key
    alias DefaultBranch = SparseBranch*; // either SparseBranch*, DenseBranch*
    alias DefaultLeaf = SparseLeaf1!Value*; // either SparseLeaf1*, DenseLeaf1*

    /** Mutable node. */
    alias Node = WordVariant!(OneLeafMax7,
                              TwoLeaf3,
                              TriLeaf2,

                              HeptLeaf1,
                              SparseLeaf1!Value*,
                              DenseLeaf1!Value*,

                              SparseBranch*,
                              DenseBranch*);

    /** Mutable branch node. */
    alias Branch = WordVariant!(SparseBranch*,
                                DenseBranch*);

    static assert(Node.typeBits <= IxsN!(7, 1, 8).typeBits);
    static assert(Leaf!Value.typeBits <= IxsN!(7, 1, 8).typeBits);
    static assert(Branch.typeBits <= IxsN!(7, 1, 8).typeBits);

    alias Sub = Tuple!(Ix, Node);

    /** Constant node. */
    // TODO make work with indexNaming
    // alias ConstNodePtr = WordVariant!(staticMap!(ConstOf, Node));

    static assert(span <= 8*Ix.sizeof, "Need more precision in Ix");

    import std.array : Appender;

    /** Element (Search) Reference. */
    struct EltRef
    {
        Node node;
        Ix ix; // `Node`-specific counter, typically either a sparse or dense index either a sub-branch or a `UKey`-ending `Ix`
        ModStatus modStatus;

        @safe pure nothrow:

        pragma(inline) bool opCast(T : bool)() const @nogc { return cast(bool)node; }

        void appendToKey(ref Appender!UKey key) const /* TODO @nogc */
        {
            with (Node.Ix)
            {
                final switch (node.typeIx)
                {
                case undefined:
                    assert(false);

                // word-packed leaf
                case ix_OneLeafMax7:
                    assert(ix == 0);
                    key.put(node.as!(OneLeafMax7).key);
                    break;
                case ix_TwoLeaf3:
                    key.put(node.as!(TwoLeaf3).keys[ix][]);
                    break;
                case ix_TriLeaf2:
                    key.put(node.as!(TriLeaf2).keys[ix][]);
                    break;
                case ix_HeptLeaf1:
                    key.put(node.as!(HeptLeaf1).keys[ix]);
                    break;

                // heap-allocated leaf
                case ix_SparseLeaf1Ptr:
                    key.put(node.as!(SparseLeaf1!Value*).ixs[ix]);
                    break;
                case ix_DenseLeaf1Ptr:
                    key.put(ix);
                    break;

                // heap-allocated branch branch
                case ix_SparseBranchPtr:
                    auto node_ = node.as!(SparseBranch*);
                    key.put(node_.prefix);
                    key.put(node_.subIxs[ix]);
                    break;
                case ix_DenseBranchPtr:
                    auto node_ = node.as!(DenseBranch*);
                    key.put(node_.prefix);
                    key.put(ix);
                    break;
               }
            }
        }

        static if (hasValue)
        {
            auto ref value() inout
            {
                with (Node.Ix)
                {
                    switch (node.typeIx)
                    {
                    case ix_SparseLeaf1Ptr: return node.as!(SparseLeaf1!Value*).values[ix];
                    case ix_DenseLeaf1Ptr: return node.as!(DenseLeaf1!Value*).values[ix];
                    default: assert(false, "Incorrect Leaf-type");
                    }
                }
            }
        }
    }

    /** Tree Iterator. */
    alias Iterator = Appender!(EltRef[]);

    /** Range over the Elements in a Radix Tree.
        Fulfills `isBidirectionalRange`.
    */
    struct Range
    {
        @safe pure nothrow /* @nogc */:
        pragma(inline):

        this(Node root)
        {
            Node curr = root;

            while (true)
            {
                with (Node.Ix)
                {
                    final switch (curr.typeIx)
                    {
                    case undefined:
                        assert(false);

                        // word-packed leaf
                    case ix_OneLeafMax7:
                        break;
                    case ix_TwoLeaf3:
                        break;
                    case ix_TriLeaf2:
                        break;
                    case ix_HeptLeaf1:
                        break;

                        // heap-allocated leaf
                    case ix_SparseLeaf1Ptr:
                        break;
                    case ix_DenseLeaf1Ptr:
                        break;

                        // heap-allocated branch branch
                    case ix_SparseBranchPtr:
                        auto node_ = curr.as!(SparseBranch*);
                        break;
                    case ix_DenseBranchPtr:
                        auto node_ = curr.as!(DenseBranch*);
                        break;
                    }
                }
            }

            copyFrontElement;
            copyBackElement;
        }

        bool empty() const
        {
            return _front == _back;
        }

        auto ref front() inout @safe pure nothrow @nogc { return _frontKey; }
        auto ref back() inout @safe pure nothrow @nogc { return _backKey; }

        void popFront()
        {
            assert(!empty);
            dln("TODO Iterate _front");
            copyFrontElement;
        }

        void popBack()
        {
            assert(!empty);
            dln("TODO Iterate _back");
            copyBackElement;
        }

        private void copyFrontElement()
        {
            _frontKey.clear;
            foreach (const eltRef; _front.data) { eltRef.appendToKey(_frontKey); }
            // if (hasFixedKeyLength) { assert(_frontKey.data.length == fixedKeyLength); }
            static if (hasValue)
            {
                _frontValue = _front.data[$ - 1].value; // last should be leaf containing value
            }
        }

        private void copyBackElement()
        {
            _backKey.clear;
            foreach (const eltRef; _back.data)  { eltRef.appendToKey(_backKey); }
            // if (hasFixedKeyLength) { assert(_backKey.data.length == fixedKeyLength); }
            static if (hasValue)
            {
                _backValue = _back.data[$ - 1].value;   // last should be leaf containgin value
            }
        }

    private:
        Iterator _front;
        Iterator _back;

        Appender!UKey _frontKey; // copy of front key
        Appender!UKey _backKey;  // copy of back key
        static if (hasValue)
        {
            Value _frontValue;             // copy of front value
            Value _backValue;             // copy of back value
        }
    }

    pragma(inline) Range opSlice() @trusted pure nothrow
    {
        return Range(this._root);
    }

    // static assert(isBidirectionalRange!Range);

    /** Sparse-Branch population histogram.
    */
    alias SparseLeaf1_PopHist = size_t[radix];

    /** 256-Branch population histogram.
     */
    alias DenseLeaf1_PopHist = size_t[radix];

    /** 4-Branch population histogram.
        Index maps to population with value range (0 .. 4).
    */
    alias SparseBranch_PopHist = size_t[SparseBranch.maxCapacity + 1];

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

        import typecons_ex : IndexedArray;

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));

        IndexedArray!(size_t, Leaf!Value.Ix) popByLeafType;
        static assert(is(typeof(popByLeafType).Index == Leaf!Value.Ix));

        /// Number of heap-allocated `Node`s. Should always equal `heapNodeAllocationBalance`.
        size_t heapNodeCount;

        /// Number of heap-allocated `Leaf`s. Should always equal `heapLeafAllocationBalance`.
        size_t heapLeafCount;

        size_t sparseBranchSizeSum;
        size_t sparseLeaf1SizeSum;
    }

    /** Sparse/Packed dynamically sized branch implemented as variable-length
        struct.
    */
    static private struct SparseBranch
    {
        import std.algorithm.sorting : isSorted;

        enum minCapacity = 0; // minimum number of preallocated sub-indexes and sub-nodes
        enum maxCapacity = 48; // maximum number of preallocated sub-indexes and sub-nodes
        enum prefixCapacity = 5; // 5, 13, 21, ...

        alias Count = Mod!(maxCapacity + 1);

        @safe pure nothrow:

        pragma(inline) this(size_t subCapacity)
        {
            initialize(subCapacity);
        }

        pragma(inline) this(size_t subCapacity, const Ix[] prefix, Leaf!Value leaf)
        {
            initialize(subCapacity);
            this.prefix = prefix;
            this.leaf = leaf;
        }

        pragma(inline) this(size_t subCapacity, const Ix[] prefix)
        {
            initialize(subCapacity);
            this.prefix = prefix;
        }

        pragma(inline) this(size_t subCapacity, Leaf!Value leaf)
        {
            initialize(subCapacity);
            this.leaf = leaf;
        }

        pragma(inline) this(size_t subCapacity, const Ix[] prefix, Sub sub)
        {
            assert(subCapacity);

            initialize(subCapacity);

            this.prefix = prefix;
            this.subCount = 1;
            this.subIxSlots[0] = sub[0];
            this.subNodeSlots[0] = sub[1];
        }

        pragma(inline) this(size_t subCapacity, typeof(this)* rhs)
        in
        {
            assert(subCapacity > rhs.subCapacity);
            assert(rhs);
        }
        body
        {
            // these two must be in this order:
            // 1.
            move(rhs.leaf, this.leaf);
            debug rhs.leaf = typeof(rhs.leaf).init;
            this.subCount = rhs.subCount;
            move(rhs.prefix, this.prefix);

            // 2.
            initialize(subCapacity);

            // copy variable length part. TODO optimize:
            this.subIxSlots[0 .. rhs.subCount] = rhs.subIxSlots[0 .. rhs.subCount];
            this.subNodeSlots[0 .. rhs.subCount] = rhs.subNodeSlots[0 .. rhs.subCount];

            assert(this.subCapacity > rhs.subCapacity);
        }

        private pragma(inline) void initialize(size_t subCapacity)
        {
            this.subCapacity = subCapacity;
            debug
            {
                // zero-initialize variable-length part
                subIxSlots[] = Ix.init;
                subNodeSlots[] = Node.init;
            }
        }

        ~this() { deinit(); }

        private pragma(inline) void deinit() { /* nothing for now */ }

        /** Insert `sub`, possibly self-reallocating `this` (into return).
        */
        typeof(this)* reconstructingInsert(Sub sub,
                                           out ModStatus modStatus,
                                           out size_t index) @trusted /* TODO @nogc */
        {
            auto next = &this;

            import searching_ex : containsStoreIndex;
            if (subIxs.containsStoreIndex(sub[0], index))
            {
                assert(subIxSlots[index] == sub[0]); // subIxSlots[index] = sub[0];
                subNodeSlots[index] = sub[1];
                modStatus = ModStatus.updated;
                return next;
            }

            if (full)
            {
                if (subCount < maxCapacity) // if we can expand more
                {
                    import vla : constructVariableLength;
                    next = constructVariableLength!(typeof(this))(subCount + 1, &this);

                    this.deinit(); free(&this); // clear `this`. TODO reuse existing helper function in Phobos?
                }
                else
                {
                    modStatus = ModStatus.maxCapacityReached; // TODO expand to `DenseBranch`
                    return next;
                }
            }

            next.insertAt(index, sub);
            modStatus = ModStatus.added;
            return next;
        }

        pragma(inline) private void insertAt(size_t index, Sub sub)
        {
            assert(index <= subCount);
            foreach (i; 0 .. subCount - index) // TODO functionize this loop or reuse memmove:
            {
                const iD = subCount - i;
                const iS = iD - 1;
                subIxSlots[iD] = subIxSlots[iS];
                subNodeSlots[iD] = subNodeSlots[iS];
            }
            subIxSlots[index] = sub[0]; // set new element
            subNodeSlots[index] = sub[1]; // set new element
            ++subCount;
        }

        inout(Node) containsSubAt(Ix ix) inout
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

        /** Get all sub-`Ix` slots, both initialized and uninitialized. */
        private auto ref subIxSlots() inout @trusted pure nothrow
        {
            return (cast(Ix*)(_subNodeSlots0.ptr + subCapacity))[0 .. subCapacity];
        }
        /** Get all sub-`Node` slots, both initialized and uninitialized. */
        private auto ref subNodeSlots() inout @trusted pure nothrow
        {
            return _subNodeSlots0.ptr[0 .. subCapacity];
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
        static size_t allocationSizeOfCapacity(size_t capacity) @safe pure nothrow @nogc
        {
            return (this.sizeof + // base plus
                    Node.sizeof*capacity + // actual size of `_subNodeSlots0`
                    Ix.sizeof*capacity);   // actual size of `_subIxSlots0`
        }

        /** Get allocated size (in bytes) of `this` including the variable-length part. */
        size_t allocatedSize() const @safe pure nothrow @nogc
        {
            return allocationSizeOfCapacity(subCapacity);
        }

    private:

        // members in order of decreasing `alignof`:
        Leaf!Value leaf;

        IxsN!prefixCapacity prefix; // prefix common to all `subNodes` (also called edge-label)
        Count subCount;
        Count subCapacity;
        static assert(prefix.sizeof + subCount.sizeof + subCapacity.sizeof == 8); // assert alignment

        // variable-length part
        Node[0] _subNodeSlots0;
        Ix[0] _subIxSlots0;     // needs to special alignment
    }

    static assert(hasVariableLength!SparseBranch);
    static if (!hasValue) { static assert(SparseBranch.sizeof == 16); }

    /** Dense/Unpacked `radix`-branch with `radix` number of sub-nodes. */
    static private struct DenseBranch
    {
        enum maxCapacity = 256;
        enum prefixCapacity = 15; // 7, 15, 23, ..., we can afford larger prefix here because DenseBranch is so large

        @safe pure nothrow:

        this(const Ix[] prefix)
        {
            this.prefix = prefix;
        }

        this(const Ix[] prefix, Sub sub)
        {
            this(prefix);
            this.subNodes[sub[0]] = sub[1];
        }

        this(const Ix[] prefix, Sub subA, Sub subB)
        {
            assert(subA[0] != subB[0]); // disjunct indexes
            assert(subA[1] != subB[1]); // disjunct nodes

            this.subNodes[subA[0]] = subA[1];
            this.subNodes[subB[0]] = subB[1];
        }

        this(SparseBranch* rhs)
        {
            this.prefix = rhs.prefix;

            // move leaf
            move(rhs.leaf, this.leaf);
            debug rhs.leaf = Leaf!Value.init; // make reference unique, to be on the safe side

            foreach (const i; 0 .. rhs.subCount) // each sub node. TODO use iota!(Mod!N)
            {
                const iN = (cast(ubyte)i).mod!(SparseBranch.maxCapacity);
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
        Leaf!Value leaf;
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

    /// ditto
    Branch setSub(SparseBranch* curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        size_t insertionIndex;
        ModStatus modStatus;
        curr = curr.reconstructingInsert(Sub(subIx, subNode), modStatus, insertionIndex);
        if (modStatus == ModStatus.maxCapacityReached) // try insert and if it fails
        {
            // we need to expand because `curr` is full
            auto next = expand(curr);
            assert(getSub(next, subIx) == Node.init); // key slot should be unoccupied
            return setSub(next, subIx, subNode);
        }
        return Branch(curr);
    }
    /// ditto
    pragma(inline) Branch setSub(DenseBranch* curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        try
        {
            assert(!curr.subNodes[subIx],
                   "sub-Node at index " ~ subIx.to!string ~
                   " already set to " ~ subNode.to!string);
        }
        catch (Exception e) {}
        curr.subNodes[subIx] = subNode;
        return Branch(curr);
    }

    /** Set sub-`Node` of branch `Node curr` at index `ix` to `subNode`. */
    pragma(inline) Branch setSub(Branch curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        final switch (curr.typeIx) with (Branch.Ix)
        {
        case ix_SparseBranchPtr: return setSub(curr.as!(SparseBranch*), subIx, subNode);
        case ix_DenseBranchPtr: return setSub(curr.as!(DenseBranch*), subIx, subNode);
        case undefined: assert(false);
        }
    }

    /** Get sub-`Node` of branch `Node curr` at index `subIx`. */
    pragma(inline) Node getSub(Branch curr, Ix subIx) @safe pure nothrow
    {
        final switch (curr.typeIx) with (Branch.Ix)
        {
        case ix_SparseBranchPtr: return getSub(curr.as!(SparseBranch*), subIx);
        case ix_DenseBranchPtr: return getSub(curr.as!(DenseBranch*), subIx);
        case undefined: assert(false);
        }
    }
    /// ditto
    pragma(inline) Node getSub(SparseBranch* curr, Ix subIx) @safe pure nothrow
    {
        if (auto subNode = curr.containsSubAt(subIx))
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
    pragma(inline) inout(Leaf!Value) getLeaf(inout Branch curr) @safe pure nothrow
    {
        final switch (curr.typeIx) with (Branch.Ix)
        {
        case ix_SparseBranchPtr: return curr.as!(SparseBranch*).leaf;
        case ix_DenseBranchPtr: return curr.as!(DenseBranch*).leaf;
        case undefined: assert(false);
        }
    }

    /** Set leaves node of node `curr` to `leaf`. */
    pragma(inline) void setLeaf(Branch curr, Leaf!Value leaf) @safe pure nothrow
    {
        final switch (curr.typeIx) with (Branch.Ix)
        {
        case ix_SparseBranchPtr: curr.as!(SparseBranch*).leaf = leaf; break;
        case ix_DenseBranchPtr: curr.as!(DenseBranch*).leaf = leaf; break;
        case undefined: assert(false);
        }
    }

    /** Get prefix of node `curr`. */
    pragma(inline) auto getPrefix(inout Branch curr) @safe pure nothrow
    {
        final switch (curr.typeIx) with (Branch.Ix)
        {
        case ix_SparseBranchPtr: return curr.as!(SparseBranch*).prefix[];
        case ix_DenseBranchPtr: return curr.as!(DenseBranch*).prefix[];
        case undefined: assert(false);
        }
    }

    /** Set prefix of branch node `curr` to `prefix`. */
    pragma(inline) void setPrefix(Branch curr, const Ix[] prefix) @safe pure nothrow
    {
        final switch (curr.typeIx) with (Branch.Ix)
        {
        case ix_SparseBranchPtr: curr.as!(SparseBranch*).prefix = typeof(curr.as!(SparseBranch*).prefix)(prefix); break;
        case ix_DenseBranchPtr: curr.as!(DenseBranch*).prefix = typeof(curr.as!(DenseBranch*).prefix)(prefix); break;
        case undefined: assert(false);
        }
    }

    /** Pop `n` from prefix. */
    pragma(inline) void popFrontNPrefix(Branch curr, size_t n) @safe pure nothrow
    {
        final switch (curr.typeIx) with (Branch.Ix)
        {
        case ix_SparseBranchPtr: curr.as!(SparseBranch*).prefix.popFrontN(n); break;
        case ix_DenseBranchPtr: curr.as!(DenseBranch*).prefix.popFrontN(n); break;
        case undefined: assert(false);
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

    @safe pure nothrow /* TODO @nogc */
    {
        static if (hasValue)
        {
            /** Returns: `true` if `key` is stored, `false` otherwise. */
            pragma(inline) inout(Value*) contains(UKey key) inout
            {
                return containsAt(_root, key);
            }

            /** Returns: `true` if `key` is stored under `curr`, `false` otherwise. */
            pragma(inline) inout(Value*) containsAt(Leaf!Value curr, UKey key) inout
            {
                debug if (willFail) { dln("key:", key); }
                switch (curr.typeIx) with (Leaf!Value.Ix)
                {
                case undefined: return null;
                case ix_SparseLeaf1Ptr: return key.length == 1 ? curr.as!(SparseLeaf1!Value*).contains(key[0]) : null;
                case ix_DenseLeaf1Ptr:  return key.length == 1 ? curr.as!(DenseLeaf1!Value*).contains(key[0]) : null;
                default: assert(false);
                }
            }

            /// ditto
            pragma(inline) inout(Value*) containsAt(Node curr, UKey key) inout
            {
                assert(key.length);
                debug if (willFail) { dln("key:", key); }
                import std.algorithm : skipOver;
                switch (curr.typeIx) with (Node.Ix)
                {
                case undefined: return null;
                case ix_SparseLeaf1Ptr: return key.length == 1 ? curr.as!(SparseLeaf1!Value*).contains(key[0]) : null;
                case ix_DenseLeaf1Ptr:  return key.length == 1 ? curr.as!(DenseLeaf1!Value*).contains(key[0]) : null;
                case ix_SparseBranchPtr:
                    auto curr_ = curr.as!(SparseBranch*);
                    if (key.skipOver(curr_.prefix))
                    {
                        return (key.length == 1 ?
                                containsAt(curr_.leaf, key) : // in leaf
                                containsAt(curr_.containsSubAt(key[0]), key[1 .. $])); // recurse into branch tree
                    }
                    return null;
                case ix_DenseBranchPtr:
                    auto curr_ = curr.as!(DenseBranch*);
                    if (key.skipOver(curr_.prefix))
                    {
                        return (key.length == 1 ?
                                containsAt(curr_.leaf, key) : // in leaf
                                containsAt(curr_.subNodes[key[0]], key[1 .. $])); // recurse into branch tree
                    }
                    return null;
                default: assert(false);
                }
            }
        }
        else
        {
            const:

            /** Returns: `true` if `key` is stored, `false` otherwise. */
            pragma(inline) bool contains(UKey key)
            {
                return containsAt(_root, key);
            }

            /** Returns: `true` if `key` is stored under `curr`, `false` otherwise. */
            pragma(inline) bool containsAt(Leaf!Value curr, UKey key)
            {
                debug if (willFail) { dln("key:", key); }
                final switch (curr.typeIx) with (Leaf!Value.Ix)
                {
                case undefined: return false;
                case ix_HeptLeaf1: return curr.as!(HeptLeaf1).contains(key);
                case ix_SparseLeaf1Ptr: return key.length == 1 && curr.as!(SparseLeaf1!Value*).contains(key[0]);
                case ix_DenseLeaf1Ptr:  return key.length == 1 && curr.as!(DenseLeaf1!Value*).contains(key[0]);
                }
            }
            /// ditto
            pragma(inline) bool containsAt(Node curr, UKey key)
            {
                assert(key.length);
                debug if (willFail) { dln("key:", key); }
                import std.algorithm : skipOver;
                final switch (curr.typeIx) with (Node.Ix)
                {
                case undefined: return false;
                case ix_OneLeafMax7: return curr.as!(OneLeafMax7).contains(key);
                case ix_TwoLeaf3: return curr.as!(TwoLeaf3).contains(key);
                case ix_TriLeaf2: return curr.as!(TriLeaf2).contains(key);
                case ix_HeptLeaf1: return curr.as!(HeptLeaf1).contains(key);
                case ix_SparseLeaf1Ptr:
                    return key.length == 1 && curr.as!(SparseLeaf1!Value*).contains(key[0]);
                case ix_DenseLeaf1Ptr:
                    return key.length == 1 && curr.as!(DenseLeaf1!Value*).contains(key[0]);
                case ix_SparseBranchPtr:
                    auto curr_ = curr.as!(SparseBranch*);
                    return (key.skipOver(curr_.prefix) &&        // matching prefix
                            (key.length == 1 ?
                             containsAt(curr_.leaf, key) : // in leaf
                             containsAt(curr_.containsSubAt(key[0]), key[1 .. $]))); // recurse into branch tree
                case ix_DenseBranchPtr:
                    auto curr_ = curr.as!(DenseBranch*);
                    return (key.skipOver(curr_.prefix) &&        // matching prefix
                            (key.length == 1 ?
                             containsAt(curr_.leaf, key) : // in leaf
                             containsAt(curr_.subNodes[key[0]], key[1 .. $]))); // recurse into branch tree
                }
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
        static if (hasValue)
        {
            pragma(inline) Node insert(UKey key, in Value value, out EltRef eltRef)
            {
                return _root = insertAt(_root, Element(key, value), eltRef);
            }
        }
        else
        {
            pragma(inline) Node insert(UKey key, out EltRef eltRef)
            {
                return _root = insertAt(_root, key, eltRef);
            }

            Node insertNew(UKey key, out EltRef eltRef)
            {
                Node next;
                debug if (willFail) { dln("WILL FAIL: key:", key); }
                switch (key.length)
                {
                case 0: assert(false, "key must not be empty"); // return eltRef = Node(construct!(OneLeafMax7)());
                case 1: next = Node(construct!(HeptLeaf1)(key[0])); break;
                case 2: next = Node(construct!(TriLeaf2)(key)); break;
                case 3: next = Node(construct!(TwoLeaf3)(key)); break;
                default:
                    if (key.length <= OneLeafMax7.capacity)
                    {
                        next = Node(construct!(OneLeafMax7)(key));
                        break;
                    }
                    else                // key doesn't fit in a `OneLeafMax7`
                    {
                        return Node(insertNewBranch(key, eltRef));
                    }
                }
                eltRef = EltRef(next,
                                Ix(0), // always first index
                                ModStatus.added);
                return next;
            }
        }

        Branch insertNewBranch(Element elt, out EltRef eltRef)
        {
            debug if (willFail) { dln("WILL FAIL: elt:", elt); }
            auto key = elementKey(elt);
            assert(key);
            import std.algorithm : min;
            const prefixLength = min(key.length - 1, // all but last Ix of key
                                     DefaultBranch.prefixCapacity); // as much as possible of key in branch prefix
            auto prefix = key[0 .. prefixLength];
            typeof(return) next = insertAtBranchBelowPrefix(Branch(constructVariableLength!(DefaultBranch)(1, prefix)),
                                                            elementKeyDropExactly(elt, prefixLength), eltRef);
            assert(eltRef);
            return next;
        }

        /** Insert `key` into sub-tree under root `curr`. */
        pragma(inline) Node insertAt(Node curr, Element elt, out EltRef eltRef)
        {
            debug if (willFail) { dln("WILL FAIL: elt:", elt, " curr:", curr); }
            auto key = elementKey(elt);
            assert(key.length);

            if (!curr)          // if no existing `Node` to insert at
            {
                static if (hasValue)
                {
                    auto next = Node(insertNewBranch(elt, eltRef));
                }
                else
                {
                    auto next = insertNew(key, eltRef);
                }
                assert(eltRef); // must be added to new Node
                return next;
            }
            else
            {
                final switch (curr.typeIx) with (Node.Ix)
                {
                case undefined:
                    return typeof(return).init;
                case ix_OneLeafMax7:
                    static if (hasValue)
                        assert(false);
                    else
                        return insertAt(curr.as!(OneLeafMax7), key, eltRef);
                case ix_TwoLeaf3:
                    static if (hasValue)
                        assert(false);
                    else
                        return insertAt(curr.as!(TwoLeaf3), key, eltRef);
                case ix_TriLeaf2:
                    static if (hasValue)
                        assert(false);
                    else
                        return insertAt(curr.as!(TriLeaf2), key, eltRef);
                case ix_HeptLeaf1:
                    static if (hasValue)
                        assert(false);
                    else
                        return insertAt(curr.as!(HeptLeaf1), key, eltRef);
                case ix_SparseLeaf1Ptr:
                    return insertAtLeaf(Leaf!Value(curr.as!(SparseLeaf1!Value*)), elt, eltRef); // TODO use toLeaf(curr)
                case ix_DenseLeaf1Ptr:
                    return insertAtLeaf(Leaf!Value(curr.as!(DenseLeaf1!Value*)), elt, eltRef); // TODO use toLeaf(curr)
                case ix_SparseBranchPtr:
                    return Node(insertAtBranchAbovePrefix(Branch(curr.as!(SparseBranch*)), elt, eltRef));
                case ix_DenseBranchPtr:
                    return Node(insertAtBranchAbovePrefix(Branch(curr.as!(DenseBranch*)), elt, eltRef));
                }
            }
        }

        /** Insert `key` into sub-tree under branch `curr` above prefix, that is
            the prefix of `curr` is stripped from `key` prior to insertion. */
        Branch insertAtBranchAbovePrefix(Branch curr, Element elt, out EltRef eltRef)
        {
            auto key = elementKey(elt);
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
                    return insertAtBranchBelowPrefix(curr, elt, eltRef);
                }
                else  // if (currPrefix.length >= 1) // non-empty current prefix
                {
                    // NOTE: prefix:"ab", key:"cd"
                    auto currSubIx = currPrefix[0]; // subIx = 'a'
                    popFrontNPrefix(curr, 1);
                    auto next = constructVariableLength!(DefaultBranch)(2, null,
                                                                        Sub(currSubIx, Node(curr)));
                    return insertAtBranchAbovePrefix(typeof(return)(next), elt, eltRef);
                }
            }
            else if (matchedKeyPrefix.length < key.length)
            {
                if (matchedKeyPrefix.length == currPrefix.length)
                {
                    // NOTE: key is an extension of prefix: prefix:"ab", key:"abcd"
                    return insertAtBranchBelowPrefix(curr, elementKeyDropExactly(elt, currPrefix.length), eltRef);
                }
                else
                {
                    // NOTE: prefix and key share beginning: prefix:"ab11", key:"ab22"
                    auto currSubIx = currPrefix[matchedKeyPrefix.length]; // need index first before we modify curr.prefix
                    popFrontNPrefix(curr, matchedKeyPrefix.length + 1);
                    auto next = constructVariableLength!(DefaultBranch)(2, matchedKeyPrefix,
                                                                        Sub(currSubIx, Node(curr)));
                    return insertAtBranchBelowPrefix(typeof(return)(next), elementKeyDropExactly(elt, matchedKeyPrefix.length), eltRef);
                }
            }
            else // if (matchedKeyPrefix.length == key.length)
            {
                assert(matchedKeyPrefix.length == key.length);
                if (matchedKeyPrefix.length < currPrefix.length)
                {
                    // NOTE: prefix is an extension of key: prefix:"abcd", key:"ab"
                    assert(matchedKeyPrefix.length);
                    const nextPrefixLength = matchedKeyPrefix.length - 1;
                    auto currSubIx = currPrefix[nextPrefixLength]; // need index first
                    popFrontNPrefix(curr, matchedKeyPrefix.length); // drop matchedKeyPrefix plus index to next super branch
                    auto next = constructVariableLength!(DefaultBranch)(2, matchedKeyPrefix[0 .. $ - 1],
                                                                        Sub(currSubIx, Node(curr)));
                    return insertAtBranchBelowPrefix(typeof(return)(next), elementKeyDropExactly(elt, nextPrefixLength), eltRef);
                }
                else /* if (matchedKeyPrefix.length == currPrefix.length) and in turn
                        if (key.length == currPrefix.length */
                {
                    // NOTE: prefix equals key: prefix:"abcd", key:"abcd"
                    assert(matchedKeyPrefix.length);
                    auto currSubIx = currPrefix[matchedKeyPrefix.length - 1]; // need index first
                    popFrontNPrefix(curr, matchedKeyPrefix.length); // drop matchedKeyPrefix plus index to next super branch
                    auto next = constructVariableLength!(DefaultBranch)(2, matchedKeyPrefix[0 .. $ - 1],
                                                                        Sub(currSubIx, Node(curr)));
                    static if (hasValue)
                        return insertAtLeafOfBranch(typeof(return)(next), key[$ - 1], elt.value, eltRef);
                    else
                        return insertAtLeafOfBranch(typeof(return)(next), key[$ - 1], eltRef);
                }
            }
        }

        /** Like `insertAtBranchAbovePrefix` but also asserts that `key` is
            currently not stored under `curr`. */
        pragma(inline) Branch insertNewAtBranchAbovePrefix(Branch curr, Element elt)
        {
            EltRef eltRef;
            auto next = insertAtBranchAbovePrefix(curr, elt, eltRef);
            assert(eltRef);
            return next;
        }

        /** Insert `key` into sub-tree under branch `curr` below prefix, that is
            the prefix of `curr` is not stripped from `key` prior to
            insertion. */
        Branch insertAtBranchBelowPrefix(Branch curr, Element elt, out EltRef eltRef)
        {
            auto key = elementKey(elt);
            assert(key.length);
            debug if (willFail) { dln("WILL FAIL: key:", key,
                                      " curr:", curr,
                                      " currPrefix:", getPrefix(curr),
                                      " eltRef:", eltRef); }
            if (key.length == 1)
            {
                static if (hasValue)
                    return insertAtLeafOfBranch(curr, key[0], elt.value, eltRef);
                else
                    return insertAtLeafOfBranch(curr, key[0], eltRef);
            }
            else
            {
                const subIx = key[0];
                static if (hasValue)
                    return setSub(curr, subIx,
                                  insertAt(getSub(curr, subIx), // recurse
                                           Element(key[1 .. $], elt.value),
                                           eltRef));
                else
                    return setSub(curr, subIx,
                                  insertAt(getSub(curr, subIx), // recurse
                                           key[1 .. $],
                                           eltRef));
            }
        }

        pragma(inline) Branch insertNewAtBranchBelowPrefix(Branch curr, Element elt)
        {
            EltRef eltRef;
            auto next = insertAtBranchBelowPrefix(curr, elt, eltRef);
            assert(eltRef);
            return next;
        }

        Leaf!Value insertIxAtLeaftoLeaf(Leaf!Value curr, IxElement elt, out EltRef eltRef)
        {
            auto key = elementIx(elt);
            debug if (willFail) { dln("WILL FAIL: elt:", elt,
                                      " curr:", curr,
                                      " eltRef:", eltRef); }
            switch (curr.typeIx) with (Leaf!Value.Ix)
            {
            case undefined:
                return typeof(return).init;
            case ix_HeptLeaf1:
                static if (hasValue)
                {
                    assert(false);
                }
                else
                {
                    return insertAt(curr.as!(HeptLeaf1), key, eltRef); // possibly expanded to other Leaf!Value
                }
            case ix_SparseLeaf1Ptr:
                auto curr_ = curr.as!(SparseLeaf1!Value*);
                size_t index;
                ModStatus modStatus;
                curr_ = curr_.reconstructingInsert(elt, modStatus, index);
                curr = Leaf!Value(curr_);
                final switch (modStatus)
                {
                case ModStatus.unchanged: // already stored at `index`
                    eltRef = EltRef(Node(curr_), Ix(index), modStatus);
                    return curr;
                case ModStatus.added:
                    eltRef = EltRef(Node(curr_), Ix(index), modStatus);
                    return curr;
                case ModStatus.maxCapacityReached:
                    auto next = insertIxAtLeaftoLeaf(expand(curr_, 1), // make room for one more
                                                     elt, eltRef);
                    assert(next.peek!(DenseLeaf1!Value*));
                    return next;
                case ModStatus.updated:
                    eltRef = EltRef(Node(curr_), Ix(index), modStatus);
                    return curr;
                }
            case ix_DenseLeaf1Ptr:
                const modStatus = curr.as!(DenseLeaf1!Value*).insert(elt);
                static if (hasValue) { const ix = elt.ix; }
                else                 { const ix = elt; }
                eltRef = EltRef(Node(curr), ix, modStatus);
                break;
            default:
                assert(false, "Unsupported Leaf!Value type " ~ curr.typeIx.to!string);
            }
            return curr;
        }

        static if (hasValue)
        {
            Branch insertAtLeafOfBranch(Branch curr, Ix key, Value value, out EltRef eltRef)
            {
                debug if (willFail) { dln("WILL FAIL: key:", key,
                                          " value:", value,
                                          " curr:", curr,
                                          " currPrefix:", getPrefix(curr),
                                          " eltRef:", eltRef); }
                if (auto leaf = getLeaf(curr))
                {
                    setLeaf(curr, insertIxAtLeaftoLeaf(leaf, IxElement(key, value), eltRef));
                }
                else
                {
                    auto leaf_ = constructVariableLength!(SparseLeaf1!Value*)(1, [key], [value]); // needed for values
                    eltRef = EltRef(Node(leaf_), Ix(0), ModStatus.added);
                    setLeaf(curr, Leaf!Value(leaf_));
                }
                return curr;
            }
        }
        else
        {
            Branch insertAtLeafOfBranch(Branch curr, Ix key, out EltRef eltRef)
            {
                debug if (willFail) { dln("WILL FAIL: key:", key,
                                          " curr:", curr,
                                          " currPrefix:", getPrefix(curr),
                                          " eltRef:", eltRef); }
                if (auto leaf = getLeaf(curr))
                {
                    setLeaf(curr, insertIxAtLeaftoLeaf(leaf, key, eltRef));
                }
                else
                {
                    auto leaf_ = construct!(HeptLeaf1)(key); // can pack more efficiently when no value
                    eltRef = EltRef(Node(leaf_), Ix(0), ModStatus.added);
                    setLeaf(curr, Leaf!Value(leaf_));
                }
                return curr;
            }
        }

        Node insertAtLeaf(Leaf!Value curr, Element elt, out EltRef eltRef)
        {
            debug if (willFail) { dln("WILL FAIL: elt:", elt); }

            auto key = elementKey(elt);
            assert(key.length);
            if (key.length == 1)
            {
                static if (hasValue)
                    return Node(insertIxAtLeaftoLeaf(curr, IxElement(key[0], elt.value), eltRef));
                else
                    return Node(insertIxAtLeaftoLeaf(curr, key[0], eltRef));
            }
            else
            {
                assert(key.length >= 2);
                const prefixLength = key.length - 2; // >= 0
                const nextPrefix = key[0 .. prefixLength];
                auto next = constructVariableLength!(DefaultBranch)(1, nextPrefix, curr); // one sub-node and one leaf
                return Node(insertAtBranchBelowPrefix(Branch(next), elementKeyDropExactly(elt, prefixLength), eltRef));
            }
        }

        static if (!hasValue)
        {
            Node insertAt(OneLeafMax7 curr, UKey key, out EltRef eltRef)
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
                        // TODO functionize:
                        Node next;
                        switch (matchedKeyPrefix.length)
                        {
                        case 0:
                            next = construct!(HeptLeaf1)(curr.key[0], key[0]);
                            eltRef = EltRef(next, Ix(1), ModStatus.added);
                            break;
                        case 1:
                            next = construct!(TriLeaf2)(curr.key, key);
                            eltRef = EltRef(next, Ix(1), ModStatus.added);
                            break;
                        case 2:
                            next = construct!(TwoLeaf3)(curr.key, key);
                            eltRef = EltRef(next, Ix(1), ModStatus.added);
                            break;
                        default:
                            import std.algorithm : min;
                            const nextPrefix = matchedKeyPrefix[0 .. min(matchedKeyPrefix.length,
                                                                         DefaultBranch.prefixCapacity)]; // limit prefix branch capacity
                            Branch nextBranch = constructVariableLength!(DefaultBranch)(1 + 1, // `curr` and `key`
                                                                                      nextPrefix);
                            nextBranch = insertNewAtBranchBelowPrefix(nextBranch, curr.key[nextPrefix.length .. $]);
                            nextBranch = insertAtBranchBelowPrefix(nextBranch, key[nextPrefix.length .. $], eltRef);
                            assert(eltRef);
                            next = Node(nextBranch);
                            break;
                        }
                        freeNode(curr);
                        return next;
                    }
                }

                return Node(insertAtBranchAbovePrefix(expand(curr), key, eltRef));
            }

            Node insertAt(TwoLeaf3 curr, UKey key, out EltRef eltRef)
            {
                assert(hasVariableKeyLength || curr.keyLength == key.length);
                if (curr.keyLength == key.length)
                {
                    if (curr.contains(key)) { return Node(curr); }
                    if (!curr.keys.full)
                    {
                        assert(curr.keys.length == 1);
                        eltRef = EltRef(Node(curr), Ix(curr.keys.length), ModStatus.added);
                        curr.keys.pushBack(key);
                        return Node(curr);
                    }
                }
                return Node(insertAtBranchAbovePrefix(expand(curr), key, eltRef)); // NOTE stay at same (depth)
            }

            Node insertAt(TriLeaf2 curr, UKey key, out EltRef eltRef)
            {
                assert(hasVariableKeyLength || curr.keyLength == key.length);
                if (curr.keyLength == key.length)
                {
                    if (curr.contains(key)) { return Node(curr); }
                    if (!curr.keys.full)
                    {
                        eltRef = EltRef(Node(curr), Ix(curr.keys.length), ModStatus.added);
                        curr.keys.pushBack(key);
                        return Node(curr);
                    }
                }
                return Node(insertAtBranchAbovePrefix(expand(curr), key, eltRef)); // NOTE stay at same (depth)
            }

            Leaf!Value insertAt(HeptLeaf1 curr, Ix key, out EltRef eltRef)
            {
                if (curr.contains(key)) { return Leaf!Value(curr); }
                if (!curr.keys.full)
                {
                    eltRef = EltRef(Node(curr), Ix(curr.keys[$ - 1]), ModStatus.added);
                    curr.keys.pushBack(key);
                    return Leaf!Value(curr);
                }
                else            // curr is full
                {
                    assert(curr.keys.length == curr.capacity);

                    // pack `curr.keys` plus `key` into `nextKeys`
                    Ix[curr.capacity + 1] nextKeys;
                    nextKeys[0 .. curr.capacity] = curr.keys;
                    nextKeys[curr.capacity] = key;

                    import std.algorithm.sorting : sort;
                    sort(nextKeys[]); // TODO move this sorting elsewhere

                    auto next = constructVariableLength!(SparseLeaf1!Value*)(nextKeys.length, nextKeys[]);
                    eltRef = EltRef(Node(next), Ix(curr.capacity), ModStatus.added);

                    freeNode(curr);
                    return Leaf!Value(next);
                }
            }

            Node insertAt(HeptLeaf1 curr, UKey key, out EltRef eltRef)
            {
                assert(hasVariableKeyLength || curr.keyLength == key.length);
                if (curr.keyLength == key.length)
                {
                    return Node(insertAt(curr, key[0], eltRef)); // use `Ix key`-overload
                }
                return insertAt(Node(constructVariableLength!(DefaultBranch)(1, Leaf!Value(curr))), // current `key`
                                key, eltRef); // NOTE stay at same (depth)
            }

            /** Split `curr` using `prefix`. */
            Node split(OneLeafMax7 curr, UKey prefix, UKey key) // TODO key here is a bit malplaced
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
                Branch next = constructVariableLength!(DefaultBranch)(1 + 1, prefix); // current plus one more
                next = insertNewAtBranchBelowPrefix(next, curr.key[prefix.length .. $]);
                freeNode(curr);   // remove old current

                return Node(next);
            }

            /** Destructively expand `curr` to make room for `capacityIncrement` more keys and return it. */
            Branch expand(OneLeafMax7 curr, size_t capacityIncrement = 1)
            {
                assert(curr.key.length >= 2);
                typeof(return) next;

                if (curr.key.length <= DefaultBranch.prefixCapacity + 1) // if `key` fits in `prefix` of `DefaultBranch`
                {
                    next = constructVariableLength!(DefaultBranch)(1 + capacityIncrement, curr.key[0 .. $ - 1], // all but last
                                                                 Leaf!Value(construct!(HeptLeaf1)(curr.key[$ - 1]))); // last as a leaf
                }
                else                // curr.key.length > DefaultBranch.prefixCapacity + 1
                {
                    next = constructVariableLength!(DefaultBranch)(1 + capacityIncrement, curr.key[0 .. DefaultBranch.prefixCapacity]);
                    next = insertNewAtBranchBelowPrefix(next, curr.key[DefaultBranch.prefixCapacity .. $]);
                }

                freeNode(curr);
                return next;
            }

            /** Destructively expand `curr` to make room for `capacityIncrement` more keys and return it. */
            Branch expand(TwoLeaf3 curr, size_t capacityIncrement = 1)
            {
                typeof(return) next;
                if (curr.keys.length == 1) // only one key
                {
                    next = constructVariableLength!(DefaultBranch)(1 + capacityIncrement);
                    next = insertNewAtBranchAbovePrefix(next, // current keys plus one more
                                                        curr.keys.at!0);
                }
                else
                {
                    next = constructVariableLength!(DefaultBranch)(curr.keys.length + capacityIncrement, curr.prefix);
                    // TODO functionize and optimize to insertNewAtBranchAbovePrefix(next, curr.keys)
                    foreach (key; curr.keys)
                    {
                        next = insertNewAtBranchBelowPrefix(next, key[curr.prefix.length .. $]);
                    }
                }
                freeNode(curr);
                return next;
            }

            /** Destructively expand `curr` to make room for `capacityIncrement` more keys and return it. */
            Branch expand(TriLeaf2 curr, size_t capacityIncrement = 1)
            {
                typeof(return) next;
                if (curr.keys.length == 1) // only one key
                {
                    next = constructVariableLength!(DefaultBranch)(1 + capacityIncrement); // current keys plus one more
                    next = insertNewAtBranchAbovePrefix(next, curr.keys.at!0);
                }
                else
                {
                    next = constructVariableLength!(DefaultBranch)(curr.keys.length + capacityIncrement, curr.prefix);
                    // TODO functionize and optimize to insertNewAtBranchAbovePrefix(next, curr.keys)
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
                auto next = constructVariableLength!(SparseLeaf1!Value*)(curr.keys.length + capacityIncrement, curr.keys);
                freeNode(curr);
                return Node(next);
            }
        }
    }

    /** Destructively expand `curr` to a branch node able to store
        `capacityIncrement` more sub-nodes.
    */
    Branch expand(SparseBranch* curr, size_t capacityIncrement = 1)
    {
        typeof(return) next;
        assert(curr.subCount < radix); // we shouldn't expand beyond radix
        if (curr.empty)     // if curr also empty length capacity must be zero
        {
            next = constructVariableLength!(typeof(curr))(1, curr); // so allocate one
        }
        else if (curr.subCount + capacityIncrement <= curr.maxCapacity) // if we can expand to curr
        {
            const requiredCapacity = curr.subCapacity + capacityIncrement;
            auto next_ = constructVariableLength!(typeof(curr))(requiredCapacity, curr);
            assert(next_.subCapacity >= requiredCapacity);
            next = next_;
        }
        else
        {
            next = construct!(DenseBranch*)(curr);
        }
        freeNode(curr);
        return next;
    }

    /** Destructively expand `curr` to a leaf node able to store
        `capacityIncrement` more sub-nodes.
    */
    Leaf!Value expand(SparseLeaf1!Value* curr, size_t capacityIncrement = 1)
    {
        typeof(return) next;
        assert(curr.length < radix); // we shouldn't expand beyond radix
        if (curr.empty)     // if curr also empty length capacity must be zero
        {
            next = constructVariableLength!(typeof(curr))(capacityIncrement); // make room for at least one
        }
        else if (curr.length + capacityIncrement <= curr.maxCapacity) // if we can expand to curr
        {
            const requiredCapacity = curr.capacity + capacityIncrement;
            static if (hasValue)
            {
                auto next_ = constructVariableLength!(typeof(curr))(requiredCapacity, curr.ixs, curr.values);
            }
            else
            {
                auto next_ = constructVariableLength!(typeof(curr))(requiredCapacity, curr.ixs);
            }
            assert(next_.capacity >= requiredCapacity);
            next = next_;
        }
        else
        {
            next = construct!(DenseLeaf1!Value*)(curr.ixs); // TODO make use of sortedness of `curr.keys`?
        }
        freeNode(curr);
        return next;
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
        if (!hasVariableLength!NodeType)
    {
        version(debugPrintAllocations) { dln("constructing ", NodeType.stringof, " from ", args); }
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

    auto constructVariableLength(NodeType, Args...)(size_t requiredCapacity, Args args) @trusted
        if (isPointer!NodeType &&
            hasVariableLength!NodeType)
    {
        version(debugPrintAllocations) { dln("constructing ", NodeType.stringof, " from ", args); }
        debug ++nodeCountsByIx[NodeType.stringof];
        debug ++_heapNodeAllocationBalance;
        import vla : constructVariableLength;
        return constructVariableLength!(typeof(*NodeType.init))(requiredCapacity, args);
        // TODO ensure alignment of node at least that of NodeType.alignof
    }

    void freeNode(NodeType)(NodeType nt) @trusted
    {
        version(debugPrintAllocations) { dln("freeing ", NodeType.stringof, " ", nt); }
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

        /// Release `Leaf!Value curr`.
        void release(Leaf!Value curr)
        {
            final switch (curr.typeIx) with (Leaf!Value.Ix)
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
            version(debugPrintAllocations) { dln("releasing Node ", curr); }
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
            writeln(typeof(curr_).stringof, " #", curr_.key.length, ": ", curr_.to!string);
            break;
        case ix_TwoLeaf3:
            auto curr_ = curr.as!(TwoLeaf3);
            writeln(typeof(curr_).stringof, " #", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_TriLeaf2:
            auto curr_ = curr.as!(TriLeaf2);
            writeln(typeof(curr_).stringof, " #", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_HeptLeaf1:
            auto curr_ = curr.as!(HeptLeaf1);
            writeln(typeof(curr_).stringof, " #", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_SparseLeaf1Ptr:
            auto curr_ = curr.as!(SparseLeaf1!Value*);
            write(typeof(*curr_).stringof, " #", curr_.length, "/", curr_.capacity, " @", curr_);
            write(": ");
            bool other = false;
            foreach (const i, const ix; curr_.ixs)
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
                static if (hasValue)
                {
                    write("=>", curr_.values[i]);
                }
            }
            writeln();
            break;
        case ix_DenseLeaf1Ptr:
            auto curr_ = curr.as!(DenseLeaf1!Value*);
            write(typeof(*curr_).stringof, " #", curr_.count, " @", curr_);
            write(": ");

            // keys
            size_t ix = 0;
            bool other = false;
            foreach (const keyBit; curr_._ixBits[])
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
                write(s);
                static if (hasValue)
                {
                    write("=>", curr_.values[ix]);
                }
                ++ix;
            }

            writeln();
            break;
        case ix_SparseBranchPtr:
            auto curr_ = curr.as!(SparseBranch*);
            write(typeof(*curr_).stringof, " #", curr_.subCount, "/", curr_.subCapacity, " @", curr_);
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix.toString('_')); }
            writeln(":");
            if (curr_.leaf)
            {
                printAt(Node(curr_.leaf), depth + 1);
            }
            foreach (const i, const subNode; curr_.subNodes)
            {
                printAt(subNode, depth + 1, cast(uint)curr_.subIxs[i]);
            }
            break;
        case ix_DenseBranchPtr:
            auto curr_ = curr.as!(DenseBranch*);
            write(typeof(*curr_).stringof, " #", curr_.subCount, "/", radix, " @", curr_);
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix.toString('_')); }
            writeln(":");
            if (curr_.leaf)
            {
                printAt(Node(curr_.leaf), depth + 1);
            }
            foreach (const i, const subNode; curr_.subNodes)
            {
                printAt(subNode, depth + 1, cast(uint)i);
            }

            break;
        }
    }

private:
    enum fixedKeyLengthUndefined = 0;
    immutable fixedKeyLength = fixedKeyLengthUndefined; ///< maximum length of key if fixed, otherwise 0

    Node _root;                 ///< tree root node
    size_t _length = 0; ///< number of elements (keys or key-value-pairs) currently stored under `_root`

    debug:                      // debug stuff
    long _heapNodeAllocationBalance = 0;
    size_t[string] nodeCountsByIx; // TODO Index using Node.Ix instead
    bool willFail;
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
        assert(curr_.length);
        ++stats.popHist_SparseLeaf1[curr_.length - 1]; // TODO type-safe indexing
        stats.sparseLeaf1SizeSum += curr_.allocatedSize;
        break;
    case ix_DenseLeaf1Ptr:
        auto curr_ = curr.as!(DenseLeaf1!Value*);
        ++stats.heapNodeCount;
        const count = curr_._ixBits.countOnes; // number of non-zero sub-nodes
        assert(count <= curr_.capacity);
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

/** Append statistics of tree under `Leaf!Value` `curr.` into `stats`.
 */
static private void calculate(Value)(Leaf!Value curr,
                                     ref RawRadixTree!(Value).Stats stats)
    @safe pure nothrow /* TODO @nogc */
{
    alias RT = RawRadixTree!(Value);
    ++stats.popByLeafType[curr.typeIx];

    with (Leaf!Value.Ix)
    {
        final switch (curr.typeIx)
        {
        case undefined: break;
        case ix_HeptLeaf1: break; // TODO calculate()
        case ix_SparseLeaf1Ptr:
            ++stats.heapNodeCount;
            auto curr_ = curr.as!(SparseLeaf1!Value*);
            assert(curr_.length);
            ++stats.popHist_SparseLeaf1[curr_.length - 1]; // TODO type-safe indexing
            break;
        case ix_DenseLeaf1Ptr:
            auto curr_ = curr.as!(DenseLeaf1!Value*);
            ++stats.heapNodeCount;
            const count = curr_._ixBits.countOnes; // number of non-zero curr-nodes
            assert(count <= curr_.capacity);
            assert(count);
            ++stats.popHist_DenseLeaf1[count - 1]; // TODO type-safe indexing
            break;
        }
    }
}

/** Remap typed key `typedKey` to untype key of type `UKey`. */
static private UKey remapKey(TypedKey)(in TypedKey typedKey)
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

    static if (_tree.hasValue)
    {
        alias Element = Tuple!(Key, "key", Value, "value");
    }
    else
    {
        alias Element = Key;
    }

    static if (_tree.hasValue)
    {
        ref Value opIndex(Key key)
        {
            Value* value = contains(key);
            if (value is null)
            {
                import core.exception : RangeError;
                throw new RangeError("Range violation");
            }
            return *value;
        }

        auto opIndexAssign(in Value value, Key key)
        {
            _tree.EltRef eltRef; // indicates that elt was added
            _tree.insert(key.remapKey, value, eltRef);
            const bool added = eltRef.node && eltRef.modStatus == ModStatus.added;
            _length += added;
            /* TODO return reference (via `auto ref` return typed) to stored
               value at `eltRef` instead, unless packed bitset storage is used
               when `Value is bool` */
            return value;
        }

        /** Insert `key`.
            Returns: `false` if key was previously already added, `true` otherwise.
        */
        bool insert(in Key key, in Value value)
        {
            _tree.EltRef eltRef; // indicates that key was added
            auto rawKey = key.remapKey;
            _tree.insert(rawKey, value, eltRef);
            debug if (willFail) { dln("WILL FAIL: eltRef:", eltRef, " key:", key); }
            if (eltRef.node)  // if `key` was added at `eltRef`
            {
                // set value
                final switch (eltRef.node.typeIx) with (_tree.Node.Ix)
                {
                case undefined:
                case ix_OneLeafMax7:
                case ix_TwoLeaf3:
                case ix_TriLeaf2:
                case ix_HeptLeaf1:
                case ix_SparseBranchPtr:
                case ix_DenseBranchPtr:
                    assert(false);
                case ix_SparseLeaf1Ptr:
                    eltRef.node.as!(SparseLeaf1!Value*).setValue(rawKey[$ - 1], value);
                    break;
                case ix_DenseLeaf1Ptr:
                    eltRef.node.as!(DenseLeaf1!Value*).setValue(rawKey[$ - 1], value);
                    break;
                }
                const bool hit = eltRef.modStatus == ModStatus.added;
                _length += hit;
                return hit;
            }
            else
            {
                dln("TODO warning no eltRef for key:", key, " rawKey:", rawKey);
                assert(false);
            }
        }

        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        inout(Value*) contains(in Key key) inout
        {
            return _tree.contains(key.remapKey);
        }
    }
    else
    {
        /** Insert `key`.
            Returns: `true` if `key` wasn't previously added, `false` otherwise.
        */
        bool insert(Key key)
        @safe pure nothrow /* TODO @nogc */
        {
            _tree.EltRef eltRef; // indicates that elt was added
            _tree.insert(key.remapKey, eltRef);
            const bool hit = eltRef.node && eltRef.modStatus == ModStatus.added;
            _length += hit;
            return hit;
        }

        nothrow:

        /** Returns: `true` if `key` is stored, `false` otherwise. */
        bool contains(in Key key) inout
        {
            return _tree.contains(key.remapKey);
        }
    }

    /** Supports $(B `Key` in `this`) syntax. */
    auto opBinaryRight(string op)(in Key key) inout if (op == "in")
    {
        return contains(key);   // TODO return `_tree.EltRef`
    }

    /** Print `this` tree. */
    void print() @safe const { _tree.print(); }

    private RawRadixTree!(Value) _tree;
    alias _tree this;
}
alias PatriciaTrie = RadixTree;
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/** Keys are stored in a way that they can't be accessed by reference so we
    allow array (and string) keys to be of mutable type.
*/
template MutableKey(Key)
{
    static if (isArray!Key)
    {
        alias MutableKey = const(Unqual!(typeof(Key.init[0])))[];
    }
    else
    {
        alias MutableKey = Key;
    }
}

/// Instantiator for the set-version of `RadixTree` where value-type is `void` (unused).
auto radixTreeSet(Key)()
{
    return RadixTree!(MutableKey!Key, void)(false);
}

/// Instantiator for the map-version of `RadixTree` where value-type is `Value`.
auto radixTreeMap(Key, Value)()
{
    return RadixTree!(MutableKey!Key, Value)(false);
}

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
    import std.conv : to;
    import std.stdio : writeln;

    auto stats = tree.usageHistograms;

    writeln("Population By Node Type: ", stats.popByNodeType);
    writeln("Population By Leaf!Value Type: ", stats.popByLeafType);

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
            case undefined: continue; // ignore
            case ix_OneLeafMax7: bytesUsed = pop*OneLeafMax7.sizeof; break;
            case ix_TwoLeaf3: bytesUsed = pop*TwoLeaf3.sizeof; break;
            case ix_TriLeaf2: bytesUsed = pop*TriLeaf2.sizeof; break;
            case ix_HeptLeaf1: bytesUsed = pop*HeptLeaf1.sizeof; break;
            case ix_SparseLeaf1Ptr:
                bytesUsed = stats.sparseLeaf1SizeSum; // must be used because SparseLeaf1.sizeof cannot be used because it's a variable length struct
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
        writeln(pop, " number of ",
                ix.to!string[3 .. $], // TODO Use RT.Node.indexTypeName(ix)
                " uses ", bytesUsed/1e6, " megabytes");
    }

    // Leaf!Value-usage
    foreach (Leaf!(RT.ValueType).Ix ix, pop; stats.popByLeafType) // TODO use stats.byPair when added to typecons_ex.d
    {
        size_t bytesUsed = 0;
        with (Leaf!(RT.ValueType).Ix)
        {
            final switch (ix)
            {
            case undefined: continue; // ignore
            case ix_HeptLeaf1: bytesUsed = pop*HeptLeaf1.sizeof; break;
            case ix_SparseLeaf1Ptr: bytesUsed = pop*SparseLeaf1!(RT.ValueType).sizeof; totalBytesUsed += bytesUsed; break;
            case ix_DenseLeaf1Ptr: bytesUsed = pop*DenseLeaf1!(RT.ValueType).sizeof; totalBytesUsed += bytesUsed; break;
            }
        }
        writeln(pop, " number of ",
                ix.to!string[3 .. $], // TODO Use RT.Node.indexTypeName(ix)
                " uses ", bytesUsed/1e6, " megabytes");
    }

    writeln("Tree uses ", totalBytesUsed/1e6, " megabytes");
}

/// test map from `uint` to values of type `double`
// @safe pure nothrow /* TODO @nogc */
unittest
{
    alias Key = uint;
    alias Value = ushort;

    auto map = radixTreeMap!(Key, Value);
    assert(map.empty);

    static assert(map.hasValue);

    foreach (const i; 0 .. SparseLeaf1!Value.maxCapacity)
    {
        assert(!map.contains(i));
        assert(map.length == i);
        map[i] = i*radix;
        assert(map.contains(i));
        assert(*map.contains(i) == i*radix);
        assert(i in map);
        assert(*(i in map) == i*radix);
        assert(map.length == i + 1);
    }

    foreach (const i; SparseLeaf1!Value.maxCapacity .. radix)
    {
        assert(!map.contains(i));
        assert(map.length == i);
        map[i] = i*radix;
        assert(map.contains(i));
        assert(*map.contains(i) == i*radix);
        assert(i in map);
        assert(*(i in map) == i*radix);
        assert(map.length == i + 1);
    }
}

/// test map to values of type `bool`
@safe pure nothrow /* TODO @nogc */ unittest
{
    alias Key = uint;
    alias Value = bool;

    auto map = radixTreeMap!(Key, Value);
    assert(map.empty);

    static assert(map.hasValue);
    map.insert(Key.init, Value.init);
}

/// test packing of set elements
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
@safe pure nothrow /* TODO @nogc */
unittest
{
    auto set = radixTreeSet!(ubyte);
    alias Set = typeof(set);

    foreach (const i; 0 .. HeptLeaf1.capacity)
    {
        assert(!set.contains(i));

        assert(set.insert(i));
        assert(set.contains(i));

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
        assert(set.contains(i));

        assert(!set.insert(i));
        assert(set.contains(i));

        debug assert(set.heapNodeAllocationBalance == 1);
        // const rootRef = set._root.peek!(SparseLeaf1!void*);
        // assert(rootRef);
    }

    // const rootRef = set._root.peek!(SparseLeaf1!void*);
    // assert(rootRef);

    // const root = *rootRef;
    // assert(!root.empty);
    // assert(root.full);
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

        // 256
        assert(!set.contains(256));

        assert(set.insert(256));
        assert(set.contains(256));

        assert(!set.insert(256));
        assert(set.contains(256));

        // 257
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

/** Generate `count` number of random unique strings of minimum length 1 and
    maximum length of `maxLength`.
 */
private static auto randomUniqueStrings(size_t count, uint maxLength)
    @trusted pure nothrow
{
    import std.random : Random, uniform;
    auto gen = Random();

    bool[string] stringSet;  // set of strings using D's builtin associative array

    try
    {
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
    }
    catch (Exception e)
    {
        dln("Couldn't randomize");
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
@safe pure nothrow /* TODO @nogc */
unittest
{
    checkString!(string)(2^^18, 2^^7);
}

/// Create a set of words from /usr/share/dict/words
void testWords(Value)()
{
    import std.datetime : StopWatch, AutoStart, Duration;
    import std.stdio : File;
    import std.range : chain;

    immutable path = "/usr/share/dict/words";

    enum hasValue = !is(Value == void);

    static if (hasValue) { auto rtr = radixTreeMap!(string, Value); }
    else                 { auto rtr = radixTreeSet!(string); }
    assert(rtr.empty);

    enum debugPrint = false;

    string[] firsts = [];
    size_t count = 0;
    auto sw = StopWatch(AutoStart.yes);
    foreach (const word; chain(firsts, File(path).byLine))
    {
        import std.algorithm.searching : endsWith;
        import std.range : empty;
        if (!word.empty &&
            !word.endsWith(`'s`)) // skip genitive forms
        {
            assert(!rtr.contains(word));

            static if (debugPrint)
            {
                import std.string : representation;
                dln(`word:"`, word, `" of length:`, word.length,
                    ` of representation:`, word.representation);
                debug rtr.willFail = word == `amiable`;
                if (rtr.willFail)
                {
                    rtr.print();
                }
            }

            static if (hasValue)
            {
                assert(rtr.insert(word, count));
                auto hitA = rtr.contains(word);
                assert(hitA);
                assert(*hitA == count);

                assert(!rtr.insert(word, count));
                auto hitB = rtr.contains(word);
                assert(hitB);
                assert(*hitB == count);
            }
            else
            {
                assert(rtr.insert(word));
                assert(rtr.contains(word));

                assert(!rtr.insert(word));
                assert(rtr.contains(word));
            }
            ++count;
        }
    }
    sw.stop;
    version(print)
    {
        import std.conv : to;
        import std.stdio : writeln;
        writeln("Added ", count, " words from ", path, " in ", sw.peek().to!Duration);
        rtr.showStatistics();
    }
}

unittest
{
    testWords!void;
    testWords!size_t;
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

        enum n = 1_000_000;

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

///
@safe pure nothrow /* TODO @nogc */ unittest
{
    version(enterSingleInfiniteMemoryLeakTest)
    {
        while (true)
        {
            checkNumeric!(float, double,
                          long, int, short, byte,
                          ulong, uint, ushort, ubyte);
        }
    }
    else
    {
        checkNumeric!(float, double,
                      long, int, short, byte,
                      ulong, uint, ushort, ubyte);
    }
}

auto testScalar(uint span, Keys...)()
    if (Keys.length >= 1)
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        foreach (Key; Keys)
        {
            auto set = radixTreeSet!(Key);

            static if (isIntegral!Key)
            {
                import std.algorithm : min, max;
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

///
@safe pure nothrow /* TODO @nogc */ unittest
{
    testScalar!(8,
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
