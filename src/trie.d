/** Tries and Prefix Trees.

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree

    TODO Use IxsN.at(ix) and use inplace of IxsN.opIndex

    TODO Make `Key` array of `immutable Ix` like `string`

    TODO Allow NodeType-constructors to take const and immutable prefixes

    TODO Check for case when expanding to bit-branch instead of LinBr4 in all `expand()` overloads

    TODO Make array indexing/slicing as @trusted and use .ptr[] instead of [] when things are stable. Especially in IxsN

    TODO Store `isKey` in top-most bit of length part of `IxsN prefix` and use for TwoLf3, TriLf2, and SixLf1.

    TODO Optimize LinBr4.findSub for specific `subPopulation`

    TODO Add function reprefix({LinBr4|FullBrM) and call after insertAt({LinBr4|FullBrM}). Only useful when one single leaf is present?
    TODO Is std.algorithm.countUntil the most suitable function to use in setSub(LinBr4*, ...)
    TODO Use std.experimental.allocator

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type FullBrM*?

    TODO Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`

    TODO Should opBinaryRight return void* instead of bool for set-case?

    TODO Add `struct Range`. Use same construct as in `containers-em/src/containers/ttree.d`.

    - Members:
      - It front()
      - void popFront()
      - bool empty())
      - It it; // It is defined below
    - Reuse RefCounted reference to _root. Add checks with `isSorted`.

    Prefix:
    - `set.prefix("alpha")`                           => `SortedTreeRange` of `Tuple!(string, Lang, PoT, Sense)`.
    - `set.prefix(tuple("alpha"))`                    => `SortedTreeRange` of `Tuple!(Lang, PoT, Sense)`.
    - `set.prefix(tuple("alpha", Lang.en))`           => `SortedTreeRange` of `Tuple!(PoT, Sense)`.
    - `set.prefix(tuple("alpha", Lang.en, PoT.noun))` => `SortedTreeRange` of `Tuple!(Sense)`.

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
// version = print;

import dbg;

alias isFixedTrieableKeyType = isIntegralBijectableType;

enum isTrieableKeyType(T) = (isFixedTrieableKeyType!T ||
                             (isInputRange!T &&
                              isFixedTrieableKeyType!(ElementType!T)));

extern(C) pure nothrow @system /* TODO @nogc */
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

/** Statically allocated `Ix`-array of fixed pre-allocated length `maxLength` of
    Ix-elements in chunks of elementLength. `ElementType` is
    `Ix[elementLength]`.
 */
struct IxsN(size_t maxLength,
            uint elementLength = 1,
            uint span = 8)
    if (maxLength*elementLength >= 2) // no use storing less than 2 bytes
{
    enum L = elementLength;
    enum M = 2^^span;   // branch-multiplicity, typically either 2, 4, 16 or 256
    alias Ix = Mod!M;

    static if (L == 1)
        alias E = Ix;
    else
        alias E = Ix[L];

    this(Es...)(Es ixs)
        if (Es.length >= 1 &&
            Es.length <= maxLength)
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
        this(Ix[] ixs)
        {
            assert(ixs.length <= maxLength);
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

    bool empty() const { return _length == 0; }
    bool full() const { return _length == maxLength; }

    auto front() inout
    {
        assert(!empty);
        return _ixs[0];
    }

    auto back() inout
    {
        assert(!empty);
        return _ixs[_length - 1];
    }

    /** Pop one front element. */
    auto ref popFront()
    {
        assert(!empty);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - 1)
        {
            _ixs[i] = _ixs[i + 1]; // TODO move construct?
        }
        --_length;
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
        _length -= n;
        return this;
    }

    auto ref popBack()
    {
        assert(!empty);
        --_length;
        return this;
    }

    auto ref pushBack(Es...)(Es moreEs)
        if (Es.length <= maxLength)
    {
        assert(length + Es.length <= maxLength);
        foreach (const i, const ix; moreEs)
        {
            _ixs[_length + i] = ix;
        }
        _length += Es.length;
        return this;
    }

    bool contains(Ix[] ix) const @nogc
    {
        import std.algorithm.searching : canFind;
        if (ix.length != L) { return false; }
        return (chunks.canFind(ix)); // TODO use binarySearch
    }

    auto chunks() inout { return _ixs[0 .. _length]; }
    alias chunks this;

    auto ref at(uint ix)()
        if (ix < maxLength)
    {
        return _ixs[i];
    }

    auto length() const { return _length; }

private:
    Mod!(maxLength + 1) _length; // number of defined elements in `_ixs`
    static if (L == 1)
    {
        Ix[maxLength] _ixs;     // byte indexes
    }
    else
    {
        Ix[L][maxLength] _ixs;  // byte indexes
    }
}

static assert(IxsN!(6, 1, 8).sizeof == 7);
static assert(IxsN!(3, 2, 8).sizeof == 7);
static assert(IxsN!(2, 3, 8).sizeof == 7);

@safe pure nothrow unittest
{
    import std.algorithm : equal;
    import modulo : mod;

    enum span = 8;
    enum M = 2^^span;

    alias Ix = Mod!(M, ubyte);
    Ix[] ixs = [11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M];
    enum maxLength = 7;

    auto x = IxsN!(maxLength, 1, span)(ixs);
    auto y = IxsN!(maxLength, 1, span)(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M);

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

@safe pure nothrow unittest
{
    assert(equalLength([1], [2], [3]));
    assert(!equalLength([1, 1], [2], [3]));
    assert(!equalLength([1], [2, 2], [3]));
    assert(!equalLength([1], [2], [3, 3]));
}

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
private struct RawRadixTree(Value,
                            uint span = 8) // binary power of radix, typically either 1, 2, 4 or 8
{
    import std.bitmanip : bitfields;
    import std.conv : to;
    import std.algorithm : filter;
    import std.meta : AliasSeq, staticMap;
    import std.typecons : ConstOf;
    import bitset : BitSet;

    static assert(span == 8, "Radix is currently limited to 8");

    enum isSet = is(Value == void); // `true` if this tree is a set. TODO better to use empty struct?
    enum isMap = !isSet;            // `true` if this tree is a map

    enum radix = 2^^span;     // branch-multiplicity, typically either 2, 4, 16 or 256

    alias order = radix;   // tree order

    /// `true` if tree has binary branch.
    enum isBinary = span == 2;

    /** Radix Modulo Index */
    alias Ix = Mod!radix; // restricted index type avoids range checking in array indexing below

    /** `span` least significant bits (LSB) of leaves directly packed into a word.

        TODO Generalize to packing of more than one `Ix` per byte.
        TODO respect byteorder in `SixLf1` to work with `WordVariant`
        TODO implement and use opSlice instead of .key[]
    */
    static if (size_t.sizeof == 8) // 64-bit CPU
    {
        static if (span == 8)
        {
            /// Single/1-Key Leaf with maximum key-length 6.
            struct OneLf6
            {
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof;
                this(Ix[] key) { this.key = key; }

                pragma(inline) bool contains(Key!span key) const @nogc { return this.key == key; }

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

                IxsN!(maxLength, 1, span) key;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }

            /// Binary/2-Key Leaf with key-length 3.
            struct TwoLf3
            {
                enum keyLength = 3;
                enum maxLength = 2;

                this(Keys...)(Keys keys)
                    if (Keys.length >= 1 && Keys.length <= maxLength)
                {
                    this.keys = keys;
                }

                inout(Ix)[] prefix() inout
                {
                    final switch (keys.length)
                    {
                    case 1:
                        return keys[0][];
                    case 2:
                        import std.algorithm : commonPrefix;
                        return commonPrefix(keys[0][], keys[1][]);
                    }
                }

                pragma(inline) bool contains(Key!span key) const @nogc { return keys.contains(key); }

                IxsN!(maxLength, keyLength, span) keys;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }

            /// Ternary/3-Key Leaf with key-length 2.
            struct TriLf2
            {
                enum keyLength = 2;
                enum maxLength = 3;

                this(Keys...)(Keys keys)
                    if (Keys.length >= 1 && Keys.length <= maxLength)
                {
                    this.keys = keys;
                }

                inout(Ix)[] prefix() inout
                {
                    final switch (keys.length)
                    {
                    case 1:
                        return keys[0][];
                    case 2:
                        import std.algorithm : commonPrefix;
                        return commonPrefix(keys[0][], keys[1][]);
                    case 3:
                        import std.algorithm : commonPrefix;
                        return commonPrefix(keys[0][],
                                            commonPrefix(keys[1][], keys[2][])); // TODO make and reuse variadic commonPrefix
                    }
                }

                pragma(inline) bool contains(Key!span key) const @nogc { return keys.contains(key); }

                IxsN!(maxLength, keyLength, span) keys;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }

            /// Hexa/6-Key Leaf with key-length 1.
            struct SixLf1
            {
                enum keyLength = 1;
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof; // maximum number of elements

                this(Keys...)(Keys keys)
                    if (Keys.length >= 1 && Keys.length <= maxLength)
                {
                    this.keys = keys;
                }

                pragma(inline) bool contains(Key!span key) const @nogc { return keys.contains(key); }

                IxsN!(maxLength, 1, span) keys;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }
        }
    }
    else
    {
        static assert(false, "Currently requires a 64-bit CPU (size_t.sizeof == 8)");
    }

    // TODO make these run-time arguments at different key depths and map to statistics of typed-key
    alias DefaultBr = LinBr4*; // either LinBr4*, FullBrM*

    static if (isSet)
        static assert(SixLf1.sizeof == size_t.sizeof); // assert that it's size matches platform word-size

    /** Node types. */
    alias NodeTypes = AliasSeq!(OneLf6,
                                TwoLf3,
                                TriLf2,
                                SixLf1,
                                LinBr4*,
                                FullLf1*,
                                FullBrM*);

    /** Mutable node. */
    alias Node = WordVariant!NodeTypes;
    /** Constant node. */
    // TODO make work with indexNaming alias ConstNodePtr = WordVariant!(staticMap!(ConstOf, NodeTypes));

    static assert(span <= 8*Ix.sizeof, "Need more precision in Ix");

    /** Iterator. */
    struct It
    {
        bool opCast(T : bool)() const @safe pure nothrow /* TODO @nogc */ { return cast(bool)node; }
        Node node;           // current leaf-`Node`. TODO use `Lf` type instead?
        Ix ix;               // index to sub at `node`
    }

    /** Tree Range. */
    struct Range
    {
        It begin;
        It end;
    }

    /** 256-Branch population histogram.
    */
    alias FullLf1_PopHist = size_t[radix];

    /** 4-Branch population histogram.
        Index maps to population with value range (1 .. 4).
    */
    alias LinBr4_PopHist = size_t[4];

    /** radix-Branch population histogram.
        Index maps to population with value range (1 .. `radix`).
    */
    alias FullBrM_PopHist = size_t[radix];

    /** radix-Leaf population histogram.
        Index maps to population with value range (1 .. `radix`).
    */
    alias LeafM_PopHist = size_t[radix];

    /** Tree Population and Memory-Usage Statistics. */
    struct Stats
    {
        FullLf1_PopHist popHist_FullLf1;
        LinBr4_PopHist popHist_LinBr4; // packed branch population histogram
        FullBrM_PopHist popHist_FullBrM; // full branch population histogram

        size_t allOneLf60CountOfLinBr4; // number of `LinBr4` which sub-branches are all `OneLf6` of length 0
        size_t allOneLf60CountOfFullBrM; // number of `FullBrM` which sub-branches are all `OneLf6` of length 0

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));

        /// Number of heap-allocated `Node`s. Should always equal `heapNodeAllocationBalance`.
        size_t heapNodeCount;
    }

    /** Full Bitset Branch with only bottom-most leaves. */
    static private struct FullLf1
    {
        enum maxPrefixLength = 14; // 6, 14, 22, ...
        enum maxSubCount = 256;

        @safe pure nothrow:

        this(Ixs...)(Ix[] prefix, bool isKey, Ixs subIxs)
            if (Ixs.length <= maxSubCount)
        {
            this.prefix = prefix;
            this.isKey = isKey;
            foreach (subIx; subIxs)
            {
                _keyBits[subIx] = true;
            }
        }

        pragma(inline) bool hasSubAt(Ix ix) const @nogc { return _keyBits[ix]; }
        pragma(inline) bool empty() const @nogc { return _keyBits.empty; }
        pragma(inline) bool full() const @nogc { return _keyBits.full; }

        pragma(inline) bool contains(Key!span key) const @nogc
        {
            import std.algorithm : skipOver;
            return (key.skipOver(prefix[]) &&  // matching prefix
                    key.length == 1 &&       // one key-chunk left
                    _keyBits[key[0]]);       // and it's set
        }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats)
        {
            const count = _keyBits.countOnes; // number of non-zero sub-nodes
            assert(count <= radix);
            ++stats.popHist_FullLf1[count - 1]; // TODO type-safe indexing
        }

        private:
        BitSet!radix _keyBits;  // 32 bytes
        IxsN!maxPrefixLength prefix; // prefix common to all `subNodes` (also called edge-label)
        bool isKey;
    }

    static assert(FullLf1.sizeof == 48);

    /** Sparse/Packed/Partial 4-way branch. */
    static private struct LinBr4
    {
        enum maxSubPopulation = 4;

        enum maxPrefixLength = 10; // 2, 10, 18, ...

        @safe pure nothrow:

        this(Ix[] prefix, bool isKey = false)
        {
            this.prefix = prefix;
            this.isKey = isKey;
        }

        this(Ix[] prefix, bool isKey, Ix subIx, Node subNode)
        {
            this.prefix = prefix;
            this.isKey = isKey;
            this.subIxSlots.at!0 = subIx;
            this.subNodeSlots.at!0 = subNode;
            this.subPopulation = 1;
        }

        void pushBackSub(Tuple!(Ix, Node) sub)
        {
            assert(!full);
            const backIx = subPopulation.mod!maxSubPopulation;
            subIxSlots[backIx] = sub[0];
            subNodeSlots[backIx] = sub[1];
            subPopulation = cast(ubyte)(subPopulation + 1); // TODO remove need for cast
        }
        inout(Node) findSub(Ix ix) inout
        {
            switch (subPopulation)
            {
            case 0:
                break;
            case 1:
                if (subIxSlots.at!0 == ix) { return subNodeSlots.at!0; }
                break;
            case 2:
                foreach (i; iota!(0, 2))
                {
                    if (subIxSlots.at!i == ix) { return subNodeSlots.at!i; }
                }
                break;
            case 3:
                foreach (i; iota!(0, 3))
                {
                    if (subIxSlots.at!i == ix) { return subNodeSlots.at!i; }
                }
                break;
            case 4:
                foreach (i; iota!(0, 4))
                {
                    if (subIxSlots.at!i == ix) { return subNodeSlots.at!i; }
                }
                break;
            default:
                // TODO do binary search
                foreach (const i_; 0 ..  subPopulation)
                {
                    const i = i_.mod!maxSubPopulation;
                    if (subIxSlots[i] == ix) { return subNodeSlots[i]; }
                }
                break;
            }
            return Node.init;
        }

        pragma(inline) bool empty() const @nogc { return subPopulation == 0; }
        pragma(inline) bool full() const @nogc { return subPopulation == maxSubPopulation; }

        pragma(inline) auto subIxs() inout @nogc
        {
            return subIxSlots[0 .. subPopulation];
        }
        pragma(inline) auto subNodes() inout @nogc
        {
            return subNodeSlots[0 .. subPopulation];
        }

        /** Returns `true` if this branch can be packed into a bitset, that is
            contains only sub-nodes of type `OneLf6` of zero length. */
        bool hasMinimumDepth() const /* TODO @nogc */
        {
            foreach (const sub; subNodes)
            {
                if (const subOneLf6Ref = sub.peek!(OneLf6))
                {
                    const subOneLf6 = *subOneLf6Ref;
                    if (subOneLf6.key.length != 0) { return false; }
                }
                else
                {
                    return false;
                }
            }
            return true;
        }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) const
        {
            size_t count = 0; // number of non-zero sub-nodes
            foreach (const sub; subNodes)
            {
                ++count;
                sub.calculate!(Value, span)(stats);
            }
            assert(count <= radix);
            ++stats.popHist_LinBr4[count - 1]; // TODO type-safe indexing
            stats.allOneLf60CountOfLinBr4 += hasMinimumDepth;
        }

        private:

        // members in order of decreasing `alignof`:
        StrictlyIndexed!(Node[maxSubPopulation]) subNodeSlots;
        IxsN!maxPrefixLength prefix; // prefix common to all `subNodes` (also called edge-label)
        StrictlyIndexed!(Ix[maxSubPopulation]) subIxSlots;
        mixin(bitfields!(ubyte, "subPopulation", 7, // counts length of defined elements in subNodeSlots
                         bool, "isKey", 1)); // key at this branch is occupied
    }

    static assert(LinBr4.sizeof == 48);

    /** Dense/Unpacked `radix`-branch with `radix` number of sub-nodes. */
    static private struct FullBrM
    {
        enum maxSubPopulation = 256;
        enum maxPrefixLength = 15; // 7, 15, 23, ..., we can afford larger prefix here because FullBrM is so large

        @safe pure nothrow:

        this(Ix[] prefix, bool isKey = false)
        {
            this.prefix = prefix;
            this.isKey = isKey;
        }

        this(Ix[] prefix, bool isKey, Ix subIx, Node subNode)
        {
            this(prefix, isKey);
            this.subNodes[subIx] = subNode;
        }

        this(LinBr4* rhs)
        {
            this.prefix = rhs.prefix;
            this.isKey = rhs.isKey;
            foreach (const i; 0 .. rhs.subPopulation) // each sub node. TODO use iota!(Mod!N)
            {
                const iN = i.mod!(LinBr4.maxSubPopulation);
                const subIx = rhs.subIxSlots[iN];
                this.subNodes[subIx] = rhs.subNodes[iN];
            }
        }

        IxsN!maxPrefixLength prefix; // prefix (edge-label) common to all `subNodes`
        bool isKey;      // key at this branch is occupied
        StrictlyIndexed!(Node[radix]) subNodes;

        /** Returns `true` if this branch can be packed into a bitset, that is
            contains only subNodes of type `OneLf6` of zero length. */
        bool hasMinimumDepth() const @safe pure nothrow /* TODO @nogc */
        {
            foreach (const subNode; subNodes)
            {
                if (const subOneLf6Ref = subNode.peek!(OneLf6))
                {
                    if ((*subOneLf6Ref).key.length != 0)
                    {
                        return false;
                    }
                }
                else
                {
                    return false;
                }
            }
            return true;
        }

        /// Number of non-null sub-Nodes.
        Mod!(radix + 1) subPopulation() const
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
                    subNode.calculate!(Value, span)(stats);
                }
            }
            assert(count <= radix);
            ++stats.popHist_FullBrM[count - 1]; // TODO type-safe indexing
            stats.allOneLf60CountOfFullBrM += hasMinimumDepth;
        }
    }

    static if (false)
    {
        pragma(msg, "LinBr4.sizeof:", LinBr4.sizeof, " LinBr4.alignof:", LinBr4.alignof);
        pragma(msg, "LinBr4.subNodes.sizeof:", LinBr4.subNodes.sizeof, " LinBr4.subNodes.alignof:", LinBr4.subNodes.alignof);
        pragma(msg, "LinBr4.prefix.sizeof:", LinBr4.prefix.sizeof, " LinBr4.prefix.alignof:", LinBr4.prefix.alignof);
        pragma(msg, "LinBr4.subIxs.sizeof:", LinBr4.subIxs.sizeof, " LinBr4.subIxs.alignof:", LinBr4.subIxs.alignof);
    }

    /** Set sub-`Node` of branch `Node curr` at index `ix` to `subNode`. */
    pragma(inline) Node setSub(Node curr, Ix subIx, Node subNode, size_t superPrefixLength) @safe pure nothrow
    {
        // debug if ((curr.peek!(FullBrM*) ||
        //            curr.peek!(LinBr4*)) &&
        //           subNode.peek!(SixLf1))
        // {
        //     dln("superPrefixLength:", superPrefixLength,
        //         " curr:", curr,
        //         " currPrefix:", getPrefix(curr),
        //         " fixedKeyLength:", fixedKeyLength);
        // }
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FullLf1Ptr: return setSub(curr.as!(FullLf1*), subIx, subNode, superPrefixLength);
        case Node.Ix.ix_LinBr4Ptr: return setSub(curr.as!(LinBr4*), subIx, subNode, superPrefixLength);
        case Node.Ix.ix_FullBrMPtr: return setSub(curr.as!(FullBrM*), subIx, subNode, superPrefixLength);
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }
    /// ditto
    Node setSub(FullLf1* curr, Ix subIx, Node subNode, size_t superPrefixLength) @safe pure nothrow /* TODO @nogc */
    {
        if (const subNodeRef = subNode.peek!(OneLf6))
        {
            const subNode_ = *subNodeRef;
            if (subNode_.key.empty)
            {
                curr._keyBits[subIx] = true;
                freeNode(subNode); // free it because it's stored inside the bitset itself
                return Node(curr);
            }
        }

        // need to expand
        const extraCount = curr._keyBits[subIx] ? 0 : 1; // if one extra sub-node needes to be added
        Node next;
        if (curr._keyBits.countOnes + extraCount <= LinBr4.maxSubPopulation) // if `curr` keys plus on extra fit
        {
            auto next_ = construct!(LinBr4*)(curr.prefix, curr.isKey);
            foreach (const ix; curr._keyBits.oneIndexes)
            {
                setSub(next_, ix, Node(construct!(OneLf6)), superPrefixLength);
            }
            next = next_;
        }
        else
        {
            auto next_ = construct!(FullBrM*)(curr.prefix, curr.isKey, );
            foreach (const ix; curr._keyBits.oneIndexes)
            {
                setSub(next_, ix, Node(construct!(OneLf6)), superPrefixLength);
            }
            next = next_;
        }
        freeNode(curr);
        return setSub(next, subIx, subNode, superPrefixLength); // insert on new
    }
    /// ditto
    Node setSub(LinBr4* curr, Ix subIx, Node subNode, size_t superPrefixLength) @safe pure nothrow /* TODO @nogc */
    {
        if (const hit = curr.subIxSlots.findIndex(subIx))
        {
            curr.subNodeSlots[hit.index] = subNode; // reuse
        }
        else if (!curr.full)     // if room left in curr
        {
            curr.pushBackSub(tuple(subIx, subNode)); // add one to existing
        }
        else                    // if no room left in curr we need to expand
        {
            auto next = construct!(FullBrM*)(curr);
            freeNode(curr);
            assert(!getSub(next, subIx)); // key slot should be free
            return setSub(next, subIx, subNode, superPrefixLength); // fast, because directly calls setSub(FullBrM*, ...)
        }
        return Node(curr);
    }
    /// ditto
    pragma(inline) Node setSub(FullBrM* curr, Ix subIx, Node subNode, size_t superPrefixLength) @safe pure nothrow /* TODO @nogc */
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

    Node getSub(FullLf1* curr, Ix subIx) @safe pure nothrow
    {
        if (curr.hasSubAt(subIx))
        {
            return Node(OneLf6.init);
        }
        return Node.init;
    }

    Node getSub(LinBr4* curr, Ix subIx) @safe pure nothrow
    {
        if (auto subNode = curr.findSub(subIx))
        {
            return subNode;
        }
        return Node.init;
    }

    Node getSub(FullBrM* curr, Ix subIx) @safe pure nothrow
    {
        auto sub = curr.subNodes[subIx];
        curr.subNodes[subIx] = Node.init; // zero it to prevent multiple references
        return sub;
    }

    /** Get sub-`Node` of branch `Node curr` at index `subIx. */
    Node getSub(Node curr, Ix subIx) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FullLf1Ptr: return getSub(curr.as!(FullLf1*), subIx);
        case Node.Ix.ix_LinBr4Ptr: return getSub(curr.as!(LinBr4*), subIx);
        case Node.Ix.ix_FullBrMPtr: return getSub(curr.as!(FullBrM*), subIx);
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Returns: `true` if `curr` is occupied, `false` otherwise. */
    pragma(inline) bool isKey(Node curr) const @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FullLf1Ptr: return curr.as!(FullLf1*).isKey;
        case Node.Ix.ix_LinBr4Ptr: return curr.as!(LinBr4*).isKey;
        case Node.Ix.ix_FullBrMPtr: return curr.as!(FullBrM*).isKey;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    pragma(inline) void makeKey(Node curr) const @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FullLf1Ptr: curr.as!(FullLf1*).isKey = true; break;
        case Node.Ix.ix_LinBr4Ptr: curr.as!(LinBr4*).isKey = true; break;
        case Node.Ix.ix_FullBrMPtr: curr.as!(FullBrM*).isKey = true; break;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Get prefix of node `curr`. */
    auto getPrefix(inout Node curr) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_OneLf6: return curr.as!(OneLf6).key[]; // suffix is the prefix
        // case Node.Ix.ix_TriLf2: return curr.as!(TriLf2).prefix[];
        case Node.Ix.ix_SixLf1: return inout(Ix[]).init; // no prefix
        case Node.Ix.ix_FullLf1Ptr: return curr.as!(FullLf1*).prefix[];
        case Node.Ix.ix_LinBr4Ptr: return curr.as!(LinBr4*).prefix[];
        case Node.Ix.ix_FullBrMPtr: return curr.as!(FullBrM*).prefix[];
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Set prefix of branch node `curr` to `prefix`. */
    void setPrefix(Node curr, Ix[] prefix) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_OneLf6:    curr.as!(OneLf6).key  = typeof(curr.as!(OneLf6).key)(prefix); break;
        case Node.Ix.ix_SixLf1:    assert(prefix.length == 0); break;
        case Node.Ix.ix_FullLf1Ptr: curr.as!(FullLf1*).prefix = typeof(curr.as!(FullLf1*).prefix)(prefix); break;
        case Node.Ix.ix_LinBr4Ptr: curr.as!(LinBr4*).prefix = typeof(curr.as!(LinBr4*).prefix)(prefix); break;
        case Node.Ix.ix_FullBrMPtr: curr.as!(FullBrM*).prefix = typeof(curr.as!(FullBrM*).prefix)(prefix); break;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Pop `n`  from prefix. */
    void popFrontNPrefix(Node curr, size_t n) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FullLf1Ptr: curr.as!(FullLf1*).prefix.popFrontN(n); break;
        case Node.Ix.ix_LinBr4Ptr: curr.as!(LinBr4*).prefix.popFrontN(n); break;
        case Node.Ix.ix_FullBrMPtr: curr.as!(FullBrM*).prefix.popFrontN(n); break;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    Stats usageHistograms() const
    {
        typeof(return) stats;
        _root.calculate!(Value, span)(stats);
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

    const @safe pure nothrow /* TODO @nogc */
    {
        /** Returns: `true` if `key` is stored, `false` otherwise. */
        pragma(inline) bool contains(Key!span key)
        {
            return containsAt(_root, key);
        }

        /** Returns: `true` if `key` is stored under `curr`, `false` otherwise. */
        pragma(inline) bool containsAt(Node curr, Key!span key)
        {
            if (willFail) { dln("Will fail, key:", key, " curr:", curr); }
            import std.algorithm : skipOver;
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: return false;
            case ix_OneLf6: return curr.as!(OneLf6).contains(key);
            case ix_TwoLf3: return curr.as!(TwoLf3).contains(key);
            case ix_TriLf2: return curr.as!(TriLf2).contains(key);
            case ix_SixLf1: return curr.as!(SixLf1).contains(key);
            case ix_FullLf1Ptr:
                auto curr_ = curr.as!(FullLf1*);
                if (willFail) { dln("Will fail, key:", key, " curr:", *curr_, " currPrefix:", curr_.prefix[]); }
                return curr_.contains(key);
            case ix_LinBr4Ptr:
                auto curr_ = curr.as!(LinBr4*);
                return (key.skipOver(curr_.prefix) &&
                        ((key.length == 0 && curr_.isKey) ||                 // either stored at `curr`
                         (key.length >= 1 && containsAt(curr_.findSub(key[0]), key[1 .. $])))); // recurse
            case ix_FullBrMPtr:
                auto curr_ = curr.as!(FullBrM*);
                return (key.skipOver(curr_.prefix) &&
                        ((key.length == 0 && curr_.isKey) ||                 // either stored at `curr`
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
            case ix_OneLf6: break;
            case ix_TwoLf3: break;
            case ix_TriLf2: break;
            case ix_SixLf1: break;

            case ix_FullLf1Ptr:
                ++count;
                break;

            case ix_LinBr4Ptr:
                auto curr_ = curr.as!(LinBr4*);
                ++count;
                foreach (subNode; curr_.subNodeSlots[0 .. curr_.subPopulation])
                {
                    if (subNode) { count += countHeapNodesAt(subNode); }
                }
                break;

            case ix_FullBrMPtr:
                ++count;
                auto curr_ = curr.as!(FullBrM*);
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
        pragma(inline) Node insert(Key!span key, out bool wasAdded)
        {
            return _root = insertAt(_root, key, 0, wasAdded);
        }

        Node insertNew(Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            switch (key.length)
            {
            case 1:
                wasAdded = true;
                return Node(construct!(SixLf1)(key[0])); // promote packing
            case 2:
                wasAdded = true;
                return Node(construct!(TriLf2)(key)); // promote packing
            case 3:
                wasAdded = true;
                return Node(construct!(TwoLf3)(key)); // promote packing
            default:
                if (key.length <= OneLf6.maxLength)
                {
                    wasAdded = true;
                    return Node(construct!(OneLf6)(key));
                }
                else if (key.length <= FullLf1.maxPrefixLength) // only if FullLf1.maxPrefixLength > OneLf6.maxLength
                {
                    wasAdded = true;
                    debug if (willFail) { dln("prefix:", key[0 .. $ - 1]); }
                    auto prefix = key[0 .. $ - 1];
                    return Node(construct!(FullLf1*)(prefix, false, key[$ - 1]));
                }
                else                // key doesn't fit in a `OneLf6`
                {
                    import std.algorithm : min;
                    auto brKey = key[0 .. min(key.length, DefaultBr.maxPrefixLength)];
                    auto next = insertAt(Node(construct!(DefaultBr)(brKey, false)), // as much as possible of key in branch prefix
                                         key, superPrefixLength, wasAdded);

                    // assert that next branch shouldn't be a bit-packed branch instead
                    debug if (const nextLinBr4Ref = next.peek!(LinBr4*))
                    {
                        assert(!(*nextLinBr4Ref).hasMinimumDepth);
                    }
                    assert(wasAdded);
                    return next;
                }
            }
        }

        /** Insert `key` into sub-tree under root `curr`. */
        pragma(inline) Node insertAt(Node curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            assert(hasVariableKeyLength || superPrefixLength + key.length == fixedKeyLength);

            if (willFail) { dln("Will fail, key:", key, " curr:", curr, " superPrefixLength:", superPrefixLength); }
            if (!curr)          // if no existing `Node` to insert at
            {
                curr = insertNew(key, superPrefixLength, wasAdded);
                assert(wasAdded); // must be added to new Node
                return curr;
            }
            else
            {
                final switch (curr.typeIx) with (Node.Ix)
                {
                case undefined: return typeof(return).init;
                case ix_OneLf6: return insertAt(curr.as!(OneLf6), key, superPrefixLength, wasAdded);
                case ix_TwoLf3: return insertAt(curr.as!(TwoLf3), key, superPrefixLength, wasAdded);
                case ix_TriLf2: return insertAt(curr.as!(TriLf2), key, superPrefixLength, wasAdded);
                case ix_SixLf1: return insertAt(curr.as!(SixLf1), key, superPrefixLength, wasAdded);

                case ix_FullLf1Ptr:
                case ix_LinBr4Ptr:
                case ix_FullBrMPtr: return insertAtBranch(curr, key, superPrefixLength, wasAdded);
                }
            }
        }

        Node insertAtBranch(Node curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            assert(hasVariableKeyLength || superPrefixLength + key.length == fixedKeyLength);

            if (willFail) { dln("Will fail, key:", key,
                                " curr:", curr,
                                " superPrefixLength:", superPrefixLength,
                                " currPrefix:", getPrefix(curr)); }

            import std.algorithm : commonPrefix;
            auto currPrefix = getPrefix(curr);
            auto matchedKeyPrefix = commonPrefix(key, currPrefix);

            // prefix:"ab", key:"cd"
            if (matchedKeyPrefix.length == 0) // no prefix key match
            {
                if (currPrefix.length == 0) // no current prefix
                {
                    if (willFail) { dln("curr:", curr); }
                    // continue below
                }
                else
                {
                    const subIx = currPrefix[0]; // subIx = 'a'
                    if (willFail) { dln("curr:", curr, " subIx:", subIx); }
                    if (willFail) dln("before popFrontNPrefix: ", getPrefix(curr));
                    popFrontNPrefix(curr, 1);
                    if (willFail) dln("after popFrontNPrefix: ", getPrefix(curr));
                    if (willFail) dln("key: ", key);
                    return insertAt(Node(construct!(DefaultBr)(Ix[].init, false, subIx, curr)),
                                    key,
                                    superPrefixLength,
                                    wasAdded);
                }
            }
            else if (matchedKeyPrefix.length < key.length)
            {
                if (matchedKeyPrefix.length == currPrefix.length)
                {
                    if (willFail) { dln("curr:", curr, " superPrefixLength:", superPrefixLength, " matchedKeyPrefix:", matchedKeyPrefix); }
                    // most probable: key is an extension of prefix: prefix:"ab", key:"abcd"
                    key = key[matchedKeyPrefix.length .. $]; // strip `currPrefix from beginning of `key`
                    superPrefixLength += matchedKeyPrefix.length;
                    // continue below
                }
                else
                {
                    if (willFail) { dln("curr:", curr, " superPrefixLength:", superPrefixLength, " matchedKeyPrefix:", matchedKeyPrefix); }
                    // prefix and key share beginning: prefix:"ab11", key:"ab22"
                    const subIx = currPrefix[matchedKeyPrefix.length]; // need index first
                    popFrontNPrefix(curr, matchedKeyPrefix.length + 1); // drop matchedKeyPrefix plus index to next super branch
                    curr = Node(construct!(DefaultBr)(matchedKeyPrefix, false, // key is not occupied
                                                      subIx, curr));
                    key = key[matchedKeyPrefix.length .. $]; // skip matchedKeyPrefix from key
                    superPrefixLength += matchedKeyPrefix.length;
                }
            }
            else
            {
                if (matchedKeyPrefix.length < currPrefix.length)
                {
                    if (willFail) { dln("curr:", curr, " superPrefixLength:", superPrefixLength, " matchedKeyPrefix:", matchedKeyPrefix); }
                    // prefix is an extension of key: prefix:"abcd", key:"ab"
                    const subIx = currPrefix[matchedKeyPrefix.length]; // need index first
                    popFrontNPrefix(curr, matchedKeyPrefix.length + 1); // drop matchedKeyPrefix plus index to next super branch
                    return Node(construct!(DefaultBr)(matchedKeyPrefix, true, // `true` because `key` occupies this node
                                                      subIx, curr));
                }
            }

            if (currPrefix == key) // prefix equals key: prefix:"ab", key:"ab"
            {
                if (!isKey(curr))
                {
                    makeKey(curr);
                    wasAdded = true;
                }
                return curr;
            }

            assert(key.length != 0);
            const ix = key[0];
            if (willFail) { debug try { dln("ix:", ix); } catch (Exception e) {} }

            Node currSubNode = getSub(curr, ix);
            if (willFail) { debug try { dln("currSubNode:", currSubNode); } catch (Exception e) {} }

            Node nextSubNode = insertAt(currSubNode, // recurse
                                        key[1 .. $],
                                        superPrefixLength + 1,
                                        wasAdded);
            if (willFail) { debug try { dln(" superPrefixLength:", superPrefixLength,
                                            " curr:", curr,
                                            " getPrefix:", getPrefix(curr),
                                            " ix:", ix,
                                            " nextSubNode:", nextSubNode); } catch (Exception e) {} }

            return setSub(curr, ix, nextSubNode, superPrefixLength);
        }

        Node insertAt(OneLf6 curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            assert(hasVariableKeyLength || superPrefixLength + key.length == fixedKeyLength);

            import std.algorithm : commonPrefix;

            if (key.length == 0)
            {
                assert(curr.key.empty, "Leaf is not empty when key is");
                return Node(curr);
            }

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
                    case 0: next = construct!(SixLf1)(curr.key[0], key[0]); break;
                    case 1: next = construct!(TriLf2)(curr.key, key); break;
                    case 2: next = construct!(TwoLf3)(curr.key, key); break;
                    default:
                        if (willFail) { dln("matchedKeyPrefix:", matchedKeyPrefix); }
                        next = construct!(FullLf1*)(matchedKeyPrefix, false,
                                                    curr.key[$ - 1],
                                                    key[$ - 1]);
                        break;
                    }
                    wasAdded = true;
                    freeNode(curr);
                    return next;
                }
            }
            return insertAt(split(curr, matchedKeyPrefix, key, superPrefixLength),
                            key, superPrefixLength, wasAdded);
        }

        Node insertAt(TwoLf3 curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            assert(hasVariableKeyLength || curr.keyLength == key.length);
            assert(hasVariableKeyLength || superPrefixLength + key.length == fixedKeyLength);

            if (willFail) { dln("Will fail, key:", key, " curr:", curr, " superPrefixLength:", superPrefixLength); }
            if (curr.keyLength == key.length)
            {
                if (curr.contains(key)) { return Node(curr); }
                if (!curr.keys.full)
                {
                    curr.keys.pushBack(key);
                    wasAdded = true;
                    return Node(curr);
                }

                // TODO Use variadic commonPrefix(curr.keys[0], curr.keys[1], key)
                enum PL = curr.keyLength - 1;    // searched prefix length
                if (curr.keys[0][0 .. PL] ==          key[0 .. PL] &&
                    curr.keys[0][0 .. PL] == curr.keys[1][0 .. PL]) // if `curr` and `key` can be combined into a `FullLf1`
                {
                    auto next = construct!(FullLf1*)(key[0 .. PL], false);
                    foreach (const currKey; curr.keys)
                    {
                        next._keyBits[currKey[PL]] = true;
                    }
                    next._keyBits[key[PL]] = true;
                    freeNode(curr);
                    wasAdded = true;
                    return Node(next);
                }
            }
            return insertAt(expand(curr, superPrefixLength), key, superPrefixLength, wasAdded); // NOTE stay at same (depth)
        }

        Node insertAt(TriLf2 curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            assert(hasVariableKeyLength || curr.keyLength == key.length);
            assert(hasVariableKeyLength || superPrefixLength + key.length == fixedKeyLength);

            if (willFail) { dln("Will fail, key:", key, " curr:", curr, " superPrefixLength:", superPrefixLength); }
            if (curr.keyLength == key.length)
            {
                if (curr.contains(key)) { return Node(curr); }
                if (!curr.keys.full)
                {
                    curr.keys.pushBack(key);
                    wasAdded = true;
                    return Node(curr);
                }

                if (curr.keys[0][0] ==          key[0] &&
                    curr.keys[0][0] == curr.keys[1][0] &&
                    curr.keys[1][0] == curr.keys[2][0]) // if `curr` and `key` can be combined into a `FullLf1`
                {
                    auto next = construct!(FullLf1*)(key[0 .. 1], false);
                    foreach (const currKey; curr.keys)
                    {
                        next._keyBits[currKey[1]] = true;
                    }
                    next._keyBits[key[1]] = true;
                    freeNode(curr);
                    wasAdded = true;
                    return Node(next);
                }
            }
            return insertAt(expand(curr, superPrefixLength),
                            key, superPrefixLength, wasAdded); // NOTE stay at same (depth)
        }

        Node insertAt(SixLf1 curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            assert(hasVariableKeyLength || superPrefixLength + key.length == fixedKeyLength);
            if (!(hasVariableKeyLength || curr.keyLength == key.length))
            {
                dln(curr);
                dln(key);
            }
            assert(hasVariableKeyLength || curr.keyLength == key.length);

            if (willFail) { dln("Will fail, key:", key, " curr:", curr, " superPrefixLength:", superPrefixLength); }
            if (curr.keyLength == key.length)
            {
                if (curr.contains(key)) { return Node(curr); }
                if (!curr.keys.full)
                {
                    curr.keys.pushBack(key[0]);
                    wasAdded = true;
                    return Node(curr);
                }

                if (willFail) { dln("prefix empty"); }
                auto next = construct!(FullLf1*)(Ix[].init, false);
                foreach (const currKey; curr.keys)
                {
                    next._keyBits[currKey] = true;
                }
                next._keyBits[key[0]] = true;
                freeNode(curr);
                wasAdded = true;
                return Node(next);
            }
            if (willFail) { dln("curr:", curr, " key:", key, " superPrefixLength:", superPrefixLength); }
            return insertAt(expand(curr, superPrefixLength),
                            key, superPrefixLength, wasAdded); // NOTE stay at same (depth)
        }

        /** Split `curr` using `prefix`. */
        Node split(OneLf6 curr, Key!span prefix, Key!span key, size_t superPrefixLength) // TODO key here is a bit malplaced
        {
            assert(hasVariableKeyLength || curr.key.length == key.length);
            assert(hasVariableKeyLength || superPrefixLength + key.length == fixedKeyLength);

            Node next;
            if (curr.key.length == key.length) // balanced tree possible
            {
                switch (curr.key.length)
                {
                case 1:
                    if (prefix.length == 0)
                    {
                        freeNode(curr);
                        return Node(construct!(SixLf1)(curr.key)); // TODO removing parameter has no effect. why?
                    }
                    else if (prefix.length == 1)
                    {
                        assert(false, "Use P1Lf with single-length prefix and a maximum of 4 ");
                    }
                    else
                    {
                        if (willFail) { dln("prefix:", prefix); }
                        next = construct!(FullLf1*)(prefix, false);
                    }
                    break;
                case 2:
                    freeNode(curr);
                    return Node(construct!(TriLf2)(curr.key));
                case 3:
                    freeNode(curr);
                    return Node(construct!(TwoLf3)(curr.key));
                default:
                    break;
                }
            }

            // default case
            if (!next) { next = construct!(DefaultBr)(prefix, false); }

            bool wasAddedCurr;      // dummy
            auto superNext = insertAt(next, curr.key, 0, wasAddedCurr);
            assert(wasAddedCurr); // assure that `curr` was reinserted
            freeNode(curr);   // remove old current

            return superNext;
        }

        /** Destructively expand `curr` and return it. */
        Node expand(TwoLf3 curr, size_t superPrefixLength)
        {
            assert(hasVariableKeyLength || superPrefixLength + curr.keyLength == fixedKeyLength);

            Node next;
            if (curr.keys.length == 1) // only one key
            {
                next = construct!(DefaultBr)(Ix[].init); // so no prefix
                bool wasAddedCurr;
                next = insertAtBranch(next,
                                      curr.keys[0],
                                      superPrefixLength,
                                      wasAddedCurr);
                assert(wasAddedCurr);
            }
            else
            {
                auto currPrefix = curr.prefix;
                next = construct!(DefaultBr)(currPrefix);
                // TODO functionize:
                foreach (key; curr.keys) // TODO const key
                {
                    bool wasAddedCurr;
                    next = insertAtBranch(next,
                                          key[currPrefix.length .. $],
                                          superPrefixLength + currPrefix.length,
                                          wasAddedCurr);
                    assert(wasAddedCurr);
                }
            }
            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` and return it. */
        Node expand(TriLf2 curr, size_t superPrefixLength)
        {
            assert(hasVariableKeyLength || superPrefixLength + curr.keyLength == fixedKeyLength);

            // TODO functionize:
            Node next;
            if (curr.keys.length == 1) // only one key
            {
                next = construct!(DefaultBr)(Ix[].init); // so no prefix
                bool wasAddedCurr;
                next = insertAtBranch(next,
                                      curr.keys[0],
                                      superPrefixLength,
                                      wasAddedCurr);
                assert(wasAddedCurr);
            }
            else
            {
                auto currPrefix = curr.prefix;
                next = construct!(DefaultBr)(currPrefix);
                foreach (key; curr.keys) // TODO const key
                {
                    bool wasAddedCurr;
                    auto subKey = key[currPrefix.length .. $];
                    assert(subKey.length != 0);
                    next = insertAtBranch(next,
                                          subKey,
                                          superPrefixLength + currPrefix.length,
                                          wasAddedCurr);
                    assert(wasAddedCurr);
                }
            }
            freeNode(curr);
            return next;
        }

        /** Destructively expand `curr` making room for `nextKey` and return it. */
        Node expand(SixLf1 curr, size_t superPrefixLength)
        {
            assert(hasVariableKeyLength || superPrefixLength + curr.keyLength == fixedKeyLength);

            auto next = construct!(FullLf1*);
            foreach (const ixM; curr.keys)
            {
                assert(!next._keyBits[ixM]); // assert no duplicates in keys
                next._keyBits[ixM] = true;
            }
            freeNode(curr);
            return Node(next);
        }

    }

    /** Returns: `true` iff tree is empty (no elements stored). */
    bool empty() const @safe pure nothrow /* TODO @nogc */ { return !_root; }

    /** Returns: number of elements store. */
    size_t length() const @safe pure nothrow /* TODO @nogc */ { return _length; }

    private:

    /** Allocate (if pointer) and Construct a `Node`-type of value type `NodeType`
        using constructor arguments `args` of `Args`. */
    auto construct(NodeType, Args...)(Args args) @trusted
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
        void release(FullLf1* curr)
        {
            freeNode(curr);
        }

        void release(LinBr4* curr)
        {
            foreach (sub; curr.subNodes[0 .. curr.subPopulation])
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(FullBrM* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(OneLf6 curr) { freeNode(curr); }
        void release(TwoLf3 curr) { freeNode(curr); }
        void release(TriLf2 curr) { freeNode(curr); }
        void release(SixLf1 curr) { freeNode(curr); }

        void release(Node curr)
        {
            version(debugAllocations) { dln("releasing Node ", curr); }
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: break; // ignored
            case ix_OneLf6: return release(curr.as!(OneLf6));
            case ix_TwoLf3: return release(curr.as!(TwoLf3));
            case ix_TriLf2: return release(curr.as!(TriLf2));
            case ix_SixLf1: return release(curr.as!(SixLf1));
            case ix_FullLf1Ptr: return release(curr.as!(FullLf1*));
            case ix_LinBr4Ptr: return release(curr.as!(LinBr4*));
            case ix_FullBrMPtr: return release(curr.as!(FullBrM*));
            }
        }

        bool isHeapAllocatedNode(Node curr) const
        {
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: return false;
            case ix_OneLf6: return false;
            case ix_TwoLf3: return false;
            case ix_TriLf2: return false;
            case ix_SixLf1: return false;
            case ix_FullLf1Ptr: return true;
            case ix_LinBr4Ptr: return true;
            case ix_FullBrMPtr: return true;
            }
        }
    }

    /** Returns: `true` if all keys in tree are of fixed length/size, `false` otherwise. */
    bool hasFixedKeyLength() const @safe pure nothrow @nogc
    {
        return (fixedKeyLength !=
                fixedKeyLengthUndefined);
    }
    /** Returns: `true` if keys in tree may be of variable length/size, `false` otherwise. */
    bool hasVariableKeyLength() const @safe pure nothrow @nogc
    {
        return !hasFixedKeyLength;
    }

    /// Returns: number of nodes used in `this` tree. Should always equal `Stats.heapNodeCount`.
    pragma(inline) debug size_t heapNodeAllocationBalance() @safe pure nothrow /* TODO @nogc */
    {
        return _heapNodeAllocationBalance;
    }

    void print() @safe const
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
        case undefined: break;
        case ix_OneLf6:
            auto curr_ = curr.as!(OneLf6);
            writeln(typeof(curr_).stringof, "#", curr_.key.length, ": ", curr_.key);
            break;
        case ix_TwoLf3:
            auto curr_ = curr.as!(TwoLf3);
            writeln(typeof(curr_).stringof, "#", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_TriLf2:
            auto curr_ = curr.as!(TriLf2);
            writeln(typeof(curr_).stringof, "#", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_SixLf1:
            auto curr_ = curr.as!(SixLf1);
            writeln(typeof(curr_).stringof, "#", curr_.keys.length, ": ", curr_.keys);
            break;
        case ix_FullLf1Ptr:
            auto curr_ = curr.as!(FullLf1*);
            write(typeof(*curr_).stringof, "#", curr_._keyBits.countOnes, (curr_.isKey ? " SET" : " UNSET"));
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            write(": ");

            // keys
            size_t ix = 0;
            bool other = false;
            foreach (const value;  curr_._keyBits[])
            {
                string s;
                if (value)
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
        case ix_LinBr4Ptr:
            auto curr_ = curr.as!(LinBr4*);
            write(typeof(*curr_).stringof, "#", curr_.subPopulation, (curr_.isKey ? " SET" : " UNSET"));
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            writeln(":");
            foreach (const i, const subNode; curr_.subNodes)
            {
                printAt(subNode, depth + 1, cast(uint)curr_.subIxs[i]);
            }
            break;
        case ix_FullBrMPtr:
            auto curr_ = curr.as!(FullBrM*);
            write(typeof(*curr_).stringof, "#", curr_.subPopulation, (curr_.isKey ? " SET" : " UNSET"));
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            writeln(":");
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

/** Append statistics of tree under `Node` `sub.` into `stats`.
 */
static private void calculate(Value, uint span)(RawRadixTree!(Value, span).Node sub,
                                                ref RawRadixTree!(Value, span).Stats stats)
    @safe pure nothrow /* TODO @nogc */
{
    alias RT = RawRadixTree!(Value, span);
    ++stats.popByNodeType[sub.typeIx];

    with (RT.Node.Ix)
    {
        final switch (sub.typeIx)
        {
        case undefined: break;
        case ix_OneLf6: break; // TODO calculate()
        case ix_TwoLf3: break; // TODO calculate()
        case ix_TriLf2: break; // TODO calculate()
        case ix_SixLf1: break; // TODO calculate()
        case ix_FullLf1Ptr: ++stats.heapNodeCount; sub.as!(RT.FullLf1*).calculate(stats); break;
        case ix_LinBr4Ptr: ++stats.heapNodeCount; sub.as!(RT.LinBr4*).calculate(stats); break;
        case ix_FullBrMPtr: ++stats.heapNodeCount; sub.as!(RT.FullBrM*).calculate(stats); break;
        }
    }
}

/** Remap typed key `typedKey` to untype key of `Key`. */
static private Key!span remapKey(TypedKey, uint span = 8)(in TypedKey typedKey)
    @trusted pure nothrow /* TODO @nogc */
    if (allSatisfy!(isTrieableKeyType, TypedKey))
{
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
    else static if (is(Unqual!TypedKey == string))
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
        assert(false, "TODO");
    }
}

/// Radix-Tree with key-type `TypedKey` and value-type `Value`.
struct RadixTree(TypedKey, Value, uint span = 8)
    if (allSatisfy!(isTrieableKeyType, TypedKey))
{
    this(bool unusedDummy)      // TODO how do we get rid of the need for `unusedDummy`?
    {
        this.fixedKeyLength = isFixedTrieableKeyType!TypedKey ? TypedKey.sizeof : fixedKeyLengthUndefined;
    }

    /** Insert `key`.
        Returns: `true` if `key` wasn't previously inserted, `false` otherwise.
     */
    bool insert(in TypedKey typedKey)
        @safe pure nothrow /* TODO @nogc */
    {
        // dln("inserting typedKey:", typedKey);
        import std.string : representation;

        // convert unsigned to fixed-length (on the stack) ubyte array

        bool wasAdded = false; // indicates that key was added
        _tree.insert(typedKey.remapKey, wasAdded);
        _length += wasAdded;

        return wasAdded;
    }

    static if (_tree.isSet)
    {
        const nothrow:

        /** Returns: `true` if `key` is stored, `false` otherwise. */
        bool contains(in TypedKey typedKey)
        {
            return _tree.contains(typedKey.remapKey);
        }
    }

    static if (_tree.isMap)
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        bool insert(in TypedKey key, Value value)
        {
            bool result = insert(key);
            return result;
        }

        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        Value* contains(in TypedKey key) const
        {
            return null;
        }
    }

    /** Supports $(B `TypedKey` in `this`) syntax. */
    auto opBinaryRight(string op)(in TypedKey key) const if (op == "in") { return contains(key); }

    void print() @safe const
    {
        _tree.print();
    }

    private RawRadixTree!(Value, span) _tree;
    alias _tree this;
}
alias PatriciaTrie = RadixTree;
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator of set-version of `RadixTree` where value-type is `void` (unused).
auto radixTreeSet(Key, uint span = 8)() { return RadixTree!(Key, void, span)(false); }

/// Instantiator of map-version of `RadixTree` where value-type is `Value`.
auto radixTreeMap(Key, Value, uint span = 8)() { return RadixTree!(Key, Value, span)(false); }

// @safe pure nothrow /* TODO @nogc */
unittest
{
    auto set = radixTreeSet!(ulong);

    assert(set.insert(0));
    assert(!set.insert(0));
    assert(set.heapNodeAllocationBalance == 1);

    assert(set.insert(1));
    assert(!set.insert(1));
    assert(set.heapNodeAllocationBalance == 1);

    foreach (const i; 2 .. 256)
    {
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.heapNodeAllocationBalance == 1);
    }

    assert(set.insert(256));
    assert(!set.insert(256));
    assert(set.heapNodeAllocationBalance == 2);
}

@safe pure nothrow /* TODO @nogc */
unittest
{
    auto set = radixTreeSet!(ubyte);
    alias Set = typeof(set);

    foreach (const i; 0 .. Set.SixLf1.maxLength)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.contains(i));
        assert(set.heapNodeAllocationBalance == 0);
        const rootRef = set._root.peek!(Set.SixLf1);
        assert(rootRef);
    }

    foreach (const i; Set.SixLf1.maxLength .. 256)
    {
        assert(!set.contains(i));
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.contains(i));
        assert(set.heapNodeAllocationBalance == 1);
        const rootRef = set._root.peek!(Set.FullLf1*);
        assert(rootRef);
    }

    const rootRef = set._root.peek!(Set.FullLf1*);
    assert(rootRef);

    const root = *rootRef;
    assert(!root.empty);
    assert(root.full);
}

@safe pure nothrow /* TODO @nogc */
unittest
{
    import std.meta : AliasSeq;
    foreach (T; AliasSeq!(ushort, uint))
    {
        auto set = radixTreeSet!(T);
        alias Set = typeof(set);

        assert(!set.contains(0));
        assert(set.insert(0));
        assert(!set.insert(0));
        assert(set.contains(0));
        assert(set.heapNodeAllocationBalance == 0);

        foreach (const i; 1 .. 256)
        {
            assert(!set.contains(i));
            assert(set.insert(i));
            assert(!set.insert(i));
            assert(set.contains(i));
        }

        assert(!set.contains(256));
        assert(set.insert(256));
        assert(!set.insert(256));
        assert(set.contains(256));
        assert(set.heapNodeAllocationBalance == 2);

        assert(!set.contains(257));
        assert(set.insert(257));
        assert(!set.insert(257));
        assert(set.contains(257));

        const rootRef = set._root.peek!(Set.DefaultBr);
        assert(rootRef);
        const root = *rootRef;
        assert(root.prefix.length == T.sizeof - 2);
    }
}

auto checkString(uint span, Keys...)()
    if (Keys.length >= 1)
{
    import std.range : iota;
    foreach (Key; Keys)
    {
        auto set = radixTreeSet!(Key, span);
        alias Set = set;
        assert(set.empty);

        import std.random : Random, uniform;
        auto gen = Random();
        const maxLength = 10;

        const count = 100_000;
        bool[string] elements;  // set of strings using D's builtin associative array
        while (elements.length < count)
        {
            const length = uniform(1, maxLength, gen);
            auto key = new char[length];
            foreach (ix; 0 .. length)
            {
                key[ix] = cast(char)('a' + 0.uniform(26, gen));
            }
            elements[key[].idup] = true;
        }

        foreach (const key; elements.byKey)
        {
            import std.string : representation;
            dln("key:", key, " (", key.representation, ")");
            set.willFail = key == "rxo";

            dln("assert(!set.contains(key)) ################################ : ");
            assert(!set.contains(key));

            dln("assert(set.insert(key)) ################################ : ");
            assert(set.insert(key));

            dln("assert(!set.insert(key)) ################################ :");
            assert(!set.insert(key));

            dln("assert(set.contains(key)) ################################ :");
            if (set.willFail)
            {
                set.print();
            }
            assert(set.contains(key));
        }
    }
}

//pure /* TODO @nogc */
unittest
{
    checkString!(8, string);
}

/// Check correctness when span is `span` and for each `Key` in `Keys`.
auto checkNumeric(uint span, Keys...)()
    if (Keys.length >= 1)
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        import std.algorithm : equal;
        struct TestValueType { int i; float f; string s; }
        alias Value = TestValueType;
        import std.meta : AliasSeq;
        foreach (Key; Keys)
        {
            dln("Key: ", Key.stringof);
            alias Tree = radixTreeSet!(Key, span);
            auto set = Tree;
            assert(set.hasFixedKeyLength == isFixedTrieableKeyType!Key);
            assert(set.empty);

            static assert(set.isSet);

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
                    debug set.willFail = (key == -32639);
                    if (useContains)
                    {
                        if (set.willFail) dln("before check no contains yet");
                        assert(!set.contains(key)); // key should not yet be in set
                        assert(key !in set);        // alternative syntax
                    }

                    if (set.willFail) dln("before first insert()");
                    assert(set.insert(key));  // insert new value returns `true` (previously not in set)
                    if (useContains)
                    {
                        if (set.willFail) dln("before check passing contains()");
                        assert(set.contains(key)); // key should now be in set
                    }
                    if (set.willFail) dln("before second insert()");
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

            auto map = radixTreeMap!(Key, Value, span);
            assert(map.hasFixedKeyLength == isFixedTrieableKeyType!Key);
            static assert(map.isMap);

            map.insert(Key.init, Value.init);
        }
    }
}

/// Benchmark performance and memory usage when span is `span`.
void benchmark(uint span)()
{
    import core.thread : sleep;
    import std.range : iota;
    import std.stdio : writeln;

    import std.algorithm : equal;
    struct TestValueType { int i; float f; string s; }
    alias Value = TestValueType;
    import std.meta : AliasSeq;
    foreach (Key; AliasSeq!(uint)) // just benchmark uint for now
    {
        auto set = radixTreeSet!(Key, span);
        alias Set = set;
        assert(set.empty);

        static assert(set.isSet);

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
            auto stats = set.usageHistograms;
            writeln("FullLf1 Population Histogram: ", stats.popHist_FullLf1);
            writeln("LinBr4 Population Histogram: ", stats.popHist_LinBr4);
            writeln("FullBrM radix=", 2^^span, "-Branch Population Histogram: ", stats.popHist_FullBrM);
            writeln("Population By Node Type: ", stats.popByNodeType);

            // these should be zero
            writeln("Number of LinBr4 with OneLf6-0 only subNodes: ", stats.allOneLf60CountOfLinBr4);
            writeln("Number of FullBrM with OneLf6-0 only subNodes: ", stats.allOneLf60CountOfFullBrM);

            size_t totalBytesUsed = 0;
            foreach (Set.Node.Ix ix, pop; stats.popByNodeType) // TODO use stats.byPair when added to typecons_ex.d
            {
                size_t bytesUsed = 0;
                with (Set.Node.Ix)
                {
                    final switch (ix)
                    {
                    case undefined: break;
                    case ix_OneLf6: bytesUsed = pop*Set.OneLf6.sizeof; break;
                    case ix_TwoLf3: bytesUsed = pop*Set.TwoLf3.sizeof; break;
                    case ix_TriLf2: bytesUsed = pop*Set.TriLf2.sizeof; break;
                    case ix_SixLf1: bytesUsed = pop*Set.SixLf1.sizeof; break;
                    case ix_FullLf1Ptr: bytesUsed = pop*Set.FullLf1.sizeof; totalBytesUsed += bytesUsed; break;
                    case ix_LinBr4Ptr: bytesUsed = pop*Set.LinBr4.sizeof; totalBytesUsed += bytesUsed; break;
                    case ix_FullBrMPtr: bytesUsed = pop*Set.FullBrM.sizeof; totalBytesUsed += bytesUsed; break;
                    }
                }
                if (bytesUsed)
                {
                    writeln(pop, " number of ", ix, " uses ", bytesUsed/1e6, " megabytes");
                }
            }
            writeln("Tree uses ", totalBytesUsed/1e6, " megabytes");

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

        // version(print)
        set.print();

        auto map = radixTreeMap!(Key, Value, span);
        assert(map.empty);
        static assert(map.isMap);

        map.insert(Key.init, Value.init);
    }
}

/// leak test
version(enterSingleInfiniteMemoryLeakTest)
@safe pure nothrow /* TODO @nogc */
unittest
{
    while (true)
    {
        checkNumeric!(8, float);
    }
}

@safe pure nothrow /* TODO @nogc */
unittest
{
    checkNumeric!(8,
                  float, double,
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
            alias Tree = radixTreeSet!(Key, span);
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

            version(print) set.print();
        }
    }
}

version(print) @safe unittest
{
    testPrint!(8,
               double, float,
               long, int, short, byte,
               ulong, uint, ushort, ubyte,
        );
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
    benchmark!8;
}
