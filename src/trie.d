/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO tuple keys are mapped to a ubyte array aliased to a raw/binary
    BKey. Need trait to figure if all expanded members are fixed-sized then key
    will be that aswell.

    TODO - `set.prefix("alpha")`                           => `SortedTreeRange` of `Tuple!(string, Lang, PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha"))`                    => `SortedTreeRange` of `Tuple!(Lang, PoT, Sense)`.
    TODO - `set.prefix(tuple("alpha", Lang.en, PoT.noun))` => `SortedTreeRange` of `Sense`.
    TODO - `set.prefix(tuple("alpha", Lang.en))`           => `SortedTreeRange` of `Tuple!(PoT, Sense)`.

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type BrM*?

    TODO Add sparse 2^^n-branches for n < radix: 2^^1=B2, 2^^2=B4, 2^^3=B8, B^^4=B16. Use
    sortExactly from sortn.d to order their members.

    TODO Make Br2 templated on N when I figure out how
    to elide the recursive template-instantiation. Ask forums
    TODO Templatize node types including SBrXX on `N` and `BKey` but *not* on
    `Key` and instantiate as N 2, 4, 16, 256.

    TODO Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`
    TODO Provide RandomAccess `opIndex` and `opSlice`! for variable-length keys aswell?
    TODO Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to _root. Add checks with `isSorted`.

    TODO Should opBinaryRight return void* instead of bool for set-case?
 */
module trie;

import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy, isPointer;
import std.typecons : tuple, Tuple;
import std.range : isInputRange, ElementType;

import bijections : isIntegralBijectableType, bijectToUnsigned;
import variant_ex : WordVariant;
import typecons_ex : IndexedArray, StrictlyIndexed;
import modulo : Mod;

version = benchmark;

import dbg : dln;

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

/** Index to chunk of bits. */
alias ChunkIx = uint;

/** Radix tree container storing keys of type `Key`.

    In set-case (`Value` is `void`) this container is especially suitable for
    representing a set of 32 or 64 integers/pointers.

    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Key,
                 Value,
                 size_t radix = 4) // radix in number of bits, typically either 1, 2, 4 or 8
    if (allSatisfy!(isTrieableKeyType, Key))
{
    import std.algorithm : filter;
    import std.meta : AliasSeq, staticMap;
    import std.typecons : ConstOf;

    static assert(radix == 4 ||
                  radix == 8 ||
                  radix == 16 ||
                  radix == 24, "Radix is currently limited to either 4, 8, 16, or 24");
    static assert(radix <= 8*Key.sizeof, "Radix must be less than or equal to Key bit-precision"); // TODO Use strictly less than: radix < ... instead?

    /** Raw Internal (Unsigned Integer) Binary Key. */
    alias BKey = typeof(Key.init.bijectToUnsigned);

    enum isSet = is(Value == void); // `true` if this tree is a set
    enum isMap = !isSet;        // `true` if this tree is a map

    alias R = radix;
    enum M = 2^^R;     // branch-multiplicity, typically either 2, 4, 16 or 256
    enum chunkMask = M - 1;

    alias order = M;   // tree order

    /// `true` if tree has fixed a key of fixed length and in turn a tree of fixed max depth.
    enum hasFixedDepth = isFixedTrieableKeyType!Key;

    static if (hasFixedDepth)
    {
        /// Maximum depth.
        enum maxDepth = 8*Key.sizeof / R;
    }

    /// `true` if tree has binary branch.
    enum isBinary = R == 2;

    /** Radix Modulo Index */
    alias IxM = Mod!M; // restricted index type avoids range checking in array indexing below

    /** `R` least significant bits (LSB) of leaves directly packed into a word.

        TODO Generalize to packing of more than one `IxM` per byte.
        TODO respect byteorder in `PLfs` to work with `WordVariant`
        TODO implement and use opSlice instead of .ixMs[]
     */
    static      if (size_t.sizeof == 4)
    {
        static if (radix == 4) { struct PLfs { enum maxLength = 2; IxM[maxLength] ixMs; ubyte length; ubyte _ignored; } } // TODO pack 6 IxM
        static if (radix == 8) { struct PLfs { enum maxLength = 2; IxM[maxLength] ixMs; ubyte length; ubyte _ignored; } } // TODO handle radix != 8
        static if (isMap && is(Value == bool)) { /* TODO pack bit efficiently */ }
    }
    else static if (size_t.sizeof == 8)
    {
        static if (radix == 4)
        {
            struct PLfs
            {
                enum maxLength = 6;
                IxM[maxLength] ixMs; // TODO pack 14 `IxM` through a range interface
                ubyte length;
                ubyte _ignored;
                static if (isMap)
                {
                    static if (is(Value == bool))
                        BitSet!maxLength values; // memory-efficient storage of `bool` values
                    else
                        Value[maxLength] values;
                }
            }
        }
        else static if (radix == 8)
        {
            struct PLfs
            {
                enum maxLength = 6;
                IxM[maxLength] ixMs;
                ubyte length;
                ubyte _ignored;
                static if (isMap)
                {
                    static if (is(Value == bool))
                        BitSet!maxLength values; // memory-efficient storage of `bool` values
                    else
                        Value[maxLength] values;
                }
            }
        }
        // TODO handle radix != 8
        static if (isMap && is(Value == bool))
        {
            /* TODO pack bit efficiently */
        }
    }

    // TODO make these CT-params (requires putting branch definitions in same scope as `RadixTree`)
    static if (radix == 4)
    {
        alias DefaultRootType = Br2*;
    }
    else static if (radix == 8)
    {
        alias DefaultRootType = BrM*;
    }
    alias DefaultBranchType = DefaultRootType;
    alias DefaultLeafType = PLfs; // TODO use either LfM* or PLfs instead

    static if (isSet)
        static assert(PLfs.sizeof == size_t.sizeof); // assert that it's size matches platform word-size

    /** Indicate that all leaves in this branch are set (denseness compression) */
    struct All1 {}

    /** Node types. */
    alias NodeTypes = AliasSeq!(PLfs, // sparse leaves
                                All1, // hinter

                                // sparse branches
                                Br2*,

                                BrM*,  // dense branches
                                LfM*); // dense leaves

    enum showSizes = false;
    static if (showSizes)
    {
        static if (isSet)
        {
            pragma(msg, "Set Br2.sizeof: ", Br2.sizeof);
            pragma(msg, "Set BrM.sizeof: ", BrM.sizeof);
            pragma(msg, "Set LfM.sizeof: ", LfM.sizeof);
            pragma(msg, "Set PLfs.sizeof: ", PLfs.sizeof);
        }
        else
        {
            pragma(msg, "Map Br2.sizeof: ", Br2.sizeof);
            pragma(msg, "Map BrM.sizeof: ", BrM.sizeof);
            pragma(msg, "Map LfM.sizeof: ", LfM.sizeof);
            pragma(msg, "Set PLfs.sizeof: ", PLfs.sizeof);
        }
    }

    /** Mutable node. */
    alias Node = WordVariant!NodeTypes;
    /** Constant node. */
    // TODO make work with indexNaming alias ConstNodePtr = WordVariant!(staticMap!(ConstOf, NodeTypes));

    static assert(radix <= 8*IxM.sizeof, "Need more precision in IxM");

    /** Tree Leaf Iterator. */
    struct It
    {
        bool opCast(T : bool)() const @safe pure nothrow @nogc { return cast(bool)node; }
        Node node;              // current leaf-`Node`. TODO use `Lf` type instead?
        IxM ixM;                // index to sub at `node`
    }

    /** Tree Key Find Result. */
    struct KeyFindResult // TODO shorter naming
    {
        /* this save 8 bytes and makes this struct 16 bytes instead of 24 bytes
           compared using a member It instead of Node and IxM */
        auto it() { return It(node, ixM); }
        bool opCast(T : bool)() const @safe pure nothrow @nogc { return hit; }
        Node node;
        IxM ixM;
        bool hit;
    }

    /** Tree Range. */
    struct Range
    {
        It low;                 // start
        It high;                // beyond last
    }

    // TODO move these definitions inside branch definitions?

    /** M-Branch population histogram.
        Index maps to population with value range (1 .. M).
    */
    alias BrM_PopHist = size_t[M];

    /** 2-Branch population histogram.
        Index maps to population with value range (1 .. 2).
    */
    alias Br2_PopHist = size_t[2];

    /** M-Leaf population histogram.
        Index maps to population with value range (1 .. M).
    */
    alias LeafM_PopHist = size_t[M];

    /** Tree Population and Memory-Usage Statistics. */
    struct Stats
    {
        Br2_PopHist popHist_Br2;
        BrM_PopHist popHist_BrM;
        LeafM_PopHist popHist_LfM;

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));
    }

    /** Dense M-Branch with `M` number of sub-nodes. */
    static private struct BrM
    {
        StrictlyIndexed!(Node[M]) subNodes;

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow @nogc const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.popHist_BrM[nnzSubCount - 1]; // TODO type-safe indexing
        }

        static if (!hasFixedDepth)        // variable length keys only
        {
            LfM subOccupations; // if i:th bit is set key (and optionally value) associated with sub[i] is also defined
            static if (isMap) { Value value; }
        }
    }

    /** Sparse/Packed 2-Branch. */
    static private struct Br2
    {
        enum N = 2;

        // TODO merge these into a new `NodeType`
        StrictlyIndexed!(Node[N]) subNodes;
        StrictlyIndexed!(IxM[N]) subChunks; // sub-ixMs. TODO Use IxMArray!N instead.

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.popHist_Br2[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    /** Bottom-most leaf node of tree storing `M` number of densly packed keys
        of fixed-length type `Key`.
    */
    static private struct LfM
    {
        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure const
        {
            ++stats.popHist_LfM[keyLSBits.countOnes - 1];
        }

        import bitset : BitSet;
        private BitSet!M keyLSBits; // if i:th bit is set, then corresponding sub is set

        static if (isMap)
        {
            static if (is(Value == bool))
                BitSet!M values; // memory-efficient storage of `bool` values
            else
                Value[M] values;
        }
    }

    Stats usageHistograms() const
    {
        typeof(return) stats;
        _root.calculate!(Key, Value, radix)(stats);
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
        assert(_nodeCount == 0);
    }

    @safe pure nothrow @nogc
    {
        /** Insert `key`.
            Recursive implementation of insert.
        */
        bool insert(in Key key)
        {
            ensureRootNode;
            bool wasAdded = false; // indicates that key was added
            _root = insert(_root, key.bijectToUnsigned, 0, wasAdded);
            _length += wasAdded;
            return wasAdded;
        }

        pragma(inline) Node insert(Node curr, in BKey ukey, ChunkIx chunkIx, out bool wasAdded) // Node-polymorphic
        {
            with (Node.Ix)
            {
                final switch (curr.typeIx)
                {
                case undefined: break;
                case ix_PLfs:   return insert(curr.as!(PLfs), ukey, chunkIx, wasAdded);
                case ix_Br2Ptr: return insert(curr.as!(Br2*), ukey, chunkIx, wasAdded);
                case ix_BrMPtr: return insert(curr.as!(BrM*), ukey, chunkIx, wasAdded);
                case ix_LfMPtr: return insert(curr.as!(LfM*), ukey, chunkIx, wasAdded);
                case ix_All1:   auto curr_ = curr.as!All1; break;
                }
                assert(false);
            }
        }

        Node insert(Br2* curr, in BKey ukey, ChunkIx chunkIx, out bool wasAdded)
        {
            const IxM chunk = bitsChunk!radix(ukey, chunkIx);

            enum N = 2;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                if (curr.subNodes[subIx])   // first is occupied
                {
                    if (curr.subChunks[subIx] == chunk) // and matches chunk
                    {
                        curr.subNodes[subIx] = insert(curr.subNodes[subIx], ukey, chunkIx + 1, wasAdded);
                        return Node(curr);
                    }
                }
                else            // use first free sub
                {
                    curr.subNodes[subIx] = insert(constructSub(chunkIx + 1), ukey, chunkIx + 1, wasAdded); // use it
                    curr.subChunks[subIx] = chunk;
                    return Node(curr);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            return insert(expand(curr), ukey, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
        }

        Node insert(BrM* curr, in BKey ukey, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth) assert(chunkIx + 1 < maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {
            const IxM chunk = bitsChunk!radix(ukey, chunkIx);
            if (!curr.subNodes[chunk]) // if not yet set
            {
                curr.subNodes[chunk] = constructSub(chunkIx + 1);
            }
            curr.subNodes[chunk] = insert(curr.subNodes[chunk], ukey, chunkIx + 1, wasAdded);
            return Node(curr);
        }

        Node insert(LfM* curr, in BKey ukey, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth) assert(chunkIx + 1 == maxDepth);
            else                      assert(chunkIx + 1 <= maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {
            const IxM chunk = bitsChunk!radix(ukey, chunkIx);
            if (!curr.keyLSBits[chunk])
            {
                curr.keyLSBits[chunk] = true;
                wasAdded = true;
            }
            else
            {
                wasAdded = false;
            }
            return Node(curr);
        }

        Node insert(PLfs curr, in BKey ukey, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth) assert(chunkIx + 1 == maxDepth);
            else                      assert(chunkIx + 1 <= maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {
            const IxM chunk = bitsChunk!radix(ukey, chunkIx);

            // TODO this is only marginally faster:
            // foreach (const i; iota!(0, curr.maxLength))
            // {
            //     if (i == curr.length) break;
            //     else if (curr.ixMs[i] == chunk) { return Node(curr); }
            // }

            import std.algorithm.searching : canFind;
            if (curr.ixMs[0 .. curr.length].canFind(chunk)) // if already stored. TODO use binarySearch
            {
                return Node(curr); // already there, so return current node as is
            }

            if (curr.length < curr.maxLength) // if there's room left in curr
            {
                curr.ixMs[curr.length++] = chunk;
                // import sortn : networkSortUpTo;
                // TODO curr.ixMs[0 .. length].networkSort;
                // TODO curr.ixMs[0 .. length].sort;
                wasAdded = true;
                return Node(curr); // current node still ok
            }
            else
            {
                return insert(expand(curr), ukey, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
            }
        }

        Node constructSub(ChunkIx chunkIx)
        {
            return (chunkIx + 1 == maxDepth ? // is last
                    Node(construct!DefaultLeafType) :
                    Node(construct!DefaultBranchType));
        }

        /** Destructively expand `curr` into a `BrM` and return it. */
        BrM* expand(Br2* curr) @trusted
        {
            enum N = 2;         // branch-order, number of possible sub-nodes
            auto next = construct!(typeof(return));
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                next.subNodes[curr.subChunks[subIx]] = curr.subNodes[subIx];
            }
            freeNode(curr);
            return next;
        }

        LfM* expand(PLfs curr) @trusted
        {
            auto next = construct!(typeof(return));
            foreach (const ixM; curr.ixMs[0 .. curr.length])
            {
                assert(!next.keyLSBits[ixM]); // assert no duplicates in ixMs
                next.keyLSBits[ixM] = true;
            }
            freeNode(curr);
            return next;
        }

    }

    static if (isSet)
    {
        /** Returns: `true` if key is contained in set, `false` otherwise. */
        bool contains(in Key key) const nothrow
        {
            return false;
        }

	/** Supports $(B `Key` in `this`) syntax.
            TODO return `NodeRef` instead.
        */
	bool opBinaryRight(string op)(in Key key) const nothrow if (op == "in") { return contains(key); }
    }

    static if (isMap)
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        bool insert(in Key key, Value value)
        {
            bool result = insert(key);
            // TODO call insertAtSubNode(result, value);
            return result;
        }
        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        Value* contains(in Key key) const
        {
            return null;
        }
    }

    /** Returns: `true` iff tree is empty (no elements stored). */
    bool empty() const @safe pure nothrow @nogc { return !_root; }

    /** Returns: number of elements store. */
    size_t length() const @safe pure nothrow @nogc { return _length; }

    private:

    /** Allocate `Node`-type of value type `U`. */
    auto construct(U)() @trusted
    {
        debug ++_nodeCount;
        static if (isPointer!U)
        {
            import std.conv : emplace;
            auto node = emplace(cast(U)malloc((*U.init).sizeof));
            // TODO ensure alignment of node at least that of U.alignof
            return node;
        }
        else
        {
            return U.init;
        }
    }

    @safe pure nothrow @nogc
    {
        void release(BrM* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(Br2* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(LfM* curr)
        {
            freeNode(curr);
        }

        void release(PLfs curr)
        {
            freeNode(curr);
        }

        void release(Node curr)
        {
            with (Node.Ix)
            {
                final switch (curr.typeIx)
                {
                case undefined: break;
                case ix_PLfs: return release(curr.as!(PLfs));
                case ix_All1: break;
                case ix_Br2Ptr: return release(curr.as!(Br2*));
                case ix_BrMPtr: return release(curr.as!(BrM*));
                case ix_LfMPtr: return release(curr.as!(LfM*));
                }
            }
        }
    }

    void freeNode(NodeType)(NodeType nt) @trusted
    {
        static if (isPointer!NodeType)
        {
            free(cast(void*)nt);  // TODO Allocator.free
        }
        debug --_nodeCount;
    }

    /** Ensure that root `Node` is allocated. */
    void ensureRootNode(U = DefaultRootType)()
    {
        if (!_root) { _root = construct!U; }
    }

    /// Returns: number of nodes used in `this` tree.
    pragma(inline) debug size_t nodeCount() @safe pure nothrow @nogc { return _nodeCount; }

    private Node _root;
    size_t _length = 0;

    debug size_t _nodeCount = 0;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/** Get chunkIx:th chunk of `radix` number of bits. */
static private Mod!(2^^radix) bitsChunk(size_t radix, BKey)(in BKey ukey, ChunkIx chunkIx) pure nothrow
{
    alias R = radix;
    enum M = 2^^R;     // branch-multiplicity, typically either 2, 4, 16 or 256
    enum chunkMask = M - 1;
    enum maxDepth = 8*BKey.sizeof / radix;

    // calculate bit shift to current chunk
    static if (isIntegral!BKey ||
               isSomeChar!BKey) // because top-most bit in ASCII coding (char) is often sparse (0 is much more common than 1)
    {
        /* most signficant bit chunk first because integers are
           typically more sparse in more significant bits */
        const shift = (maxDepth - 1 - chunkIx)*radix;
    }
    else
    {
        // default to most signficant bit chunk first
        const shift = (maxDepth - 1 - chunkIx)*radix;
    }

    return cast(typeof(return))((ukey >> shift) & chunkMask); // part of value which is also an index
}

/** Append statistics of tree under `Node` `sub.` into `stats`.
 */
static private void calculate(Key, Value, size_t radix)(RadixTree!(Key, Value, radix).Node sub,
                                                        ref RadixTree!(Key, Value, radix).Stats stats)
    @safe pure nothrow @nogc
    if (allSatisfy!(isTrieableKeyType, Key))
{
    alias RT = RadixTree!(Key, Value, radix);
    ++stats.popByNodeType[sub.typeIx];

    with (RT.Node.Ix)
    {
        final switch (sub.typeIx)
        {
        case undefined: break;
        case ix_PLfs: break; // TODO calculate()
        case ix_All1: break;
        case ix_Br2Ptr: sub.as!(RT.Br2*).calculate(stats); break;
        case ix_BrMPtr: sub.as!(RT.BrM*).calculate(stats); break;
        case ix_LfMPtr: sub.as!(RT.LfM*).calculate(stats); break;
        }
    }
}

/// Instantiator of set-version of `RadixTree`.
auto radixTreeSet(Key, size_t radix = 4)() { return RadixTree!(Key, void, radix)(); }

/// Instantiator of map-version of `RadixTree`.
auto radixTreeMap(Key, Value, size_t radix = 4)() { return RadixTree!(Key, Value, radix)(); }

/// Check correctness when radix is `radix` and for each `Key` in `Keys`.
auto check(size_t radix, Keys...)()
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
            auto set = radixTreeSet!(Key, radix);
            assert(set.empty);

            static assert(set.isSet);

            const n = 100_000;
            const useContains = false;
            foreach (Key k; 0.iota(n))
            {
                if (useContains)
                {
                    assert(!set.contains(k)); // key should not yet be in set
                    assert(k !in set);        // alternative syntax
                }

                assert(set.insert(k));  // insert new value returns `true` (previously not in set)
                assert(!set.insert(k)); // reinsert same value returns `false` (already in set)
                assert(!set.insert(k)); // reinsert same value returns `false` (already in set)

                if (useContains)
                {
                    assert(set.contains(k)); // key should now be in set
                    assert(k in set);        // alternative syntax
                    assert(!set.contains(k + 1)); // next key is not yet in set
                }
            }

            assert(set.length == n);

            auto map = radixTreeMap!(Key, Value, radix);
            static assert(map.isMap);

            map.insert(Key.init, Value.init);
        }
    }
}

/// Benchmark performance and memory usage when radix is `radix`.
void benchmark(size_t radix)()
{
    import core.thread : sleep;
    import std.range : iota;

    import std.algorithm : equal;
    struct TestValueType { int i; float f; string s; }
    alias Value = TestValueType;
    import std.meta : AliasSeq;
    foreach (Key; AliasSeq!(uint)) // just benchmark uint for now
    {
        auto set = radixTreeSet!(Key, radix);
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
                    assert(set.insert(k));
                else
                    set.insert(k);
            }

            dln("trie: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            auto stats = set.usageHistograms;
            dln("Sparse 2-Branch Population Histogram: ", stats.popHist_Br2);
            dln("Dense M=", 2^^radix, "-Branch Population Histogram: ", stats.popHist_BrM);
            dln("Dense M=", 2^^radix, "-Leaf   Population Histogram: ", stats.popHist_LfM);
            dln("Population By Node Type: ", stats.popByNodeType);

            size_t totalBytesUsed = 0;
            foreach (Set.Node.Ix ix, pop; stats.popByNodeType) // TODO use stats.byPair when added to typecons_ex.d
            {
                size_t bytesUsed = 0;
                with (Set.Node.Ix)
                {
                    final switch (ix)
                    {
                    case undefined: break;
                    case ix_PLfs:   bytesUsed = pop*Set.PLfs.sizeof; break;
                    case ix_All1:   bytesUsed = pop*Set.All1.sizeof; break;
                    case ix_Br2Ptr: bytesUsed = pop*Set.Br2.sizeof; break;
                    case ix_BrMPtr: bytesUsed = pop*Set.BrM.sizeof; break;
                    case ix_LfMPtr: bytesUsed = pop*Set.LfM.sizeof; break;
                    }
                }
                totalBytesUsed += bytesUsed;
                if (bytesUsed)
                {
                    dln(pop, " number of ", ix, " uses ", bytesUsed/1e6, " megabytes");
                }
            }
            dln("Tree uses ", totalBytesUsed/1e6, " megabytes");

            sleep(2);
            dln("Sleeping done");
        }

        {
            auto sw = StopWatch(AutoStart.yes);
            bool[int] aa;

            foreach (Key k; randomSamples) { aa[k] = true; }

            dln("D-AA: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            sleep(2);
            dln("Sleeping done");
        }

        dln();

        auto map = radixTreeMap!(Key, Value, radix);
        assert(map.empty);
        static assert(map.isMap);

        map.insert(Key.init, Value.init);
    }
}

@safe pure nothrow @nogc
unittest
{
    check!(4, double, ulong, dchar);
    check!(8, double, ulong, dchar);
}

version(benchmark) unittest
{
    benchmark!8;
    benchmark!4;
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
