/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type BrM*?

    TODO Use opIndex instead of subNode(): x.subNode(i) => (*x)[i]

    TODO Use `bijectToUnsigned` to support ordered storage of float and double

    TODO Add sparse 2^^n-branches for n < radix: 2^^1=B2, 2^^2=B4, 2^^3=B8, B^^4=B16. Use
    sortExactly from sortn.d to order their members.

    TODO Add traits `isSparseNodeType`, `isDenseNodeType` or PackingOf!(T) returns either dense, sparse

    TODO Make SBr02, SBr04, SBr16, SBr256 templated on N when I figure out how
    to elide the recursive template-instantiation. Ask forums

    TODO Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`
    TODO Provide RandomAccess `opIndex` and `opSlice`! for variable-length keys aswell?
    TODO Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to _root. Add checks with `isSorted`.

    TODO Name members so they indicate that range is sorted (`isSorted`).
    TODO Name members to indicate complexity

    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse

    TODO Should opBinaryRight return void* instead of bool for set-case?
 */
module trie;

import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy, isPointer;
import std.range : isInputRange, ElementType;

import bijections;
import variant_ex : WordVariant;
import typecons_ex : IndexedArray, IndexedBy;

version = benchmark;

import dbg;

enum isFixedTrieableKeyType(T) = isScalarType!T;

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
    import bitop_ex : UnsignedOfSameSizeAs;

    static assert(radix == 4 ||
                  radix == 8 ||
                  radix == 16 ||
                  radix == 24, "Radix is currently limited to either 4, 8, 16, or 24");
    static assert(radix <= 8*Key.sizeof, "Radix must be less than or equal to Key bit-precision"); // TODO Use strictly less than: radix < ... instead?

    enum isSet = is(Value == void); // `true` if this tree is a set
    enum isMap = !isSet;        // `true` if this tree is a map

    enum M = 2^^R;     // branch-multiplicity, typically either 2, 4, 16 or 256
    enum chunkMask = M - 1;

    alias order = M;   // tree order
    alias R = radix;

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
    import modulo : Mod;
    alias IxM = Mod!M; // restricted index type avoids range checking in array indexing below
    alias ChunkIx = uint;

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
        alias DefaultRootType = SBr02*;
    }
    else static if (radix == 8)
    {
        alias DefaultRootType = BrM*;
    }
    alias DefaultBranchType = DefaultRootType;
    alias DefaultLeafType = PLfs; // TODO use either LfM* or PLfs instead

    static assert(PLfs.sizeof == size_t.sizeof); // assert that it's size matches platform word-size

    /** Indicate that all leaves in this branch are set (denseness compression) */
    struct All1 {}

    /** Node types. */
    alias NodeTypes = AliasSeq!(PLfs,   // directly packed leaves
                                All1, // hinter
                                SBr02*, SBr04*, SBr16*, // sparse branching nodes
                                BrM*,                   // dense branching nodes
                                LfM*);                  // dense leaves-nodes

    enum showSizes = false;
    static if (showSizes)
    {
        static if (isSet)
        {
            pragma(msg, "Set SBr02.sizeof: ", SBr02.sizeof);
            pragma(msg, "Set BrM.sizeof: ", BrM.sizeof);
            pragma(msg, "Set LfM.sizeof: ", LfM.sizeof);
            pragma(msg, "Set PLfs.sizeof: ", PLfs.sizeof);
        }
        else
        {
            pragma(msg, "Map SBr02.sizeof: ", SBr02.sizeof);
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

    /** M-Branch population histogram.
        Index maps to population with value range (1 .. M).
    */
    alias BrM_PopHist = size_t[M];

    /** 2-Branch population histogram.
        Index maps to population with value range (1 .. 2).
    */
    alias SBr02_PopHist = size_t[2];

    /** 4-Branch population histogram.
        Index maps to population with value range (1 .. 4).
    */
    alias SBr04_PopHist = size_t[4];

    /** 16-Branch population histogram.
        Index maps to population with value range (1 .. 16).
    */
    alias SBr16_PopHist = size_t[16];

    /** 256-Branch population histogram.
        Index maps to population with value range (1 .. 256).
    */
    alias SBr256_PopHist = size_t[256];

    /** M-Leaf population histogram.
        Index maps to population with value range (1 .. M).
    */
    alias LeafM_PopHist = size_t[M];

    /** Tree Population and Memory-Usage Statistics. */
    struct Stats
    {
        SBr02_PopHist popHist_SBr02;
        SBr04_PopHist popHist_SBr04;
        SBr16_PopHist popHist_SBr16;
        SBr256_PopHist popHist_SBr256;

        BrM_PopHist popHist_BrM;
        LeafM_PopHist popHist_LfM;

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));
    }

    /** Non-bottom branch node containing densly packed array of `M` number of
        pointers to sub-`BrM`s or `Leaf`s.
    */
    static private struct BrM
    {
        IndexedBy!(Node[M]) subNodes;

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
            static if (isMap)
            {
                Value value;
            }
        }
    }

    // TODO templatize on `N` (currently 2)
    static private struct SBr02
    {
        enum N = 2;
        // TODO merge these into a new `NodeType`
        IndexedBy!(Node[N]) subNodes;
        IndexedBy!(IxM[N]) subChunks; // sub-ixMs. NOTE wastes space because IxM[N] only requires two bytes. Use IxM!2 instead.

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.popHist_SBr02[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    // TODO templatize on `N` (currently 4)
    static private struct SBr04
    {
        enum N = 4;
        // TODO merge these into a new `NodeType`
        IndexedBy!(Node[N]) subNodes; // TODO used typecons_ex.IndexedArray
        IndexedBy!(IxM[N]) subChunks; // sub-ixMs. NOTE wastes space because IxM[N] only requires two bytes. Use IxM!4 instead.

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.popHist_SBr04[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    // TODO templatize on `N` (currently 16)
    static private struct SBr16
    {
        enum N = 16;
        // TODO merge these into a new `NodeType`
        IndexedBy!(Node[N]) subNodes; // TODO used typecons_ex.IndexedArray
        IndexedBy!(IxM[N]) subChunks; // sub-ixMs. NOTE wastes space because IxM[N] only requires two bytes. Use IxM!16 instead.

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.popHist_SBr16[nnzSubCount - 1]; // TODO type-safe indexing
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
            {
                BitSet!M values; // memory-efficient storage of `bool` values
            }
            else
            {
                Value[M] values;
            }
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

    /** Get chunkIx:th chunk of `radix` number of bits. */
    IxM bitsChunk(ChunkIx chunkIx)(in Key key) const @trusted pure nothrow
    {
        // calculate bit shift to current chunk
        static if (isIntegral!Key ||
                   isSomeChar!Key) // because top-most bit in ASCII coding (char) is often sparse (0 is much more common than 1)
        {
            /* most signficant bit chunk first because integers are
               typically more sparse in more significant bits */
            enum shift = (maxDepth - 1 - chunkIx)*R;
        }
        else
        {
            // default to most signficant bit chunk first
            enum shift = (maxDepth - 1 - chunkIx)*R;
            // enum shift = chunkIx*R; // least significant bit first
        }

        const u = *(cast(UnsignedOfSameSizeAs!Key*)(&key)); // TODO functionize and reuse here and in intsort.d
        const IxM chunk = (u >> shift) & chunkMask; // part of value which is also an index
        return chunk;
    }

    /** Get chunkIx:th chunk of `radix` number of bits. */
    IxM bitsChunk()(in Key key, ChunkIx chunkIx) const @trusted pure nothrow
    {
        // calculate bit shift to current chunk
        static if (isIntegral!Key ||
                   isSomeChar!Key) // because top-most bit in ASCII coding (char) is often sparse (0 is much more common than 1)
        {
            /* most signficant bit chunk first because integers are
               typically more sparse in more significant bits */
            const shift = (maxDepth - 1 - chunkIx)*R;
        }
        else
        {
            // default to most signficant bit chunk first
            const shift = (maxDepth - 1 - chunkIx)*R;
            // enum shift = chunkIx*R; // least significant bit first
        }

        const u = *(cast(UnsignedOfSameSizeAs!Key*)(&key)); // TODO functionize and reuse here and in intsort.d
        const IxM chunk = (u >> shift) & chunkMask; // part of value which is also an index
        return chunk;
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
            _root = insert(_root, key, 0, wasAdded);
            _length += wasAdded;
            return wasAdded;
        }

        pragma(inline) Node insert(Node curr, in Key key, ChunkIx chunkIx, out bool wasAdded) // Node-polymorphic
        {
            with (Node.Ix)
            {
                final switch (curr.typeIx)
                {
                case undefined: break;
                case ix_PLfs:     return insert(curr.as!(PLfs),   key, chunkIx, wasAdded);
                case ix_SBr02Ptr: return insert(curr.as!(SBr02*), key, chunkIx, wasAdded);
                case ix_SBr04Ptr: return insert(curr.as!(SBr04*), key, chunkIx, wasAdded);
                case ix_SBr16Ptr: return insert(curr.as!(SBr16*), key, chunkIx, wasAdded);
                case ix_BrMPtr:   return insert(curr.as!(BrM*),   key, chunkIx, wasAdded);
                case ix_LfMPtr:   return insert(curr.as!(LfM*),   key, chunkIx, wasAdded);
                case ix_All1:     auto curr_ = curr.as!All1; break;
                }
                assert(false);
            }
        }

        Node insert(SBr02* curr, in Key key, ChunkIx chunkIx, out bool wasAdded)
        {
            const IxM chunk = bitsChunk(key, chunkIx);

            enum N = 2;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                if (curr.subNodes[subIx])   // first is occupied
                {
                    if (curr.subChunks[subIx] == chunk) // and matches chunk
                    {
                        curr.subNodes[subIx] = insert(curr.subNodes[subIx], key, chunkIx + 1, wasAdded);
                        return Node(curr);
                    }
                }
                else            // use first free sub
                {
                    curr.subNodes[subIx] = insert(constructSub(chunkIx + 1), key, chunkIx + 1, wasAdded); // use it
                    curr.subChunks[subIx] = chunk;
                    return Node(curr);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            return insert(expand(curr), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
        }

        Node insert(SBr04* curr, in Key key, ChunkIx chunkIx, out bool wasAdded)
        {
            const IxM chunk = bitsChunk(key, chunkIx);

            enum N = 4;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                if (curr.subNodes[subIx])   // first is occupied
                {
                    if (curr.subChunks[subIx] == chunk) // and matches chunk
                    {
                        curr.subNodes[subIx] = insert(curr.subNodes[subIx], key, chunkIx + 1, wasAdded);
                        return Node(curr);
                    }
                }
                else            // use first free sub
                {
                    curr.subNodes[subIx] = insert(constructSub(chunkIx + 1), key, chunkIx + 1, wasAdded); // use it
                    curr.subChunks[subIx] = chunk;
                    return Node(curr);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            return insert(expand(curr), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
        }

        Node insert(SBr16* curr, in Key key, ChunkIx chunkIx, out bool wasAdded)
        {
            const IxM chunk = bitsChunk(key, chunkIx);

            enum N = 16;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                if (curr.subNodes[subIx])   // first is occupied
                {
                    if (curr.subChunks[subIx] == chunk) // and matches chunk
                    {
                        curr.subNodes[subIx] = insert(curr.subNodes[subIx], key, chunkIx + 1, wasAdded);
                        return Node(curr);
                    }
                }
                else            // use first free sub
                {
                    curr.subNodes[subIx] = insert(constructSub(chunkIx + 1), key, chunkIx + 1, wasAdded); // use it
                    curr.subChunks[subIx] = chunk;
                    return Node(curr);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            return insert(expand(curr), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
        }

        Node insert(BrM* curr, in Key key, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth) assert(chunkIx + 1 < maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {
            const IxM chunk = bitsChunk(key, chunkIx);
            if (!curr.subNodes[chunk]) // if not yet set
            {
                curr.subNodes[chunk] = constructSub(chunkIx + 1);
            }
            curr.subNodes[chunk] = insert(curr.subNodes[chunk], key, chunkIx + 1, wasAdded);
            return Node(curr);
        }

        Node insert(LfM* curr, in Key key, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth) assert(chunkIx + 1 == maxDepth);
            else                      assert(chunkIx + 1 <= maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {
            const IxM chunk = bitsChunk(key, chunkIx);
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

        Node insert(PLfs curr, in Key key, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth) assert(chunkIx + 1 == maxDepth);
            else                      assert(chunkIx + 1 <= maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {
            const IxM chunk = bitsChunk(key, chunkIx);

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
                return insert(expand(curr), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
            }
        }

        Node constructSub(ChunkIx chunkIx)
        {
            return (chunkIx + 1 == maxDepth ? // is last
                    Node(construct!DefaultLeafType) :
                    Node(construct!DefaultBranchType));
        }

        /** Destructively expand `curr` into a `BrM` and return it. */
        BrM* expand(SBr02* curr) @trusted
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

        /** Destructively expand `curr` into a `BrM` and return it. */
        BrM* expand(SBr04* curr) @trusted
        {
            enum N = 4;         // branch-order, number of possible sub-nodes
            auto next = construct!(typeof(return));
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                next.subNodes[curr.subChunks[subIx]] = curr.subNodes[subIx];
            }
            freeNode(curr);
            return next;
        }

        /** Destructively expand `popHist_SBr04` into a `BrM` and return it. */
        BrM* expand(SBr16* curr) @trusted
        {
            enum N = 16;         // branch-order, number of possible sub-nodes
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

        void release(SBr02* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(SBr04* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(SBr16* curr)
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
                case ix_PLfs:     return release(curr.as!(PLfs));
                case ix_All1: break;
                case ix_SBr02Ptr: return release(curr.as!(SBr02*));
                case ix_SBr04Ptr: return release(curr.as!(SBr04*));
                case ix_SBr16Ptr: return release(curr.as!(SBr16*));
                case ix_BrMPtr:   return release(curr.as!(BrM*));
                case ix_LfMPtr:   return release(curr.as!(LfM*));
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
        case ix_PLfs: break;
        case ix_All1: break;
        case ix_SBr02Ptr:  sub.as!(RT.SBr02*).calculate(stats); break;
        case ix_SBr04Ptr:  sub.as!(RT.SBr04*).calculate(stats); break;
        case ix_SBr16Ptr:  sub.as!(RT.SBr16*).calculate(stats); break;
        case ix_BrMPtr:    sub.as!(RT.BrM*).calculate(stats); break;
        case ix_LfMPtr:    sub.as!(RT.LfM*).calculate(stats); break;
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
    foreach (Key; AliasSeq!(uint))
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
            dln("2-Branch Population Histogram: ", stats.popHist_SBr02);
            dln("4-Branch Population Histogram: ", stats.popHist_SBr04);
            dln("16-Branch Population Histogram: ", stats.popHist_SBr16);
            dln("256-Branch Population Histogram: ", stats.popHist_SBr256);
            dln("M=", 2^^radix, "-Branch Population Histogram: ", stats.popHist_BrM);
            dln("M=", 2^^radix, "-Leaf   Population Histogram: ", stats.popHist_LfM);
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
                    case ix_PLfs:     bytesUsed = pop*Set.PLfs.sizeof; break;
                    case ix_All1:     bytesUsed = pop*Set.All1.sizeof; break;
                    case ix_SBr02Ptr: bytesUsed = pop*Set.SBr02.sizeof; break;
                    case ix_SBr04Ptr: bytesUsed = pop*Set.SBr04.sizeof; break;
                    case ix_SBr16Ptr: bytesUsed = pop*Set.SBr16.sizeof; break;
                    case ix_BrMPtr:   bytesUsed = pop*Set.BrM.sizeof; break;
                    case ix_LfMPtr:   bytesUsed = pop*Set.LfM.sizeof; break;
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
    check!(4, uint, ulong);
    check!(8, uint, ulong);
}

version(benchmark) unittest
{
    benchmark!8;                // should be faster
    benchmark!4;                // should be slower
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
