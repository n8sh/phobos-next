/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO Add extra CT-parameter being a tuple of `Packing`s that hints about
    branching statistic. In the case where `hasFixedDepth` is true these packing must add upp to 8*Key.sizeof.

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type BrM*?

    TODO Use opIndex instead of atSubNode(): x.atSubNode(i) => (*x)[i]

    TODO Need bijectToUnsigned in intsort to support ordered storage of float and double. Move it to bijections.

    TODO Add sparse 2^^n-branches for n < radix: 2^^1=B2, 2^^2=B4, 2^^3=B8, B^^4=B16. Use
    sortExactly from sortn.d to order their members.

    TODO Add traits isSparseNodeType, isDenseNodeType or PackingOf!(T) returns either dense, sparse

    TODO Make SBr02, SBr04, SBr16, SBr256 templated on N when I figure out how to elide the recursive template-instantiation

    TODO Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`
    TODO Provide RandomAccess `opIndex` and `opSlice`! for variable-length keys aswell?
    TODO Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to _root. Add checks with `isSorted`.

    TODO Name members so they indicate that range is sorted (`isSorted`).
    TODO Name members to indicate complexity

    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse

    TODO RadixTree: Assigning a void pointer to a class

    TODO Should opBinaryRight return void* instead of bool for set-case?

    TODO Replace
    - if (auto currBrM = curr.peek!BrM)
    - else if (const currLfM = curr.peek!LfM)
    with some compacter logic perhaps using mixins or pattern matching using
    switch-case on internally store type in `WordVariant`.
 */
module trie;

import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy;
import std.range : isInputRange, ElementType;

import bijections;
import variant_ex : WordVariant;

version = benchmark;

import dbg;

enum Packing { dense8bit,
               dense4bit,
               sparse12bit, }

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
    // TODO make these CT-params (requires putting branch definitions in same scope as `RadixTree`)
    alias DefaultRootNodeType = SBr02;
    alias DefaultSubBranchNodeType = SBr02;

    import std.algorithm : filter;
    import std.meta : AliasSeq, staticMap;
    import std.typecons : ConstOf;
    import bitop_ex : UnsignedOfSameSizeAs;

    static assert(radix == 4 ||
                  radix == 8 ||
                  radix == 16 ||
                  radix == 32, "Radix is currently limited to either 4, 8, 16, or 32");
    static assert(radix <= 8*Key.sizeof, "Radix must be less than or equal to Key bit-precision"); // TODO Use strictly less than: radix < ... instead?

    enum isSet = is(Value == void); // `true` if this tree is a set
    enum isMap = !isSet;        // `true` if this tree is a map

    enum M = 2^^R;     // branch-multiplicity, typically either 2, 4, 16 or 256
    enum chunkMask = M - 1;

    alias order = M;   // tree order
    alias R = radix;

    /// Tree depth.
    enum maxDepth = 8*Key.sizeof / R;

    /// `true` if tree has fixed a key of fixed length and in turn a tree of fixed max depth.
    enum hasFixedDepth = isFixedTrieableKeyType!Key;

    /// `true` if tree has binary branch.
    enum isBinary = R == 2;

    /** Radix Modulo Index */
    import modulo : Mod;
    alias IxM = Mod!M; // restricted index type avoids range checking in array indexing below
    alias ChunkIx = uint;

    /** `R` least significant bits (LSB) of leaves directly packed into a word.
        TODO Generalize to packing of more than one `IxM` per byte.
     */
    static      if (size_t.sizeof == 4)
    {
        static if (radix == 4) { struct PackedLfs { ubyte cnt; IxM[3] ixMs; } } // TODO pack 6 IxM
        static if (radix == 8) { struct PackedLfs { ubyte cnt; IxM[3] ixMs; } } // TODO handle radix != 8
        static if (isMap && is(Value == bool)) { /* TODO pack bit efficiently */ }
    }
    else static if (size_t.sizeof == 8)
    {
        static if (radix == 4) { struct PackedLfs { ubyte cnt; IxM[7] ixMs; } } // TODO pack 14 IxM
        static if (radix == 8) { struct PackedLfs { ubyte cnt; IxM[7] ixMs; } } // TODO handle radix != 8
        static if (isMap && is(Value == bool)) { /* TODO pack bit efficiently */ }
    }

    static assert(PackedLfs.sizeof == size_t.sizeof); // assert that it's size matches platform word-size

    /** Indicate that all leaves in this branch are set (denseness compression) */
    struct AllSet {}

    /** Node types. */
    alias NodeTypes = AliasSeq!(PackedLfs, // directly packed leaves
                                AllSet,    // hinter
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
        }
        else
        {
            pragma(msg, "Map SBr02.sizeof: ", SBr02.sizeof);
            pragma(msg, "Map BrM.sizeof: ", BrM.sizeof);
            pragma(msg, "Map LfM.sizeof: ", LfM.sizeof);
        }
    }

    /** Mutable node. */
    alias Node = WordVariant!NodeTypes;
    /** Constant node. */
    // alias ConstNodePtr = WordVariant!(staticMap!(ConstOf, NodeTypes));

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
    alias SBr0256_PopHist = size_t[256];

    /** M-Leaf population histogram.
        Index maps to population with value range (1 .. M).
    */
    alias LeafM_PopHist = size_t[M];

    /** Tree Statistics. */
    struct Stats
    {
        SBr02_PopHist sbr02;
        SBr04_PopHist sbr04;
        SBr16_PopHist sbr16;
        SBr0256_PopHist sbr256;
        BrM_PopHist brM;
        LeafM_PopHist lfM;
    }

    /** Non-bottom branch node containing densly packed array of `M` number of
        pointers to sub-`BrM`s or `Leaf`s.
    */
    static private struct BrM
    {
        Node[M] subNodes; // TODO used typecons_ex.indexedBy

        // Indexing with internal range check is safely avoided.
        // TODO move to modulo.d: opIndex(T[M], IxM i) or atSubNode(T[M], IxM i) if that doesn't work
        pragma(inline) auto ref atSubNode(IxM i) @trusted { return subNodes.ptr[i]; }
        pragma(inline) auto ref opIndex(IxM i) { return atSubNode(i); }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow @nogc const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.brM[nnzSubCount - 1]; // TODO type-safe indexing
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
        Node[N] subNodes; // TODO used typecons_ex.indexedBy
        IxM[N] subChunks; // sub-ixMs. NOTE wastes space because IxM[N] only requires two bytes. Use IxM!2 instead.

        // Indexing with internal range check is safely avoided.
        // TODO move to modulo.d: opIndex(T[M], IxM i) or atSubNode(T[M], IxM i) if that doesn't work
        pragma(inline) auto ref atSubNode(Mod!N i) @trusted { return subNodes.ptr[i]; }
        pragma(inline) auto ref atSubChunk(Mod!N i) @trusted { return subChunks.ptr[i]; }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.sbr02[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    // TODO templatize on `N` (currently 4)
    static private struct SBr04
    {
        enum N = 4;
        // TODO merge these into a new `NodeType`
        Node[N] subNodes; // TODO used typecons_ex.indexedBy
        IxM[N] subChunks; // sub-ixMs. NOTE wastes space because IxM[N] only requires two bytes. Use IxM!4 instead.

        // Indexing with internal range check is safely avoided.
        // TODO move to modulo.d: opIndex(T[M], IxM i) or atSubNode(T[M], IxM i) if that doesn't work
        pragma(inline) auto ref atSubNode(Mod!N i) @trusted { return subNodes.ptr[i]; }
        pragma(inline) auto ref atSubChunk(Mod!N i) @trusted { return subChunks.ptr[i]; }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.sbr04[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    // TODO templatize on `N` (currently 16)
    static private struct SBr16
    {
        enum N = 16;
        // TODO merge these into a new `NodeType`
        Node[N] subNodes; // TODO used typecons_ex.indexedBy
        IxM[N] subChunks; // sub-ixMs. NOTE wastes space because IxM[N] only requires two bytes. Use IxM!16 instead.

        // Indexing with internal range check is safely avoided.
        // TODO move to modulo.d: opIndex(T[M], IxM i) or atSubNode(T[M], IxM i) if that doesn't work
        pragma(inline) auto ref atSubNode(Mod!N i) @trusted { return subNodes.ptr[i]; }
        pragma(inline) auto ref atSubChunk(Mod!N i) @trusted { return subChunks.ptr[i]; }

        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure nothrow const
        {
            size_t nnzSubCount = 0; // number of non-zero sub-nodes
            foreach (sub; subNodes[].filter!(sub => sub))
            {
                ++nnzSubCount;
                sub.calculate!(Key, Value, radix)(stats);
            }
            ++stats.sbr16[nnzSubCount - 1]; // TODO type-safe indexing
        }
    }

    /** Bottom-most leaf node of `RadixTree`-set storing `M` number of densly packed
        keys of fixed-length type `Key`.
    */
    static private struct LfM
    {
        /** Append statistics of tree under `this` into `stats`. */
        void calculate(ref Stats stats) @safe pure const
        {
            ++stats.lfM[keyLSBits.countOnes - 1];
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
                case undefined:    break;
                case ix_PackedLfs: auto curr_ = curr.as!PackedLfs; break;
                case ix_AllSet:    auto curr_ = curr.as!AllSet; break;
                case ix_SBr02Ptr:  return insert(curr.as!(SBr02*), key, chunkIx, wasAdded);
                case ix_SBr04Ptr:  return insert(curr.as!(SBr04*), key, chunkIx, wasAdded);
                case ix_SBr16Ptr:  return insert(curr.as!(SBr16*), key, chunkIx, wasAdded);
                case ix_BrMPtr:    return insert(curr.as!(BrM*), key, chunkIx, wasAdded);
                case ix_LfMPtr:    return insert(curr.as!(LfM*), key, chunkIx, wasAdded);
                }
                assert(false);
            }
        }

        Node insert(SBr02* br, in Key key, ChunkIx chunkIx, out bool wasAdded)
        {
            const IxM chunk = bitsChunk(key, chunkIx);

            enum N = 2;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                if (br.subNodes[subIx])   // first is occupied
                {
                    if (br.subChunks[subIx] == chunk) // and matches chunk
                    {
                        br.subNodes[subIx] = insert(br.subNodes[subIx], key, chunkIx + 1, wasAdded);
                        return Node(br);
                    }
                }
                else            // use first free sub
                {
                    br.subNodes[subIx] = insert(constructSub(chunkIx + 1), key, chunkIx + 1, wasAdded); // use it
                    br.subChunks[subIx] = chunk;
                    return Node(br);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            return insert(expand(br), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
        }

        Node insert(SBr04* br, in Key key, ChunkIx chunkIx, out bool wasAdded)
        {
            const IxM chunk = bitsChunk(key, chunkIx);

            enum N = 4;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                if (br.subNodes[subIx])   // first is occupied
                {
                    if (br.subChunks[subIx] == chunk) // and matches chunk
                    {
                        br.subNodes[subIx] = insert(br.subNodes[subIx], key, chunkIx + 1, wasAdded);
                        return Node(br);
                    }
                }
                else            // use first free sub
                {
                    br.subNodes[subIx] = insert(constructSub(chunkIx + 1), key, chunkIx + 1, wasAdded); // use it
                    br.subChunks[subIx] = chunk;
                    return Node(br);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            return insert(expand(br), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
        }

        Node insert(SBr16* br, in Key key, ChunkIx chunkIx, out bool wasAdded)
        {
            const IxM chunk = bitsChunk(key, chunkIx);

            enum N = 16;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                if (br.subNodes[subIx])   // first is occupied
                {
                    if (br.subChunks[subIx] == chunk) // and matches chunk
                    {
                        br.subNodes[subIx] = insert(br.subNodes[subIx], key, chunkIx + 1, wasAdded);
                        return Node(br);
                    }
                }
                else            // use first free sub
                {
                    br.subNodes[subIx] = insert(constructSub(chunkIx + 1), key, chunkIx + 1, wasAdded); // use it
                    br.subChunks[subIx] = chunk;
                    return Node(br);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            return insert(expand(br), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
        }

        Node insert(BrM* brM, in Key key, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth)
                assert(chunkIx + 1 < maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {

            const IxM chunk = bitsChunk(key, chunkIx);
            if (!brM.atSubNode(chunk)) // if not yet set
            {
                brM.atSubNode(chunk) = constructSub(chunkIx + 1);
            }
            brM.atSubNode(chunk) = insert(brM.atSubNode(chunk), key, chunkIx + 1, wasAdded);

            return Node(brM);
        }

        Node insert(LfM* lfM, in Key key, ChunkIx chunkIx, out bool wasAdded)
        in
        {
            static if (hasFixedDepth)
                assert(chunkIx + 1 == maxDepth);
            else
                assert(chunkIx + 1 <= maxDepth);
            assert(!wasAdded);               // check that we haven't yet added it
        }
        body
        {

            const IxM chunk = bitsChunk(key, chunkIx);
            if (!lfM.keyLSBits[chunk])
            {
                lfM.keyLSBits[chunk] = true;
                wasAdded = true;
            }
            else
            {
                wasAdded = false;
            }

            return Node(lfM);
        }

        Node constructSub(ChunkIx chunkIx)
        {
            // dln("constructSub: chunkIx is ", chunkIx);
            return (chunkIx + 1 == maxDepth ? // is last
                    Node(construct!LfM) :
                    Node(construct!DefaultSubBranchNodeType));
        }

        /** Destructively expand `sbr02` into a `BrM` and return it. */
        BrM* expand(SBr02* sbr02) @trusted
        {
            enum N = 2;         // branch-order, number of possible sub-nodes
            BrM* brM = construct!BrM;
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                brM.atSubNode(sbr02.atSubChunk(subIx)) = sbr02.subNodes[subIx];
            }
            freeNode(sbr02);
            return brM;
        }

        /** Destructively expand `sbr04` into a `BrM` and return it. */
        BrM* expand(SBr04* sbr04) @trusted
        {
            enum N = 4;         // branch-order, number of possible sub-nodes
            BrM* brM = construct!BrM;
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                brM.atSubNode(sbr04.atSubChunk(subIx)) = sbr04.subNodes[subIx];
            }
            freeNode(sbr04);
            return brM;
        }

        /** Destructively expand `sbr04` into a `BrM` and return it. */
        BrM* expand(SBr16* sbr16) @trusted
        {
            enum N = 16;         // branch-order, number of possible sub-nodes
            BrM* brM = construct!BrM;
            foreach (Mod!N subIx; iota!(0, N)) // each sub node. TODO use iota!(Mod!N)
            {
                brM.atSubNode(sbr16.atSubChunk(subIx)) = sbr16.subNodes[subIx];
            }
            freeNode(sbr16);
            return brM;
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
    U* construct(U)() @trusted
    {
        import std.conv : emplace;
        U* node = emplace!U(cast(U*)malloc(U.sizeof));
        // TODO ensure alignment of node at least that of U.alignof
        debug ++_nodeCount;
        return node;
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

        void release(Node curr)
        {
            // TODO use switch
            if      (auto subSBr02 = curr.peek!(SBr02*)) { release(*subSBr02); }
            else if (auto subSBr04 = curr.peek!(SBr04*)) { release(*subSBr04); }
            else if (auto subSBr16 = curr.peek!(SBr16*)) { release(*subSBr16); }
            else if (auto subBrM   = curr.peek!(BrM*))   { release(*subBrM); }
            else if (auto subLfM   = curr.peek!(LfM*))   { release(*subLfM); }
            else if (curr)                          { assert(false, "Unknown type of non-null pointer"); }
        }
    }

    void freeNode(NodeType)(NodeType* nt) @trusted
    {
        debug --_nodeCount;
        free(cast(void*)nt);  // TODO Allocator.free
    }

    /** Ensure that root `Node` is allocated. */
    void ensureRootNode(U = DefaultRootNodeType)()
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

/** Append statistics of tree under `Node` `sub.` into `stats`. */
static private void calculate(Key, Value, size_t radix)(RadixTree!(Key, Value, radix).Node sub,
                                                        ref RadixTree!(Key, Value, radix).Stats stats)
    @safe pure nothrow @nogc
    if (allSatisfy!(isTrieableKeyType, Key))
{
    alias RT = RadixTree!(Key, Value, radix);
    import std.algorithm : filter;
    // TODO use switch
    if      (const subSBr02 = sub.peek!(RT.SBr02*)) { (*subSBr02).calculate(stats); }
    else if (const subSBr04 = sub.peek!(RT.SBr04*)) { (*subSBr04).calculate(stats); }
    else if (const subSBr16 = sub.peek!(RT.SBr16*)) { (*subSBr16).calculate(stats); }
    else if (const subBrM   = sub.peek!(RT.BrM*))   { (*subBrM)  .calculate(stats); }
    else if (const subLfM   = sub.peek!(RT.LfM*))   { (*subLfM)  .calculate(stats); }
    else if (sub)
    {
        assert(false, "Unknown type of non-null pointer");
    }
}

/// Instantiator of set-version of `RadixTree`.
auto radixTreeSet(Key, size_t radix = 4)() { return RadixTree!(Key, void, radix)(); }

/// Instantiator of map-version of `RadixTree`.
auto radixTreeMap(Key, Value, size_t radix = 4)() { return RadixTree!(Key, Value, radix)(); }

/// Check correctness when radix is `radix`.
auto check(size_t radix)()
{
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
        import std.algorithm : equal;
        struct TestValueType { int i; float f; string s; }
        alias Value = TestValueType;
        import std.meta : AliasSeq;
        foreach (Key; AliasSeq!(uint))
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

                // dln("#################################### insert(k), k is ", k);
                assert(set.insert(k));  // insert new value returns `true` (previously not in set)
                // dln("#################################### insert(k), k is ", k);
                assert(!set.insert(k)); // reinsert same value returns `false` (already in set)
                // dln("#################################### insert(k), k is ", k);
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
            auto uhists = set.usageHistograms;
            dln("2-Branch Population Histogram: ", uhists.sbr02);
            dln("4-Branch Population Histogram: ", uhists.sbr04);
            dln("16-Branch Population Histogram: ", uhists.sbr16);
            dln("256-Branch Population Histogram: ", uhists.sbr256);
            dln("M=", 2^^radix, "-Branch Population Histogram: ", uhists.brM);
            dln("M=", 2^^radix, "-Leaf   Population Histogram: ", uhists.lfM);
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

version(benchmark) unittest
{
    benchmark!8;
    benchmark!4;
}

@safe pure nothrow @nogc
unittest
{
    check!4;
    check!8;
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
