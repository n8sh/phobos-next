/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type BrM*?

    TODO Make oneSet typed VariantPointer

    TODO Use opIndex instead of at(): x.at(i) => (*x)[i]

    TODO Add support for map insert

    TODO Need bijectToUnsigned in intsort to support ordered storage of float and double. Move it to bijections.

    TODO Add sparse 2^^n-branches for n < radix: 2^^1=B2, 2^^2=B4, 2^^3=B8, B^^4=B16. Use
    sortExactly from sortn.d to order their members.

    TODO Add traits isSparseNodeType, isDenseNodeType or PackingOf!(T) returns either dense, sparse

    TODO Make Br2, Br4 templated on N when I figure out how to elide the recursive template-instantiation

    TODO Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`
    TODO Provide RandomAccess `opIndex` and `opSlice`! for variable-length keys aswell?
    TODO Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to _root. Add checks with `isSorted`.

    TODO Name members so they indicate that range isSorted.
    TODO Name members to indicate complexity

    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse

    TODO RadixTree: Assigning a void pointer to a class

    TODO Should opBinaryRight return void* instead of bool for set-case?

    TODO Replace
    - if (auto currBrM = curr.peek!BrM)
    - else if (const currLfM = curr.peek!LfM)
    with some compacter logic perhaps using mixins or pattern matching using
    switch-case on internally store type in `VariantPointer`.

    I've just started working on a RadixTree implementation in D here:

    https://github.com/nordlow/phobos-next/blob/master/src/trie.d

    The current solution is very generic as it CT-specializes to either a
    `RadixTreeMap` and `RadixTreeSet`. These provide memory-efficient
    replacements for builtin AAs.

    Each radix-tree branching contains 2^^radix pointers to the next
    sub-branching. In the set-case I want these pointers to also be able to
    contain special "codes" with the following meanings

    - 0x1: *only* key associated with sub-branching is set.

    0xFFFFFFFFFFFFFFFF: all values in this sub-branch are set.
 */
module trie;

import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy;
import std.range : isInputRange, ElementType;

import bijections;
import variant_pointer : VariantPointer;

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

/** Radix Tree storing keys of type `Key`.

    In set-case (Value is Void) this containers is especially suitable for
    representing a set of 32 or 64 integers/pointers.

    See also: https://en.wikipedia.org/wiki/Radix_tree
 */
struct RadixTree(Key,
                 Value,
                 size_t radix = 4) // radix in number of bits, typically either 1, 2, 4 or 8
    if (allSatisfy!(isTrieableKeyType, Key))
{
    import std.meta : AliasSeq, staticMap;
    import std.typecons : ConstOf;

    static assert(radix == 4 ||
                  radix == 8 ||
                  radix == 16 ||
                  radix == 32, "Radix is currently limited to either 4, 8, 16, or 32");
    static assert(radix <= 8*Key.sizeof, "Radix must be less than or equal to Key bit-precision"); // TODO Use strictly less than: radix < ... instead?

    import bitop_ex : UnsignedOfSameSizeAs;

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

    /** Node types. */
    alias NodeTypes = AliasSeq!(Br2, BrM, LfM);

    enum showSizes = false;
    static if (showSizes)
    {
        static if (isSet)
        {
            pragma(msg, "Set Br2.sizeof: ", Br2.sizeof);
            pragma(msg, "Set BrM.sizeof: ", BrM.sizeof);
            pragma(msg, "Set LfM.sizeof: ", LfM.sizeof);
        }
        else
        {
            pragma(msg, "Map Br2.sizeof: ", Br2.sizeof);
            pragma(msg, "Map BrM.sizeof: ", BrM.sizeof);
            pragma(msg, "Map LfM.sizeof: ", LfM.sizeof);
        }
    }

    /** Mutable node. */
    alias Node = VariantPointer!(NodeTypes);
    /** Constant node. */
    alias ConstNodePtr = VariantPointer!(staticMap!(ConstOf, NodeTypes));

    /** Radix Modulo Index */
    import modulo : Mod;
    alias IxM = Mod!M; // restricted index type avoids range checking in array indexing below
    alias ChunkIx = uint;

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

    /** M-Branch occupation histogram.
        Index maps to occupation with value range (1 .. M).
    */
    alias BranchMOccupationHistogram = size_t[M];

    /** 2-Branch occupation histogram.
        Index maps to occupation with value range (1 .. 2).
    */
    alias Branch2OccupationHistogram = size_t[2];

    /** M-Leaf occupation histogram.
        Index maps to occupation with value range (1 .. M).
    */
    alias LeafMOccupationHistogram = size_t[M];

    /** Tree Statistics. */
    struct Stats
    {
        Branch2OccupationHistogram br2;
        BranchMOccupationHistogram brM;
        LeafMOccupationHistogram lfM;
    }

    /** Non-bottom branch node containing densly packed array of `M` number of
        pointers to sub-`BrM`s or `Leaf`s.
    */
    static private struct BrM
    {
        static immutable oneSet = cast(typeof(this)*)1UL; /// indicates that only next/child at this index is occupied

        static if (hasFixedDepth)
        {
            /** Indicates that all children of `this` branch are occupied. Only
                for fixed-sized `Keys`. */
            static immutable allSet = cast(typeof(this)*)size_t.max;
        }

        Node[M] subs;        // sub-branches

        // Indexing with internal range check is safely avoided.
        // TODO move to modulo.d: opIndex(T[M], IxM i) or at(T[M], IxM i) if that doesn't work
        pragma(inline) auto ref at     (IxM i) @trusted { return subs.ptr[i]; }
        pragma(inline) auto ref opIndex(IxM i) { return at(i); }

        /** Returns: depth of tree at this branch. */
        // size_t linearDepth() @safe pure nothrow const
        // {
        //     // TODO replace with fold when switching to 2.071
        //     import std.algorithm : map, reduce, max;
        //     return reduce!max(0UL, subs[].map!(sub => (sub.peek!(typeof(this)) == oneSet ? 1UL :
        //                                                  sub ? 1 + sub.linearDepth :
        //                                                  0UL)));
        // }

        void calculate(ref Stats hists) @safe pure nothrow const
        {
            import std.algorithm : count, filter;
            size_t nzcnt = 0; // number of non-zero branches

            // TODO functionize
            foreach (sub; subs[].filter!(sub => sub))
            {
                ++nzcnt;
                if (const subBr2 = sub.peek!Br2)
                {
                    if (subBr2 != Br2.oneSet) { subBr2.calculate(hists); } // TODO verify correct depth
                }
                else if (const subBrM = sub.peek!BrM)
                {
                    if (subBrM != BrM.oneSet) { subBrM.calculate(hists); } // TODO verify correct depth
                }
                else if (const subLfM = sub.peek!LfM)
                {
                    subLfM.calculate(hists);
                }
                else if (sub)
                {
                    assert(false, "Unknown type of non-null pointer");
                }
            }

            ++hists.brM[nzcnt - 1]; // TODO type-safe indexing
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
    static private struct Br2
    {
        enum N = 2;

        static immutable oneSet = cast(typeof(this)*)1UL; /// indicates that only next/child at this index is occupied

        // TODO merge these into a new `NodeType`
        Node[N] subs;        // sub-branches
        IxM[N] subKeyChunks; // sub-ixMs. NOTE wastes space because IxM[N] only requires two bytes. Use IxM2 instead.

        // Indexing with internal range check is safely avoided.
        // TODO move to modulo.d: opIndex(T[M], IxM i) or at(T[M], IxM i) if that doesn't work
        pragma(inline) auto ref at     (Mod!N i) @trusted { return subs.ptr[i]; }
        pragma(inline) auto ref atSubKeyChunk(Mod!N i) @trusted { return subKeyChunks.ptr[i]; }

        void calculate(ref Stats hists) @safe pure nothrow const
        {
            import std.algorithm : count, filter;
            size_t nzcnt = 0; // number of non-zero branches

            // TODO functionize
            foreach (sub; subs[].filter!(sub => sub))
            {
                ++nzcnt;
                if (const subBr2 = sub.peek!Br2)
                {
                    if (subBr2 != Br2.oneSet) { subBr2.calculate(hists); } // TODO verify correct depth
                }
                else if (const subBrM = sub.peek!BrM)
                {
                    if (subBrM != BrM.oneSet) { subBrM.calculate(hists); } // TODO verify correct depth
                }
                else if (const subLfM = sub.peek!LfM)
                {
                    subLfM.calculate(hists);
                }
                else if (sub)
                {
                    assert(false, "Unknown type of non-null pointer");
                }
            }

            ++hists.br2[nzcnt - 1]; // TODO type-safe indexing
        }
    }

    /** Bottom-most leaf node of `RadixTree`-set storing `M` number of densly packed
        keys of fixed-length type `Key`.
    */
    static private struct LfM
    {
        void calculate(ref Stats hists) @safe pure const
        {
            ++hists.lfM[keyLSBits.countOnes - 1];
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

    size_t calculateSubs(Subs)(Subs subs, ref Stats hists) @safe pure nothrow const
        if (isInputRange!Subs)
    {
        size_t nzcnt = 0; // number of non-zero branches
        foreach (Node sub; subs)
        {
            ++nzcnt;
            if (const subBr2 = sub.peek!Br2)
            {
                if (subBr2 != Br2.oneSet) { subBr2.calculate(this, hists); } // TODO verify correct depth
            }
            else if (const subBrM = sub.peek!BrM)
            {
                if (subBrM != BrM.oneSet) { subBrM.calculate(this, hists); } // TODO verify correct depth
            }
            else if (const subLfM = sub.peek!LfM)
            {
                subLfM.calculate(this, hists);
            }
            else if (sub)
            {
                assert(false, "Unknown type of non-null pointer");
            }
        }
        return nzcnt;
    }

    /// Get depth of tree.
    // size_t depth() @safe pure nothrow const
    // {
    //     return _root !is null ? _root.linearDepth : 0;
    // }

    Stats usageHistograms() const
    {
        typeof(return) hists;

        // TODO reuse rangeinterface when made available
        if (const rootBr2 = _root.peek!Br2)
        {
            // if (rootBr2 != Br2.oneSet) { rootBr2.calculate(hists[0], hists[1]); }
        }
        else if (const rootBrM = _root.peek!BrM)
        {
            if (rootBrM != BrM.oneSet) { rootBrM.calculate(hists); }
        }
        else if (const rooLfM = _root.peek!LfM)
        {
            assert(false, "TODO");
        }
        else if (_root)
        {
            assert(false, "Unknown type of non-null pointer");
        }
        return hists;
    }

    this(this)
    {
        if (!_root.ptr) return;
        auto oldRootPtr = _root;
        ensureRootNode;
        // TODO insert(oldRootPtr[]);
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

    /** Insert `key`.

        Returns: a non-null (defined) `KeyFindResult` if key was previously
        already inserted, `KeyFindResult.init` otherwise. This return value can then be
        used for map's `insert(Key key, Value value)` so a single implementation
        for `insert(Key key)` can be used for both set and map variant.
    */
    KeyFindResult insertOld(in Key key)
    {
        ensureRootNode;

        Node curr = _root;
        foreach (const ix; iota!(0, maxDepth)) // NOTE unrolled/inlined compile-time-foreach chunk index
        {
            const IxM chunk = bitsChunk!ix(key);

            static if (ix + 2 == maxDepth) // if this is the second last chunk
            {
                if (auto currBrM = curr.peek!BrM)
                {
                    // assure that we have prepared `LfM` at next depth (sub-node)
                    if (!currBrM.at(chunk)) // if not yet set
                    {
                        currBrM.at(chunk) = construct!LfM;
                    }
                    else
                    {
                        assert(currBrM.at(chunk).peek!LfM);
                    }
                    curr = currBrM.at(chunk); // and go there
                }
                else if (auto currLfM = curr.peek!LfM) { assert(false, "TODO"); }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
            else static if (ix + 1 == maxDepth) // if this is the last chunk
            {
                if (auto currBrM = curr.peek!BrM) // branch-M
                {
                    assert(currBrM != BrM.oneSet);
                    if (!currBrM.at(chunk))
                    {
                        currBrM.at(chunk) = BrM.oneSet; // tag this as last
                        return KeyFindResult(Node(currBrM), chunk, true); // a new value was inserted
                    }
                    else
                    {
                        static if (isFixedTrieableKeyType!Key)
                        {
                            assert(currBrM.at(chunk).peek!BrM == BrM.oneSet);
                            return KeyFindResult(Node(currBrM), chunk, false); // value already set
                        }
                        else
                        {
                            assert(false, "TODO");
                            // TODO test this
                            if (curr.isOccupied)
                            {
                                return It.init;
                            }
                            else
                            {
                                curr.isOccupied = false;
                                return It(Node(currBrM), chunk);
                            }
                        }
                    }
                }
                else if (auto currLfM = curr.peek!LfM) // leaf-M
                {
                    if (!currLfM.keyLSBits[chunk])
                    {
                        currLfM.keyLSBits[chunk] = true;
                        return KeyFindResult(Node(currLfM), chunk, true);
                    }
                    else
                    {
                        return KeyFindResult(Node(currLfM), chunk, false);
                    }

                    assert(false, "TODO");
                }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
            else            // other
            {
                if (auto currBrM = curr.peek!BrM)
                {
                    assert(currBrM != BrM.oneSet);
                    if (!currBrM.at(chunk)) // if branch not yet visited
                    {
                        currBrM.at(chunk) = construct!BrM; // create it
                    }
                    curr = currBrM.at(chunk); // and visit it
                }
                else if (auto currLfM = curr.peek!LfM) { assert(false, "TODO"); }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
        }
        assert(false, "End of function shouldn't be reached");
    }

    @safe pure nothrow // TODO @nogc
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
            // if (key == 544) dln("Node.insert: chunkIx is ", chunkIx);
            if      (auto currBr2 = curr.peek!Br2) { return insert(currBr2, key, chunkIx, wasAdded); }
            else if (auto currBrM = curr.peek!BrM) { return insert(currBrM, key, chunkIx, wasAdded); }
            else if (auto currLfM = curr.peek!LfM) { return insert(currLfM, key, chunkIx, wasAdded); }
            else                                   { assert(false, "Unknown Node type"); }
        }

        Node insert(Br2* br2, in Key key, ChunkIx chunkIx, out bool wasAdded)
        {
            // if (key == 544) dln("Br2.insert: chunkIx is ", chunkIx);
            const IxM chunk = bitsChunk(key, chunkIx);

            enum N = 2;         // branch-order, number of possible sub-nodes
            foreach (Mod!N subIx; iota!(0, N)) // each sub node
            {
                // if (key == 544) dln("subIx is ", subIx);
                if (br2.subs[subIx])   // first is occupied
                {
                    if (br2.subKeyChunks[subIx] == chunk) // and matches chunk
                    {
                        br2.subs[subIx] = insert(br2.subs[subIx], key, chunkIx + 1, wasAdded);
                        return Node(br2);
                    }
                }
                else            // use first free sub
                {
                    br2.subs[subIx] = insert(constructSub(chunkIx + 1),
                                             key, chunkIx + 1, wasAdded); // use it
                    br2.subKeyChunks[subIx] = chunk;
                    return Node(br2);
                }
            }

            // if we got here all N sub-nodes are occupied so we need to expand
            // if (key == 544) dln("Br2.insert: Calling destructivelyExpand when chunkIx is ", chunkIx);
            return insert(destructivelyExpand(br2), key, chunkIx, wasAdded); // NOTE stay at same chunkIx (depth)
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
            // if (key == 544) dln("BrM.insert: chunkIx is ", chunkIx);

            const IxM chunk = bitsChunk(key, chunkIx);
            if (!brM.at(chunk)) // if not yet set
            {
                brM.at(chunk) = constructSub(chunkIx + 1);
            }
            brM.at(chunk) = insert(brM.at(chunk), key, chunkIx + 1, wasAdded);

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
            // if (key == 544) dln("LfM.insert: chunkIx is ", chunkIx);

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
                    Node(construct!Br2));
        }

        /** Destructively expand `br2` into a `BrM` and return it. */
        BrM* destructivelyExpand(Br2* br2) @trusted
        {
            enum N = 2;         // branch-order, number of possible sub-nodes
            BrM* brM = construct!BrM;
            foreach (Mod!N subIx; iota!(0, N)) // each sub node
            {
                brM.at(br2.atSubKeyChunk(subIx)) = br2.subs[subIx];
            }
            freeNode(br2);
            return brM;
        }
    }

    static if (isMap)
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        bool insert(in Key key, Value value)
        {
            bool result = insert(key);
            // TODO call insertAt(result, value);
            return result;
        }
    }

    static if (isSet)
    {

        /** Returns: `true` if key is contained in set, `false` otherwise. */
        bool contains(in Key key) const nothrow
        {
            if (!_root) { return false; }

            Node curr = _root;
            foreach (const ix; iota!(0, maxDepth)) // NOTE unrolled/inlined compile-time-foreach chunk index
            {
                const IxM chunk = bitsChunk!ix(key);
                if (auto currBrM = curr.peek!BrM)
                {
                    assert(currBrM != BrM.oneSet);
                    if (!currBrM.at(chunk))
                    {
                        return false;
                    }
                    else
                    {
                        static if (isFixedTrieableKeyType!Key)
                        {
                            if (currBrM.at(chunk).peek!BrM == BrM.oneSet) { return true; }
                        }
                        else
                        {
                            if (currBrM.isOccupied)
                            {
                                return false;
                            }
                            else
                            {
                                currBrM.isOccupied = false;
                                return true;
                            }
                        }
                        curr = currBrM.at(chunk);
                    }
                }
                else if (const currLfM = curr.peek!LfM)
                {
                    return currLfM.keyLSBits[chunk];
                }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
            return false;
        }

	/** Supports $(B `Key` in `this`) syntax. */
	bool opBinaryRight(string op)(in Key key) const nothrow
            if (op == "in")
	{
            return contains(key);
	}

    }
    else
    {
        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        Value* contains(in Key key) const
        {
            return null;
        }
    }

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
            import std.algorithm : count, filter;
            foreach (sub; curr.subs[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(Br2* curr)
        {
            import std.algorithm : count, filter;
            foreach (sub; curr.subs[].filter!(sub => sub)) // TODO use static foreach
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
            if (auto subBrM = curr.peek!BrM)
            {
                if (subBrM != BrM.oneSet)
                {
                    release(subBrM);
                }
            }
            else if (auto subBr2 = curr.peek!Br2)
            {
                release(subBr2);
            }
            else if (auto subLfM = curr.peek!LfM)
            {
                release(subLfM);
            }
            else if (curr)
            {
                assert(false, "Unknown type of non-null pointer");
            }
        }
    }

    void freeNode(NodeType)(NodeType* nt) @trusted
    {
        debug --_nodeCount;
        free(cast(void*)nt);  // TODO Allocator.free
    }

    /** Ensure that root `Node` is allocated. */
    void ensureRootNode(U = Br2)()
    {
        if (!_root.ptr) { _root = construct!U; }
    }

    /// Returns: number of nodes used in `this` tree.
    pragma(inline) debug size_t nodeCount() @safe pure nothrow @nogc { return _nodeCount; }

    private Node _root;
    size_t _length = 0;

    debug size_t _nodeCount = 0;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator of set-version of `RadixTree`.
auto radixTreeSet(Key, size_t radix = 4)() { return RadixTree!(Key, void, radix)(); }

/// Instantiator of map-version of `RadixTree`.
auto radixTreeMap(Key, Value, size_t radix = 4)() { return RadixTree!(Key, Value, radix)(); }

/// Verify behaviour of `radixTreeSet` and `radixTreeMap` with explicit radix `radix`.
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

@safe pure nothrow // @nogc
unittest
{
    check!4;
    check!8;
}

/// Performance benchmark
version(benchmark) unittest
{
    import core.thread : sleep;
    import std.range : iota;

    import std.algorithm : equal;
    struct TestValueType { int i; float f; string s; }
    alias Value = TestValueType;
    import std.meta : AliasSeq;
    foreach (Key; AliasSeq!(uint))
    {
        auto set = radixTreeSet!(Key);

        static assert(set.isSet);

        import std.conv : to;
        import std.datetime : StopWatch, AutoStart, Duration;

        enum n = 10_000_000;

        import std.array : array;
        import std.random : randomShuffle;

        const useUniqueRandom = false;

        // TODO functionize to randomIota in range_ex.d
        auto randomIotaSamples = 0.iota(n).array; randomIotaSamples.randomShuffle;

        // TODO functionize to lazy generate!rand
        import random_ex : randomize;
        auto randomAnySamples = new Key[n]; randomAnySamples.randomize;

        {
            auto sw = StopWatch(AutoStart.yes);

            foreach (Key k; randomAnySamples)
            {
                if (useUniqueRandom)
                    assert(set.insert(k));
                else
                    set.insert(k);
            }

            dln("trie: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            auto uhists = set.usageHistograms;
            dln("2-Branch Usage Histogram: ", uhists.br2);
            dln("M-Branch Usage Histogram: ", uhists.brM);
            dln("M-Leaf   Usage Histogram: ", uhists.lfM);
            sleep(2);
            dln("Sleeping done");
        }

        {
            auto sw = StopWatch(AutoStart.yes);
            bool[int] aa;

            foreach (Key k; randomAnySamples) { aa[k] = true; }

            dln("D-AA: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            sleep(2);
            dln("Sleeping done");
        }

        auto map = radixTreeMap!(Key, Value);
        static assert(map.isMap);

        map.insert(Key.init, Value.init);
    }
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
