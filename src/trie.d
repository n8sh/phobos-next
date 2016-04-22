/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie

    TODO Add support for map insert

    TODO Need bijectToUnsigned in intsort to support ordered storage of float and double. Move it to bijections.

    TODO Add sparse 2^^n-branches for n < radix: 2^^1=B2, 2^^2=B4, 2^^3=B8, B^^4=B16. Use
    sortExactly from sortn.d to order their members.

    TODO Add traits isSparseNodeType, isDenseNodeType.

    TODO if `Value` is a `isScalarType` store it in Value[M]
    TODO if `Value` is a `bool` store it in bitset

    TODO Andrei!: Provide `opIndex` and make `opSlice` for set-case (`Value` is `void`) return `SortedRange`

    TODO Andrei!: Provide RandomAccess `opIndex` and `opSlice`! for variable-length keys aswell?

    TODO Andrei!: Provide in operator for fixed-length keys!?

    TODO Add RadixTreeRange.{front,popFront,empty}. Reuse RefCounted reference
    to _root. Add checks with `isSorted`.

    TODO Name members so they indicate that range isSorted.

    TODO Andrei!: Name members to indicate complexity

    TODO Benchmark `contains()` and compare it to to builtin AAs.in.

    TODO Extend bitop_ex.d with {set,get}{Bit,Qit,Bytes} and reuse

    TODO RadixTree: Assigning a void pointer to a class

    TODO Should opBinaryRight return void* instead of bool for set-case?

    TODO Replace
    - if (auto currBM = curr.peek!BM)
    - else if (const currLM = curr.peek!LM)
    with some compacter logic perhaps using mixins or pattern matching using
    switch-case on internally store type in `VariantPointer`.

    I've just started working on a RadixTree implementation in D here:

    https://github.com/nordlow/phobos-next/blob/master/src/trie.d

    The current solution is very generic as it CT-specializes to either a
    `RadixTreeMap` and `RadixTreeSet`. These provide very memory-efficient
    replacements for a `HashMap` and a `HashSet`.

    Each radix-tree branching contains 2^^radix pointers to the next
    sub-branching. In the RadixTreeSet case I want these pointers to also be
    able to contain special "codes" with the following meanings

    0x1: *only* value associated with sub-branching is set.

    0xFFFFFFFFFFFFFFFF: all values values in this sub-branch are set.
 */
module trie;

import std.traits : isIntegral, isSomeChar, isSomeString, isScalarType, isArray, allSatisfy, anySatisfy;
import std.range : isInputRange, ElementType;

import variant_pointer : VariantPointer;

version = benchmark;

import dbg : dln;

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

    static assert(radix <= 32, "Radix is currently limited to 32"); // TODO adjust?
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
    enum isFixed = isFixedTrieableKeyType!Key;

    /// `true` if tree has binary branch.
    enum isBinary = R == 2;

    /** Node types. */
    alias NodeTypes = AliasSeq!(BM, LM);

    /** Mutable node. */
    alias Node = VariantPointer!(NodeTypes);
    /** Constant node. */
    alias ConstNodePtr = VariantPointer!(staticMap!(ConstOf, NodeTypes));

    /** Radix Index
        TODO limit to an uint in the range 0..M-1.
    */
    alias MIx = uint;

    /** Tree Iterator. */
    struct It
    {
        bool opCast(T : bool)() const @safe pure nothrow @nogc { return node; }
        Node node;
        MIx mix;
    }

    /** Tree Range. */
    struct Range
    {
        It low;                 // start
        It high;                // beyond last
    }

    alias BranchUsageHistogram = size_t[M];

    /** Non-bottom branch node containing densly packed array of `M` number of
        pointers to sub-`BM`s or `Leaf`s.
    */
    static private struct BM
    {
        /// Indicates that only next/child at this index is occupied.
        static immutable oneSet = cast(typeof(this)*)1UL;

        static if (isFixed)
        {
            /** Indicates that all children of `this` branch are occupied. Only
                for fixed-sized `Keys`. */
            static immutable allSet = cast(typeof(this)*)size_t.max;
        }

        Node[M] subs;        // sub-branches

        /** Returns: depth of tree at this branch. */
        // size_t linearDepth() @safe pure nothrow const
        // {
        //     // TODO replace with fold when switching to 2.071
        //     import std.algorithm : map, reduce, max;
        //     return reduce!max(0UL, subs[].map!(sub => (sub.peek!(typeof(this)) == oneSet ? 1UL :
        //                                                  sub ? 1 + sub.linearDepth :
        //                                                  0UL)));
        // }

        /** Returns: depth of tree at this branch. */
        void calculate(ref BranchUsageHistogram hist) @safe pure nothrow const
        {
            import std.algorithm : count, filter;
            size_t nzcnt = 0; // number of non-zero branches
            foreach (sub; subs[].filter!(sub => sub))
            {
                ++nzcnt;
                if (const subBM = sub.peek!BM)
                {
                    if (subBM != BM.oneSet)
                    {
                        subBM.calculate(hist);
                    }
                }
                else if (const subLM = sub.peek!LM)
                {
                    subLM.calculate(hist);
                }
                else if (sub)
                {
                    assert(false, "Unknown type of non-null pointer");
                }
            }
            ++hist[nzcnt - 1];
        }

        static if (!isFixed)        // variable length keys only
        {
            LM subOccupations; // if i:th bit is set key (and optionally value) associated with sub[i] is also defined
            static if (isMap)
            {
                Value value;
            }
        }
    }

    /** Bottom-most leaf node of `RadixTree`-set storing `M` number of densly packed
        keys of fixed-length type `Key`.
    */
    static private struct LM
    {
        void calculate(ref BranchUsageHistogram hist) @safe pure const
        {
            import std.range : iota;
            foreach (const i; 0.iota(M))
            {
                if (keyLSBits[i])
                    ++hist[i];
            }
        }

        import bitset : BitSet;
        private BitSet!M keyLSBits; // if i:th bit is set corresponding sub is set

        static if (isMap)
        {
            static if (is(Value == bool))
            {
                BitSet!M values;
            }
            else
            {
                Value[M] values;
            }
        }
    }

    /// Get depth of tree.
    // size_t depth() @safe pure nothrow const
    // {
    //     return _root !is null ? _root.linearDepth : 0;
    // }

    BranchUsageHistogram branchUsageHistogram() const
    {
        typeof(return) hist;
        // TODO reuse rangeinterface when made available
        if (const bM = _root.peek!BM)
        {
            if (bM != BM.oneSet)
            {
                bM.calculate(hist);
            }
        }
        else if (const subLM = _root.peek!LM)
        {
            assert(false, "TODO");
        }
        else if (_root)
        {
            assert(false, "Unknown type of non-null pointer");
        }
        return hist;
    }

    this(this)
    {
        if (!_root.ptr) return;
        auto oldRootPtr = _root;
        makeRoot;
        auto curr = oldRootPtr;
        while (curr)
        {
            // TODO iterative or recursive?
        }
        assert(false, "TODO calculate tree by branches and leafs and make copies of them");
    }

    ~this()
    {
        if (_root) { release(_root); }
        // debug assert(_nodeCount == 0);
    }

    /** Get chunkIndex:th chunk of `radix` number of bits. */
    MIx bitsChunk(uint chunkIndex)(Key key) const @trusted pure nothrow
    {
        // calculate bit shift to current chunk
        static if (isIntegral!Key ||
                   isSomeChar!Key) // because top-most bit in ASCII coding (char) is often sparse
        {
            /* most signficant bit chunk first because integers are
               typically more sparse in more significant bits */
            enum shift = (maxDepth - 1 - chunkIndex)*R;
        }
        else
        {
            // default to most signficant bit chunk first
            enum shift = (maxDepth - 1 - chunkIndex)*R;
            // enum shift = chunkIndex*R; // least significant bit first
        }

        const u = *(cast(UnsignedOfSameSizeAs!Key*)(&key)); // TODO functionize and reuse here and in intsort.d
        const uint keyBitChunk = (u >> shift) & chunkMask; // part of value which is also an index
        static assert(radix <= 8*keyBitChunk.sizeof, "Need more precision in keyBitChunk");

        assert(keyBitChunk < M); // extra range check
        return keyBitChunk;
    }

    /** Insert `key`.

        Returns: a non-null Node if key was previously already inserted,
        `Node(null)` otherwise. This return value can then be used for map's
        `insert(Key key, Value value)` so a single implementation for
        `insert(Key key)` can be used for both set and map variant.
    */
    Node insert(Key key)
    {
        makeRoot;

        Node curr = _root;
        foreach (const ix; iota!(0, maxDepth)) // NOTE unrolled/inlined compile-time-foreach chunk index
        {
            const keyBitChunk = bitsChunk!ix(key);

            enum isLast = ix + 1 == maxDepth; // if this is the last chunk
            enum isSecondLast = ix + 2 == maxDepth; // if this is the second last chunk

            static if (isSecondLast)
            {
                if (auto currBM = curr.peek!BM)
                {
                    // assert that sub is a LM
                    if (!currBM.subs[keyBitChunk]) // if not yet set
                    {
                        currBM.subs[keyBitChunk] = allocateNode!LM;
                    }
                    else
                    {
                        assert(currBM.subs[keyBitChunk].peek!LM);
                    }
                    curr = currBM.subs[keyBitChunk]; // and visit it
                }
                else if (auto currLM = curr.peek!LM) { assert(false, "TODO"); }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
            else static if (isLast)
            {
                if (auto currBM = curr.peek!BM) // branch-M
                {
                    assert(currBM != BM.oneSet);
                    if (!currBM.subs[keyBitChunk])
                    {
                        currBM.subs[keyBitChunk] = BM.oneSet; // tag this as last
                        return Node(currBM); // a new value was inserted
                    }
                    else
                    {
                        static if (isFixedTrieableKeyType!Key)
                        {
                            assert(currBM.subs[keyBitChunk].peek!BM == BM.oneSet);
                            return Node(null); // value already set
                        }
                        else
                        {
                            assert(false, "TODO");
                            // TODO test this
                            if (curr.isOccupied)
                            {
                                return Node(null);
                            }
                            else
                            {
                                curr.isOccupied = false;
                                return Node(currBM);
                            }
                        }
                    }
                }
                else if (auto currLM = curr.peek!LM) // leaf-M
                {
                    if (!currLM.keyLSBits[keyBitChunk])
                    {
                        currLM.keyLSBits[keyBitChunk] = true;
                        return Node(currLM);
                    }
                    else
                    {
                        return Node(null);
                    }

                    assert(false, "TODO");
                }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
            else            // other
            {
                if (auto currBM = curr.peek!BM)
                {
                    assert(currBM != BM.oneSet);
                    if (!currBM.subs[keyBitChunk]) // if branch not yet visited
                    {
                        currBM.subs[keyBitChunk] = allocateNode!BM; // create it
                    }
                    curr = currBM.subs[keyBitChunk]; // and visit it
                }
                else if (auto currLM = curr.peek!LM) { assert(false, "TODO"); }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
        }
        assert(false, "End of function shouldn't be reached");
    }

    static if (isMap)
    {
        /** Insert `key`.
            Returns: `false` if key was previously already inserted, `true` otherwise.
        */
        Node insert(Key key, Value value)
        {
            Node node = insert(key);
            // TODO put value at node
            return node;
        }
    }

    static if (isSet)
    {

        /** Returns: `true` if key is contained in set, `false` otherwise. */
        bool contains(Key key) const nothrow
        {
            if (!_root) { return false; }

            Node curr = _root;
            foreach (const ix; iota!(0, maxDepth)) // NOTE unrolled/inlined compile-time-foreach chunk index
            {
                const keyBitChunk = bitsChunk!ix(key);
                if (auto currBM = curr.peek!BM)
                {
                    assert(currBM != BM.oneSet);
                    if (!currBM.subs[keyBitChunk])
                    {
                        return false;
                    }
                    else
                    {
                        static if (isFixedTrieableKeyType!Key)
                        {
                            if (currBM.subs[keyBitChunk].peek!BM == BM.oneSet)
                            {
                                return true;
                            }
                        }
                        else
                        {
                            if (currBM.isOccupied)
                            {
                                return false;
                            }
                            else
                            {
                                currBM.isOccupied = false;
                                return true;
                            }
                        }
                        curr = currBM.subs[keyBitChunk];
                    }
                }
                else if (const currLM = curr.peek!LM)
                {
                    return currLM.keyLSBits[keyBitChunk];
                }
                else if (curr) { assert(false, "Unknown type of non-null pointer"); }
            }
            return false;
        }

	/** Supports $(B `Key` in `this`) syntax. */
	bool opBinaryRight(string op)(Key key) const nothrow
            if (op == "in")
	{
            return contains(key);
	}

    }
    else
    {
        /** Returns: pointer to value if `key` is contained in set, null otherwise. */
        Value* contains(Key key) const
        {
            return null;
        }
    }

    private:

    NodeType* allocateNode(NodeType)() @trusted
    {
        NodeType* node = cast(typeof(return))calloc(1, NodeType.sizeof);
        debug ++_nodeCount;
        return node;
    }

    void release(BM* curr) pure nothrow
    {
        import std.algorithm : count, filter;
        foreach (sub; curr.subs[].filter!(sub => sub))
        {
            if (auto subBM = sub.peek!BM)
            {
                if (subBM != BM.oneSet)
                {
                    release(subBM);  // recurse
                }
            }
            else if (auto subLM = sub.peek!LM)
            {
                // ok
            }
            else if (curr)
            {
                assert(false, "Unknown type of non-null pointer");
            }
        }
        deallocateNode(curr);
    }

    void release(Node curr) pure nothrow
    {
        if (auto subBM = curr.peek!BM)
        {
            if (subBM != BM.oneSet)
            {
                release(subBM);  // recurse
            }
        }
        else if (auto subLM = curr.peek!LM)
        {
            assert(false, "TODO");
        }
        else if (curr)
        {
            assert(false, "Unknown type of non-null pointer");
        }
    }

    void deallocateNode(NodeType)(NodeType* nt) @trusted
    {
        debug --_nodeCount;
        free(cast(void*)nt);  // TODO Allocator.free
    }

    void makeRoot()
    {
        if (!_root.ptr) { _root = allocateNode!BM; }
    }

    /// Returns: number of branches used in `this` tree.
    debug size_t branchCount() @safe pure nothrow @nogc { return _nodeCount; }

    private Node _root;
    debug size_t _nodeCount = 0;
}
alias RadixTrie = RadixTree;
alias CompactPrefixTree = RadixTree;

/// Instantiator of set-version of `RadixTree`.
auto radixTreeSet(Key, size_t radix = 4)() { return RadixTree!(Key, void, radix)(); }

/// Instantiator of map-version of `RadixTree`.
auto radixTreeMap(Key, Value, size_t radix = 4)() { return RadixTree!(Key, Value, radix)(); }

auto check()
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
            auto set = radixTreeSet!(Key);

            static assert(set.isSet);

            foreach (Key k; 0.iota(512))
            {
                assert(!set.contains(k));

                assert(k !in set);

                assert(set.insert(k)); // insert new value returns `true`
                assert(!set.insert(k)); // reinsert same value returns `false`
                assert(!set.insert(k)); // reinsert same value returns `false`

                assert(set.contains(k));
                assert(k in set);
                // assert(set.depth == set.maxDepth);

                assert(!set.contains(k + 1)); // sub is yet inserted
            }

            // debug assert(set.branchCount == 40);

            auto map = radixTreeMap!(Key, Value);
            static assert(map.isMap);

            map.insert(Key.init, Value.init);
        }
    }
}

@safe pure nothrow unittest
{
    check();
}

/// Performance benchmark
version(benchmark) unittest
{
    import core.thread : sleep;
    import std.range : iota;
    foreach (const it; 0.iota(1))
    {
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

            {
                auto sw = StopWatch(AutoStart.yes);
                foreach (Key k; 0.iota(n))
                {
                    assert(set.insert(k)); // insert new value returns `true`
                }

                dln("trie: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
                dln("BranchUsageHistogram: ", set.branchUsageHistogram);
                sleep(5);
                dln("Sleeping done");
            }

            {
                auto sw = StopWatch(AutoStart.yes);
                bool[int] aa;
                foreach (Key k; 0.iota(n))
                {
                    aa[k] = true;
                }

                dln("D-AA: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            }

            auto map = radixTreeMap!(Key, Value);
            static assert(map.isMap);

            map.insert(Key.init, Value.init);
        }
    }
}

/** Static Iota.
    TODO Move to Phobos.
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
