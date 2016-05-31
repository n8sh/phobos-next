/** Tries and PrefixTrees.

    See also: https://en.wikipedia.org/wiki/Trie
    See also: https://en.wikipedia.org/wiki/Radix_tree

    TODO Store `isKey` in top-most bit of length part of `IxsN prefix` in branch node.

    TODO Optimize PBr.findSub for specific `subPopulation`

    TODO Expand PBr to BBr when sizeof BrN is larger than 32 bytes (256 bits) and all
    leaves are single SLf. Converted to when BrN.length >= someLimit

    TODO Add function reprefix({PBr|FBr) and call after insertAt({PBr|FBr}). Only useful when one single leaf is present?
    TODO Is std.algorithm.countUntil the most suitable function to use in setSub(PBr*, ...)
    TODO Make array indexing/slicing as @trusted and use .ptr[] instead of [] when things are stable
    TODO Use std.experimental.allocator

    TODO Can we somehow overload opIndex so we can do brM[i] instead of more cumbersome (*brM)[i] when brM is of type FBr*?

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

/** Raw Internal (Unsigned Integer) Binary Key. */
alias Key(size_t span) = Mod!(2^^span)[]; // TODO use bitset to more naturally support span != 8
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

/** Statically allocated `Ix`-array of fixed pre-allocated length `maxLength`.
 */
struct IxsN(size_t maxLength,
            uint span = 8)
    if (maxLength >= 2)         // no use storing less than 2 bytes
{
    enum M = 2^^span;     // branch-multiplicity, typically either 2, 4, 16 or 256
    alias Ix = Mod!M;

    this(Ixs...)(Ixs ixs)
        if (Ixs.length >= 1 && Ixs.length <= maxLength)
    {
        foreach (const i, const ix; ixs)
        {
            this.ixs[i] = ix;
        }
        this._length = ixs.length;
    }

    this(Ix[] ixs)
    {
        assert(ixs.length <= maxLength);
        this.ixs[0 .. ixs.length] = ixs;
        this._length = cast(ubyte)ixs.length;
    }

    @property auto toString() const
    {
        import std.conv : to;
        return ixs[0 .. _length].to!string;
    }

    @safe pure nothrow @nogc:

    bool empty() const { return _length == 0; }
    bool full() const { return _length == maxLength; }

    auto ref front() inout
    {
        assert(!empty);
        return ixs[0];
    }

    auto ref back() inout
    {
        assert(!empty);
        return ixs[_length - 1];
    }

    void popFront()
    {
        assert(!empty);
        // TODO is there a reusable Phobos function for this?
        foreach (const i; 0 .. _length - 1)
        {
            ixs[i] = ixs[i + 1]; // TODO move construct?
        }
        --_length;
    }

    void popBack() { assert(!empty); --_length; }
    void pushBack(Ixs...)(Ixs moreIxs)
        if (Ixs.length <= maxLength)
    {
        assert(!full);
        foreach (const i, const ix; moreIxs)
        {
            this.ixs[_length + i] = ix;
        }
        _length += Ixs.length;
    }

    auto chunks() inout { return ixs[0 .. _length]; }
    alias chunks this;

    auto length() const { return _length; }

private:
    ubyte _length;               // number of defined elements in ixs
    Ix[maxLength] ixs;          // indexes
}

static assert(IxsN!(6, 8).sizeof == 7);

@safe pure nothrow unittest
{
    import std.algorithm : equal;
    import modulo : mod;

    enum span = 8;
    enum M = 2^^span;

    alias Ix = Mod!(M, ubyte);
    Ix[] ixs = [11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M];
    enum maxLength = 7;

    auto x = IxsN!(maxLength, span)(ixs);
    auto y = IxsN!(maxLength, span)(11.mod!M, 22.mod!M, 33.mod!M, 44.mod!M);

    assert(x == y);

    assert(x.length == 4);
    assert(!x.empty);

    assert(x.equal([11, 22, 33, 44]));
    assert(!x.full);
    x.popFront;
    assert(x.equal([22, 33, 44]));
    assert(!x.full);
    x.popBack;
    assert(x.equal([22, 33]));
    assert(!x.full);
    x.popFront;
    assert(x.equal([33]));
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
        TODO respect byteorder in `MLf` to work with `WordVariant`
        TODO implement and use opSlice instead of .suffix[]
    */
    static if (size_t.sizeof == 8) // 64-bit CPU
    {
        static if (span == 8)
        {
            /// Variable-Length Single-Key Leaf
            struct SLf
            {
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof;
                this(Ix[] suffix) { this.suffix = suffix; }

                @property string toString() const @safe pure
                {
                    import std.string : format;
                    string s;
                    foreach (key; suffix) { s ~= format("%.2X", key); } // in hexadecimal
                    return s;
                }

                IxsN!(maxLength, span) suffix;
                // alias suffix this;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }

            /// Fixed-Length Multiple-Key Leaf
            struct MLf
            {
                enum maxLength = (size_t.sizeof - 2) / Ix.sizeof; // maximum number of elements

                this(Ixs...)(Ixs ixs)
                if (Ixs.length >= 1 && Ixs.length <= maxLength)
                {
                    this.keys = ixs;
                }

                pragma(inline) bool contains(Key!span key) const @nogc
                {
                    // TODO use binarySearch
                    import std.algorithm.searching : canFind;
                    return (key.length == 1 &&
                            keys[].canFind(key[0]));
                }

                @property string toString() const @safe pure
                {
                    import std.string : format;
                    string s;
                    foreach (key; keys) { s ~= format("%.2X", key) ~ ','; } // in hexadecimal
                    return s;
                }

                IxsN!(maxLength, span) keys;
                // alias keys this;
            private:
                ubyte _mustBeIgnored = 0; // must be here and ignored because it contains `WordVariant` type of `Node`
            }
        }
    }
    else
    {
        static assert(false, "Currently requires a 64-bit CPU (size_t.sizeof == 8)");
    }

    static if (span == 8)
    {
        alias DefaultRootType = PBr*;
    }

    alias DefaultBr = DefaultRootType;
    alias DefaultLf = MLf;

    static if (isSet)
        static assert(MLf.sizeof == size_t.sizeof); // assert that it's size matches platform word-size

    /** Node types. */
    alias NodeTypes = AliasSeq!(SLf,
                                MLf,
                                PBr*,
                                FLf*,
                                FBr*);

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
    alias FLf_PopHist = size_t[radix];

    /** 4-Branch population histogram.
        Index maps to population with value range (1 .. 4).
    */
    alias PBr_PopHist = size_t[4];

    /** radix-Branch population histogram.
        Index maps to population with value range (1 .. `radix`).
    */
    alias FBr_PopHist = size_t[radix];

    /** radix-Leaf population histogram.
        Index maps to population with value range (1 .. `radix`).
    */
    alias LeafM_PopHist = size_t[radix];

    /** Tree Population and Memory-Usage Statistics. */
    struct Stats
    {
        FLf_PopHist popHist_FLf;
        PBr_PopHist popHist_PBr; // packed branch population histogram
        FBr_PopHist popHist_FBr; // full branch population histogram

        size_t allSLf0CountOfPBr; // number of `PBr` which sub-branches are all `SLf` of length 0
        size_t allSLf0CountOfFBr; // number of `FBr` which sub-branches are all `SLf` of length 0

        /** Maps `Node` type/index `Ix` to population.

            Used to calculate complete tree memory usage, excluding allocator
            overhead typically via `malloc` and `calloc`.
         */
        IndexedArray!(size_t, Node.Ix) popByNodeType;
        static assert(is(typeof(popByNodeType).Index == Node.Ix));
    }

    /** Full Bitset Branch with only bottom-most leaves. */
    static private struct FLf
    {
        enum maxPrefixLength = 5;
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
        pragma(inline) bool empty() const @nogc { return _keyBits.allZero; }

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
            ++stats.popHist_FLf[count - 1]; // TODO type-safe indexing
        }

        private:
        BitSet!radix _keyBits;  // 32 bytes
        IxsN!maxPrefixLength prefix; // prefix common to all `subNodes` (also called edge-label)
        bool isKey;
    }

    static assert(FLf.sizeof == 40);

    /** Sparse/Packed/Partial 4-way branch. */
    static private struct PBr
    {
        enum N = 4;

        enum maxPrefixLength = 7;

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
            const backIx = subPopulation.mod!N;
            subIxSlots[backIx] = sub[0];
            subNodeSlots[backIx] = sub[1];
            subPopulation = cast(ubyte)(subPopulation + 1);
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
            default:
                // TODO do binary search
                foreach (const i_; 0 ..  subPopulation)
                {
                    const i = i_.mod!N;
                    if (subIxSlots[i] == ix) { return subNodeSlots[i]; }
                }
                break;
            }
            return Node.init;
        }

        pragma(inline) bool empty() const @nogc { return subPopulation == 0; }
        pragma(inline) bool full() const @nogc { return subPopulation == N; }

        pragma(inline) auto subNodes() inout @nogc
        {
            return subNodeSlots[0 .. subPopulation];
        }

        /** Returns `true` if this branch can be packed into a bitset, that is
            contains only sub-nodes of type `SLf` of zero length. */
        bool isBitPackable() const /* TODO @nogc */
        {
            foreach (const sub; subNodes)
            {
                if (const subSLfRef = sub.peek!SLf)
                {
                    const subSLf = *subSLfRef;
                    if (subSLf.suffix.length != 0) { return false; }
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
            ++stats.popHist_PBr[count - 1]; // TODO type-safe indexing
            stats.allSLf0CountOfPBr += isBitPackable;
        }

        private:

        // members in order of decreasing `alignof`:
        StrictlyIndexed!(Node[N]) subNodeSlots;
        IxsN!maxPrefixLength prefix; // prefix common to all `subNodes` (also called edge-label)
        StrictlyIndexed!(Ix[N]) subIxSlots;
        mixin(bitfields!(ubyte, "subPopulation", 7, // counts length of defined elements in subNodeSlots
                         bool, "isKey", 1)); // key at this branch is occupied
    }

    static assert(PBr.sizeof == 48);

    /** Dense/Unpacked `radix`-branch with `radix` number of sub-nodes. */
    static private struct FBr
    {
        enum maxPrefixLength = 15; // we can afford larger prefix here because FBr is so large

        @safe pure nothrow:

        this(Ix[] prefix, bool isKey = false)
        {
            this.prefix = prefix;
            this.isKey = isKey;
        }

        this(PBr* rhs)
        {
            this.prefix = rhs.prefix;
            this.isKey = rhs.isKey;
            foreach (const i; 0 .. rhs.subPopulation) // each sub node. TODO use iota!(Mod!N)
            {
                const iN = i.mod!(PBr.N);
                const subIx = rhs.subIxSlots[iN];
                this.subNodes[subIx] = rhs.subNodes[iN];
            }
        }

        IxsN!maxPrefixLength prefix; // prefix (edge-label) common to all `subNodes`
        bool isKey;      // key at this branch is occupied
        StrictlyIndexed!(Node[radix]) subNodes;

        /** Returns `true` if this branch can be packed into a bitset, that is
            contains only subNodes of type `SLf` of zero length. */
        bool isBitPackable() const @safe pure nothrow /* TODO @nogc */
        {
            foreach (const subNode; subNodes)
            {
                if (const subSLfRef = subNode.peek!SLf)
                {
                    if ((*subSLfRef).suffix.length != 0)
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
            ++stats.popHist_FBr[count - 1]; // TODO type-safe indexing
            stats.allSLf0CountOfFBr += isBitPackable;
        }
    }

    static if (false)
    {
        pragma(msg, "PBr.sizeof:", PBr.sizeof, " PBr.alignof:", PBr.alignof);
        pragma(msg, "PBr.subNodes.sizeof:", PBr.subNodes.sizeof, " PBr.subNodes.alignof:", PBr.subNodes.alignof);
        pragma(msg, "PBr.prefix.sizeof:", PBr.prefix.sizeof, " PBr.prefix.alignof:", PBr.prefix.alignof);
        pragma(msg, "PBr.subIxs.sizeof:", PBr.subIxs.sizeof, " PBr.subIxs.alignof:", PBr.subIxs.alignof);
    }

    /** Set sub-`Node` of branch `Node curr` at index `ix` to `subNode`. */
    pragma(inline) Node setSub(Node curr, Ix subIx, Node subNode)
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FLfPtr: return setSub(curr.as!(FLf*), subIx, subNode);
        case Node.Ix.ix_PBrPtr: return setSub(curr.as!(PBr*), subIx, subNode);
        case Node.Ix.ix_FBrPtr: return setSub(curr.as!(FBr*), subIx, subNode);
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }
    /// ditto
    Node setSub(FLf* curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        if (const subSLfRef = subNode.peek!(SLf))
        {
            const subSLf = *subSLfRef;
            if (subSLf.suffix.empty)
            {
                curr._keyBits[subIx] = true;
                freeNode(subNode); // free it because it's stored inside the bitset itself
                return Node(curr);
            }
            else
            {
            }
        }
        const currValue = *curr;
        show!currValue;
        show!subIx;
        show!subNode;
        assert(false, "Expand FLf into either PBr or FBr depending on curr.popcnt");
    }
    /// ditto
    Node setSub(PBr* curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        import std.algorithm : countUntil;
        const i = curr.subIxSlots[0 .. curr.subPopulation].countUntil(subIx); // TODO is this the preferred function?
        if (i != -1)            // if hit. TODO use bool conversion if this gets added to countUntil
        {
            curr.subNodeSlots[i.mod!(curr.N)] = subNode; // reuse
        }
        else if (!curr.full)     // if room left in curr
        {
            curr.pushBackSub(tuple(subIx, subNode)); // add one to existing
        }
        else                    // if no room left in curr we need to expand
        {
            auto next = construct!(FBr*)(curr);
            freeNode(curr);
            return setSub(next, subIx, subNode); // fast, because directly calls setSub(FBr*, ...)
        }
        return Node(curr);
    }
    /// ditto
    pragma(inline) Node setSub(FBr* curr, Ix subIx, Node subNode) @safe pure nothrow /* TODO @nogc */
    {
        curr.subNodes[subIx] = subNode;
        return Node(curr);
    }

    /** Get sub-`Node` of branch `Node curr` at index `ix. */
    inout(Node) getSub(inout Node curr, Ix ix) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        // case Node.Ix.ix_SLf:
        //     auto currSLf = curr.as!(SLf);
        //     if (currSLf.suffix.length == 1 && currSLf.suffix[0] == ix) { return curr; }
        //     break;
        case Node.Ix.ix_FLfPtr:
            if (curr.as!(FLf*).hasSubAt(ix))
            {
                return Node(SLf.init);
            }
            break;
        case Node.Ix.ix_PBrPtr:
            if (auto subNode = curr.as!(PBr*).findSub(ix))
            {
                return subNode;
            }
            break;
        case Node.Ix.ix_FBrPtr:
            return curr.as!(FBr*).subNodes[ix];
        default:
            assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
        return Node.init;
    }

    /** Returns: `true` if `curr` is occupied, `false` otherwise. */
    pragma(inline) bool isKey(Node curr) const @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FLfPtr: return curr.as!(FLf*).isKey;
        case Node.Ix.ix_PBrPtr: return curr.as!(PBr*).isKey;
        case Node.Ix.ix_FBrPtr: return curr.as!(FBr*).isKey;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    pragma(inline) void makeKey(Node curr) const @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_FLfPtr: curr.as!(FLf*).isKey = true; break;
        case Node.Ix.ix_PBrPtr: curr.as!(PBr*).isKey = true; break;
        case Node.Ix.ix_FBrPtr: curr.as!(FBr*).isKey = true; break;
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Get prefix of node `curr`. */
    auto getPrefix(inout Node curr) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SLf:    return curr.as!(SLf).suffix[]; // suffix is the prefix
        case Node.Ix.ix_MLf:    return inout(Ix[]).init; // no prefix
        case Node.Ix.ix_FLfPtr: return curr.as!(FLf*).prefix[];
        case Node.Ix.ix_PBrPtr: return curr.as!(PBr*).prefix[];
        case Node.Ix.ix_FBrPtr: return curr.as!(FBr*).prefix[];
            // TODO extend to leaves aswell?
        default: assert(false, "Unsupported Node type " ~ curr.typeIx.to!string);
        }
    }

    /** Set prefix of branch node `curr` to `prefix`. */
    void setPrefix(Node curr, Ix[] prefix) @safe pure nothrow
    {
        switch (curr.typeIx)
        {
        case Node.Ix.ix_SLf:    curr.as!(SLf).suffix  = typeof(curr.as!(SLf).suffix)(prefix); break;
        case Node.Ix.ix_MLf:    assert(prefix.length == 0); break;
        case Node.Ix.ix_FLfPtr: curr.as!(FLf*).prefix = typeof(curr.as!(FLf*).prefix)(prefix); break;
        case Node.Ix.ix_PBrPtr: curr.as!(PBr*).prefix = typeof(curr.as!(PBr*).prefix)(prefix); break;
        case Node.Ix.ix_FBrPtr: curr.as!(FBr*).prefix = typeof(curr.as!(FBr*).prefix)(prefix); break;
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
        debug assert(_branchCount == 0, "Pointer node count is not zero, but " ~ _branchCount.to!string);
    }

    const @safe pure nothrow /* TODO @nogc */
    {
        /** Returns: `true` if `key` is stored, `false` otherwise. */
        pragma(inline) bool contains(Key!span key)
        {
            // dln("contains key=", key);
            return containsAt(_root, key);
        }

        /** Returns: `true` if `key` is stored under `curr`, `false` otherwise. */
        pragma(inline) bool containsAt(Node curr, Key!span key)
        {
            import std.algorithm : skipOver;
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: return false;
            case ix_SLf:    return curr.as!(SLf).suffix == key;
            case ix_MLf:    return curr.as!(MLf).contains(key);
            case ix_FLfPtr: return curr.as!(FLf*).contains(key);
            case ix_PBrPtr:
                auto curr_ = curr.as!(PBr*);
                // dln("BBr: key=", key, " prefix:", curr_.prefix, " isKey:", curr_.isKey);
                return (key.skipOver(curr_.prefix) &&
                        (key.length == 0 && curr_.isKey ||                 // either stored at `curr`
                         key.length >= 1 && containsAt(curr_.findSub(key[0]), key[1 .. $]))); // recurse
            case ix_FBrPtr:
                auto curr_ = curr.as!(FBr*);
                // dln("FBr: key=", key, " prefix:", curr_.prefix, " isKey:", curr_.isKey);
                return (key.skipOver(curr_.prefix) &&
                        (key.length == 0 && curr_.isKey ||                 // either stored at `curr`
                         key.length >= 1 && containsAt(curr_.subNodes[key[0]], key[1 .. $]))); // recurse
            }
        }
    }

    @safe pure nothrow /* TODO @nogc */
    {
        /** Insert `key` into `this` tree. */
        pragma(inline) Node insert(Key!span key, out bool wasAdded)
        {
            // dln("insert key=", key);
            return _root = insertAt(_root, key, 0, wasAdded);
        }

        Node insertNew(Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            if (key.length == 1)
            {
                wasAdded = true;
                return Node(construct!(MLf)(key[0]));
            }
            else if (key.length <= SLf.maxLength)
            {
                wasAdded = true;
                return Node(construct!(SLf)(key));
            }
            else                // key doesn't fit in a `SLf`
            {
                import std.algorithm : min;
                auto brKey = key[0 .. min(key.length, DefaultBr.maxPrefixLength)];
                dln("Creating DefaultBr: key:", key, " brKey:", brKey, " superPrefixLength:", superPrefixLength);
                auto next = insertAt(Node(construct!(DefaultBr)(brKey, false)), // as much as possible of key in branch prefix
                                     key, superPrefixLength, wasAdded);
                assert(wasAdded);
                return next;
            }
        }

        /** Insert `key` into sub-tree under root `curr`. */
        pragma(inline) Node insertAt(Node curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
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
                case undefined: break;
                case ix_SLf:    return insertAt(curr.as!(SLf), key, superPrefixLength, wasAdded);
                case ix_MLf:    return insertAt(curr.as!(MLf), key, superPrefixLength, wasAdded);

                case ix_FLfPtr:
                case ix_PBrPtr:
                case ix_FBrPtr: return insertAtBranch(curr, key, superPrefixLength, wasAdded);
                }
                assert(false);
            }
        }

        Node insertAtBranch(Node curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            import std.algorithm : commonPrefix;
            auto currPrefix = getPrefix(curr);
            auto matchedPrefix = commonPrefix(key, currPrefix);

            // in order of descending probability
            // most probable: key is an extension of prefix: prefix:"ab", key:"abcd"
            if (matchedPrefix.length == currPrefix.length &&
                matchedPrefix.length < key.length)
            {
                key = key[matchedPrefix.length .. $]; // strip `currPrefix from beginning of `key`
                superPrefixLength += matchedPrefix.length;
                // continue below
            }
            // prefix is an extension of key: prefix:"abcd", key:"ab"
            else if (matchedPrefix.length == key.length &&
                     matchedPrefix.length < currPrefix.length)
            {
                const subIx = currPrefix[matchedPrefix.length]; // need index first
                setPrefix(curr, currPrefix[matchedPrefix.length + 1 .. $]); // drop matchedPrefix plus index
                dln("Creating DefaultBr:", " matchedPrefix:", matchedPrefix);
                return Node(construct!(DefaultBr)(matchedPrefix, true, // `true` because `key` occupies this node
                                                  subIx, curr));
            }
            // prefix and key share beginning: prefix:"ab11", key:"ab22"
            else if (matchedPrefix.length < key.length &&
                     matchedPrefix.length < currPrefix.length)
            {
                const subIx = currPrefix[matchedPrefix.length]; // need index first
                setPrefix(curr, currPrefix[matchedPrefix.length + 1 .. $]); // drop matchedPrefix plus index
                dln("Creating DefaultBr:", " matchedPrefix:", matchedPrefix);
                curr = Node(construct!(DefaultBr)(matchedPrefix, false, // key is not occupied
                                                  subIx, curr));
                key = key[matchedPrefix.length .. $];
                superPrefixLength += matchedPrefix.length;
            }
            // prefix:"ab", key:"ab"
            else if (matchedPrefix.length == currPrefix.length && // exact key prefix match
                     matchedPrefix.length == key.length)
            {
                if (!isKey(curr))
                {
                    makeKey(curr);
                    wasAdded = true;
                }
                return curr;
            }
            // prefix:"ab", key:"cd"
            else if (matchedPrefix.length == 0) // no prefix key match
            {
                if (currPrefix.length == 0) // no current prefix
                {
                    // continue below
                }
                else
                {
                    const subIx = currPrefix[0]; // subIx = 'a'
                    setPrefix(curr, currPrefix[1 .. $].to!(typeof(DefaultBr.prefix))); // new prefix becomes "b"
                    dln("Creating DefaultBr:");
                    return insertAt(Node(construct!(DefaultBr)(Ix[].init, false, subIx, curr)),
                                    key,
                                    superPrefixLength,
                                    wasAdded);
                }
            }

            const ix = key[0];
            return setSub(curr, ix,
                          insertAt(getSub(curr, ix), // recurse
                                   key[1 .. $],
                                   superPrefixLength + 1,
                                   wasAdded));
        }

        Node insertAt(SLf curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            import std.algorithm : commonPrefix;

            if (key.length == 0)
            {
                assert(curr.suffix.empty, "Leaf is not empty when key is");
                return Node(curr);
            }

            auto matchedPrefix = commonPrefix(key, curr.suffix);
            if (curr.suffix.length == key.length)
            {
                if (matchedPrefix.length == key.length) // curr.suffix, key and matchedPrefix all equal
                {
                    return Node(curr); // already stored in `curr`
                }
                else if (matchedPrefix.length + 1 == key.length) // key and curr.suffix are both matchedPrefix plus one extra
                {
                    auto next = construct!(FLf*)(matchedPrefix, false,
                                                 curr.suffix[$ - 1],
                                                 key[$ - 1]);
                    wasAdded = true;
                    freeNode(curr);
                    return Node(next);
                }
            }
            return insertAt(split(curr, matchedPrefix, key),
                            key, superPrefixLength, wasAdded);
        }

        Node insertAt(MLf curr, Key!span key, size_t superPrefixLength, out bool wasAdded)
        {
            assert(key.length == 1);

            // check if already stored in `curr`
            if (curr.contains(key)) { return Node(curr); }

            // not already stored `curr` so add it
            if (curr.keys.length < curr.maxLength) // if room left
            {
                curr.keys.pushBack(key[0]);
                wasAdded = true;
                return Node(curr); // current node still ok
            }
            else
            {
                return insertAt(Node(expand(curr)), key, superPrefixLength, wasAdded); // NOTE stay at same (depth)
            }
        }

        /** Split `curr` using `prefix`. */
        Node split(SLf curr, Key!span prefix, Key!span key) // TODO key here is a bit malplaced
        {
            Node next;
            if (curr.suffix.length == 1 && key.length == 1) // if (outer) leaf node storage is possible
            {
                if (prefix.length == 0)
                {
                    return Node(construct!(MLf)(curr.suffix)); // TODO removing parameter has no effect. why?
                }
                else if (prefix.length == 1)
                {
                    assert(false, "Use P1Lf with single-length prefix and a maximum of 4 ");
                }
                else
                {
                    next = construct!(FLf*)(prefix, false);
                }
            }
            if (!next)
            {
                next = construct!(DefaultBr)(prefix, false);
            }

            bool wasAdded;      // dummy
            auto node = insertAt(next, curr.suffix, 0, wasAdded);
            assert(wasAdded); // assure that existing key was reinserted
            freeNode(curr);   // remove old current

            return node;
        }

        /** Construct and return sub-Node at `key`.  */
        Node constructSub(Key!span key)
        {
            if (key.length == 0)
            {
                return Node(construct!(SLf));
            }
            else
            {
                const bool isLast = key.length == 1;
                return (isLast ?
                        Node(construct!(DefaultLf)) :
                        Node(construct!(DefaultBr)));
            }
        }

        /** Destructively expand `curr` into a `FLf` and return it. */
        FLf* expand(MLf curr)
        {
            auto next = construct!(typeof(return));
            foreach (const ixM; curr.keys[0 .. curr.keys.length])
            {
                assert(!next._keyBits[ixM]); // assert no duplicates in keys
                next._keyBits[ixM] = true;
            }
            freeNode(curr);
            return next;
        }

    }

    /** Returns: `true` iff tree is empty (no elements stored). */
    bool empty() const @safe pure nothrow /* TODO @nogc */ { return !_root; }

    /** Returns: number of elements store. */
    size_t length() const @safe pure nothrow /* TODO @nogc */ { return _length; }

    private:

    /** Allocate (if pointer) and Construct a `Node`-type of value type `U`
        using constructor arguments `args` of `Args`. */
    auto construct(U, Args...)(Args args) @trusted
    {
        version(debugAllocations) { dln("constructing ", U.stringof, " from ", args); }
        static if (isPointer!U)
        {
            debug ++_branchCount;
            import std.conv : emplace;
            return emplace(cast(U)malloc((*U.init).sizeof), args);
            // TODO ensure alignment of node at least that of U.alignof
        }
        else
        {
            return U(args);
        }
    }

    void freeNode(NodeType)(NodeType nt) @trusted
    {
        version(debugAllocations) { dln("freeing ", NodeType.stringof, " ", nt); }
        static if (isPointer!NodeType)
        {
            free(cast(void*)nt);  // TODO Allocator.free
            debug --_branchCount;
        }
    }

    @safe pure nothrow /* TODO @nogc */
    {
        void release(FLf* curr)
        {
            freeNode(curr);
        }

        void release(PBr* curr)
        {
            foreach (sub; curr.subNodes[0 .. curr.subPopulation])
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(FBr* curr)
        {
            foreach (sub; curr.subNodes[].filter!(sub => sub)) // TODO use static foreach
            {
                release(sub); // recurse
            }
            freeNode(curr);
        }

        void release(SLf curr) { freeNode(curr); }
        void release(MLf curr) { freeNode(curr); }

        void release(Node curr)
        {
            version(debugAllocations) { dln("releasing Node ", curr); }
            final switch (curr.typeIx) with (Node.Ix)
            {
            case undefined: break; // ignored
            case ix_SLf: return release(curr.as!(SLf));
            case ix_MLf: return release(curr.as!(MLf));
            case ix_FLfPtr: return release(curr.as!(FLf*));
            case ix_PBrPtr: return release(curr.as!(PBr*));
            case ix_FBrPtr: return release(curr.as!(FBr*));
            }
        }
    }

    /** Ensure that root `Node` is allocated. */
    void ensureRootNode(U = DefaultRootType)()
    {
        if (!_root) { _root = construct!(U); }
    }

    /** Get fixed length of keys, or `size_t.max` if key length is variable. */
    size_t keyLength() const @safe pure nothrow @nogc { return _keyLength; }

    /** Returns: `true` if all keys are of fixed size, `false` otherwise. */
    bool hasFixedKeyLength() const @safe pure nothrow @nogc { return keyLength != size_t.max; }

    /// Returns: number of nodes used in `this` tree.
    pragma(inline) debug size_t branchCount() @safe pure nothrow /* TODO @nogc */ { return _branchCount; }

    void print() @safe const
    {
        printAt(_root, 0);
    }

    void printAt(Node curr, size_t depth) @safe const
    {
        import std.range : repeat;
        import std.stdio : write, writeln;

        if (!curr) { return; }

        foreach (const i; 0 .. depth)
        {
            write('-');
        }

        final switch (curr.typeIx) with (Node.Ix)
        {
        case undefined: break;
        case ix_SLf:
            auto curr_ = curr.as!(SLf);
            writeln(typeof(curr_).stringof, "#", curr_.suffix.length, ": ", curr_);
            break;
        case ix_MLf:
            auto curr_ = curr.as!(MLf);
            writeln(typeof(curr_).stringof, "#", curr_.keys.length, ": ", curr_);
            break;
        case ix_FLfPtr:
            auto curr_ = curr.as!(FLf*);
            write(typeof(*curr_).stringof, ":");
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            write(" #ones=", curr_._keyBits.countOnes);
            writeln();
            break;
        case ix_PBrPtr:
            auto curr_ = curr.as!(PBr*);
            write(typeof(*curr_).stringof, "#", curr_.subPopulation, ": ");
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            writeln();
            foreach (const subNode; curr_.subNodes)
            {
                printAt(subNode, depth + 1);
            }
            break;
        case ix_FBrPtr:
            auto curr_ = curr.as!(FBr*);
            write(typeof(*curr_).stringof, "#", curr_.subPopulation, ": ");
            writeln();
            if (!curr_.prefix.empty) { write(" prefix=", curr_.prefix); }
            foreach (const subNode; curr_.subNodes)
            {
                printAt(subNode, depth + 1);
            }

            break;
        }
    }

    private:
    Node _root;                 ///< tree root node
    size_t _length = 0; ///< number of elements (keys or key-value-pairs) currently stored under `_root`
    immutable _keyLength = size_t.max; ///< maximum length of key
    debug long _branchCount = 0;
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
        case ix_SLf: break; // TODO calculate()
        case ix_MLf: break; // TODO calculate()
        case ix_FLfPtr: sub.as!(RT.FLf*).calculate(stats); break;
        case ix_PBrPtr: sub.as!(RT.PBr*).calculate(stats); break;
        case ix_FBrPtr: sub.as!(RT.FBr*).calculate(stats); break;
        }
    }
}

static private Key!span remapKey(TypedKey, uint span = 8)(in TypedKey typedKey)
    @safe pure nothrow /* TODO @nogc */
    if (allSatisfy!(isTrieableKeyType, TypedKey))
{
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
                enum radix = 2^^span;
                key[bix] = (ukey >> bitShift) & (radix - 1); // part of value which is also an index
            }
        }

        return key.dup; // TODO avoid this GC-allocation
    }
    else static if (is(Unqual!TypedKey == string))
    {
        const ubyte[] key = typedKey.representation; // lexical byte-order
        return key;
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
        this._keyLength = isFixedTrieableKeyType!TypedKey ? TypedKey.sizeof : size_t.max;
    }

    /** Insert `key`.
        Returns: `true` if `key` wasn't previously inserted, `false` otherwise.
     */
    bool insert(in TypedKey typedKey)
        @safe pure nothrow /* TODO @nogc */
    {
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

@safe pure nothrow /* TODO @nogc */ unittest
{
    auto set = radixTreeSet!(ushort);

    assert(set.insert(0));
    assert(!set.insert(0));
    assert(set.branchCount == 0);

    foreach (const i; 1 .. 256)
    {
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.branchCount == 1);
    }

    assert(set.insert(256));
    assert(!set.insert(256));
    assert(set.branchCount == 2);

    assert(set.insert(257));
    assert(!set.insert(257));
}

// @safe pure nothrow
/* TODO @nogc */ unittest
{
    auto set = radixTreeSet!(ulong);

    assert(set.insert(0));
    assert(!set.insert(0));
    assert(set.branchCount == 1);

    foreach (const i; 1 .. 256)
    {
        assert(set.insert(i));
        assert(!set.insert(i));
        assert(set.branchCount == 1);
    }

    assert(set.insert(256));
    assert(!set.insert(256));
    assert(set.branchCount == 2);

    set.print();
}

/// Check correctness when span is `span` and for each `Key` in `Keys`.
auto check(uint span, Keys...)()
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
                    // dln("========================= Before insert of new key:", key);
                    if (useContains)
                    {
                        assert(!set.contains(key)); // key should not yet be in set
                        assert(key !in set);        // alternative syntax
                    }

                    // dln("========================= Inserting new key:", key);
                    assert(set.insert(key));  // insert new value returns `true` (previously not in set)
                    // dln("========================= Inserting existing key:", key);
                    assert(!set.insert(key)); // reinsert same value returns `false` (already in set)

                    // dln("========================= After insert of key:", key);
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

                    // import std.stdio : writeln; try { debug writeln(); } catch (Exception e) {} // empty newline
                }
                assert(set.length == length);
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
                    assert(set.insert(k));
                else
                    set.insert(k);

                /* second insert of same key should always return `false` to
                   indicate that key was already stored */
                static if (false) { assert(!set.insert(k)); }
            }

            writeln("trie: Added ", n, " ", Key.stringof, "s of size ", n*Key.sizeof/1e6, " megabytes in ", sw.peek().to!Duration, ". Sleeping...");
            auto stats = set.usageHistograms;
            writeln("Sparse Bit-Branch Population Histogram: ", stats.popHist_FLf);
            writeln("Sparse 4-Branch Population Histogram: ", stats.popHist_PBr);
            writeln("Dense radix=", 2^^span, "-Branch Population Histogram: ", stats.popHist_FBr);
            writeln("Population By Node Type: ", stats.popByNodeType);
            writeln("Number of PBr with SLf-0 only subNodes: ", stats.allSLf0CountOfPBr);
            writeln("Number of FBr with SLf-0 only subNodes: ", stats.allSLf0CountOfFBr);

            size_t totalBytesUsed = 0;
            foreach (Set.Node.Ix ix, pop; stats.popByNodeType) // TODO use stats.byPair when added to typecons_ex.d
            {
                size_t bytesUsed = 0;
                with (Set.Node.Ix)
                {
                    final switch (ix)
                    {
                    case undefined: break;
                    case ix_SLf:    bytesUsed = pop*Set.SLf.sizeof; break;
                    case ix_MLf:    bytesUsed = pop*Set.MLf.sizeof; break;
                    case ix_FLfPtr: bytesUsed = pop*Set.FLf.sizeof; totalBytesUsed += bytesUsed; break;
                    case ix_PBrPtr: bytesUsed = pop*Set.PBr.sizeof; totalBytesUsed += bytesUsed; break;
                    case ix_FBrPtr: bytesUsed = pop*Set.FBr.sizeof; totalBytesUsed += bytesUsed; break;
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

@safe pure nothrow /* TODO @nogc */
unittest
{
    // TODO Support this struct A { long x, y; }
    check!(8,
           double, float,
           long, int, short, byte,
           ulong, uint, ushort, ubyte,
           // string, wstring, dstring,
        );
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
