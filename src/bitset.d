/** Statically sized variant of `std.bitmanip.BitArray.

    Copyright: Per Nordlöw 2016-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
 */
module bitarrayn;

version(unittest)
{
    import array_ex : s;
    import dbgio;
}

/** A statically sized `std.bitmanip.BitArray`.

    TODO Infer `Block` from `len` as is done for `Bound` and `Mod`.

    TODO Optimize `allOne`, `allZero` using intrinsic?
 */
struct BitArrayN(uint len, Block = size_t)
{
    import std.format : FormatSpec, format;
    import core.bitop : bitswap;
    import rational : Rational;
    import modulo : Mod;

    import std.traits : isUnsigned;
    static assert(isUnsigned!Block, "Block must be a builtin unsigned integer");

    static if (len >= 1)
    {
        alias Index = Mod!len;
    }

    static if (Block.sizeof == 8)
    {
        import core.bitop : bt, bts, btr; // use intrinsics
    }
    else
    {
        import bitop_ex : bt, bts, btr; // use own overloads
    }

    /** Number of bits per `Block`. */
    enum bitsPerBlock = 8*Block.sizeof;
    /** Number of `Block`s. */
    enum blockCount = (len + (bitsPerBlock-1)) / bitsPerBlock;

    /** Data stored as `Block`s. */
    private Block[blockCount] _blocks;

    /** Data as an array unsigned bytes. */
    const(ubyte)[] ubytes() const @trusted pure nothrow @nogc { return (cast(ubyte*)&_blocks)[0 .. _blocks.sizeof]; }
    immutable(ubyte)[] ubytes() immutable @trusted pure nothrow @nogc { return (cast(ubyte*)&_blocks)[0 .. _blocks.sizeof]; }

    /** Get pointer to data blocks. */
    @property inout (Block*) ptr() inout pure nothrow @nogc { return _blocks.ptr; }

    /** Reset all bits (to zero). */
    void reset() @safe pure nothrow @nogc { _blocks[] = 0; }

    /** Gets the amount of native words backing this $(D BitArrayN). */
    @property static uint dim() @safe pure nothrow @nogc { return blockCount; }

    /** Number of bits in the $(D BitArrayN). */
    enum length = len;

    /** BitArrayN range.

        TODO Provide opSliceAssign for interopability with range algorithms
        via private static struct member `Range`

        TODO Look at how std.container.array implements this.

        See also: https://dlang.org/phobos/std_bitmanip.html#bitsSet
    */
    struct Range
    {
        @safe pure @nogc:

        /// Returns: `true` iff `this` is empty.
        bool   empty()  const nothrow { return _i == _j; }
        /// Returns: `this` length.
        size_t length() const nothrow { return _j - _i; }

        /// Get front.
        bool front() const
        {
            assert(!empty); // only in debug mode since _store is range-checked
            return _store[_i];
        }
        /// Get back.
        bool back() const
        {
            assert(!empty);  // only in debug mode since _store is range-checked
            return _store[_j - 1];
        }

        /// Pop front.
        void popFront() { assert(!empty); ++_i; }
        /// Pop back.
        void popBack()  { assert(!empty); ++_i; }

    private:
        BitArrayN _store;          // copy of store
        size_t _i = 0;          // iterator into _store
        size_t _j = _store.length;
    }

    pragma(inline, true) Range opSlice() const @trusted pure nothrow
    {
        return Range(this);
    }

    pragma(inline, true) Range opSlice(size_t i, size_t j) const @trusted pure nothrow
    {
        return Range(this, i, j);
    }

    /** Gets the $(D i)'th bit in the $(D BitArrayN). */
    pragma(inline, true) bool opIndex(size_t i) const @trusted pure nothrow
    in
    {
        assert(i < len);        // TODO nothrow or not?
    }
    body
    {
        // Andrei: review for @@@64-bit@@@
        static if (Block.sizeof == 8)
        {
            return cast(bool)bt(ptr, i);
        }
        else
        {
            return bt(_blocks[i/bitsPerBlock], i%bitsPerBlock);
        }
    }

    /** Gets the $(D i)'th bit in the $(D BitArrayN). No range checking needed. */
    static if (len >= 1)
    {
        /** Get the $(D i)'th bit in the $(D BitArrayN).

            Avoids range-checking because `i` of type is bound to (0 .. len-1).
        */
        pragma(inline) bool opIndex(ModUInt)(Mod!(len, ModUInt) i) const @trusted pure nothrow
            if (isUnsigned!ModUInt)
        {
            static if (Block.sizeof == 8)
            {
                return cast(bool)bt(ptr, cast(size_t)i);
            }
            else
            {
                return bt(_blocks[i/bitsPerBlock], i%bitsPerBlock);
            }
        }

        /** Get the $(D i)'th bit in the $(D BitArrayN).
            Statically verifies that i is < BitArrayN length.
        */
        pragma(inline) bool at(size_t i)() const @trusted pure nothrow
            if (i < len)
        {
            return this[i];
        }
    }

    /** Puts the $(D i)'th bit in the $(D BitArrayN) to $(D b). */
    pragma(inline, true) auto ref put()(size_t i, bool b) @trusted pure nothrow
    {
        this[i] = b;
        return this;
    }

    /** Sets the $(D i)'th bit in the $(D BitArrayN). */
    import std.traits: isIntegral;
    pragma(inline, true) bool opIndexAssign(Index2)(bool b, Index2 i) @trusted pure nothrow
        if (isIntegral!Index2)
    in
    {
        // import std.traits: isMutable;
        // See also: http://stackoverflow.com/questions/19906516/static-parameter-function-specialization-in-d
        /* static if (!isMutable!Index2) { */
        /*     import std.conv: to; */
        /*     static assert(i < len, */
        /*                   "Index2 " ~ to!string(i) ~ " must be smaller than BitArrayN length " ~  to!string(len)); */
        /* } */
        assert(i < len);
    }
    body
    {
        b ? bts(ptr, cast(size_t)i) : btr(ptr, cast(size_t)i);
        return b;
    }

    static if (len >= 1)
    {
        /** Sets the $(D i)'th bit in the $(D BitArrayN). No range checking needed. */
        pragma(inline, true) bool opIndexAssign(ModUInt)(bool b, Mod!(len, ModUInt) i)
            @trusted pure nothrow @nogc
        if (isUnsigned!ModUInt)
        {
            b ? bts(ptr, cast(size_t)i) : btr(ptr, cast(size_t)i);
            return b;
        }
    }

    ///
    @safe pure nothrow @nogc unittest
    {
        BitArrayN!2 bs;
        bs[0] = true;
        assert(bs[0]);
    }

    /** Duplicates the $(D BitArrayN) and its contents. */
    @property BitArrayN dup() const
        @safe pure nothrow @nogc
    {
        return this;
    }

    /** Support for $(D foreach) loops for $(D BitArrayN). */
    int opApply(scope int delegate(ref bool) dg)
    {
        int result;

        for (size_t i = 0; i < len; ++i)
        {
            bool b = opIndex(i);
            result = dg(b);
            this[i] = b;
            if (result) { break; }
        }
        return result;
    }

    /** ditto */
    int opApply(scope int delegate(bool) dg) const
    {
        int result;
        for (size_t i = 0; i < len; ++i)
        {
            bool b = opIndex(i);
            result = dg(b);
            if (result) { break; }
        }
        return result;
    }

    /** ditto */
    int opApply(scope int delegate(ref size_t, ref bool) dg)
    {
        int result;
        for (size_t i = 0; i < len; ++i)
        {
            bool b = opIndex(i);
            result = dg(i, b);
            this[i] = b;
            if (result) { break; }
        }
        return result;
    }

    /** ditto */
    int opApply(scope int delegate(size_t, bool) dg) const
    {
        int result;
        for (size_t i = 0; i < len; ++i)
        {
            bool b = opIndex(i);
            result = dg(i, b);
            if (result) { break; }
        }
        return result;
    }

    ///
    unittest
    {
        static bool[] ba = [1,0,1];
        auto a = BitArrayN!3(ba);
        size_t i;
        foreach (immutable b; a[]) // TODO is `opSlice` the right thing?
        {
            switch (i)
            {
            case 0: assert(b == true); break;
            case 1: assert(b == false); break;
            case 2: assert(b == true); break;
            default: assert(0);
            }
            i++;
        }
        foreach (j, b; a)       // TODO is `opSlice` the right thing?
        {
            switch (j)
            {
            case 0: assert(b == true); break;
            case 1: assert(b == false); break;
            case 2: assert(b == true); break;
            default: assert(0);
            }
        }
    }

    /** Reverse block `Block`. */
    private pragma(inline, true) @property Block reverseBlock(in Block block)
        @safe pure nothrow @nogc
    {
        static if (Block.sizeof == 4)
        {
            return cast(uint)block.bitswap;
        }
        else static if (Block.sizeof == 8)
        {
            return (((cast(Block)((cast(uint)(block)).bitswap)) << 32) |
                    (cast(Block)((cast(uint)(block >> 32)).bitswap)));
        }
        else
        {
            return block;
        }
    }

    /** Reverses the bits of the $(D BitArrayN) in place. */
    @property BitArrayN reverse() out (result) { assert(result == this); }
    body
    {
        static if (length == blockCount * bitsPerBlock)
        {
            static if (blockCount == 1)
            {
                _blocks[0] = reverseBlock(_blocks[0]);
            }
            else static if (blockCount == 2)
            {
                const tmp = _blocks[1];
                _blocks[1] = reverseBlock(_blocks[0]);
                _blocks[0] = reverseBlock(tmp);
            }
            else static if (blockCount == 3)
            {
                const tmp = _blocks[2];
                _blocks[2] = reverseBlock(_blocks[0]);
                _blocks[1] = reverseBlock(_blocks[1]);
                _blocks[0] = reverseBlock(tmp);
            }
            else
            {
                size_t lo = 0;
                size_t hi = _blocks.length - 1;
                for (; lo < hi; ++lo, --hi)
                {
                    immutable t = reverseBlock(_blocks[lo]);
                    _blocks[lo] = reverseBlock(_blocks[hi]);
                    _blocks[hi] = t;
                }
                if (lo == hi)
                {
                    _blocks[lo] = reverseBlock(_blocks[lo]);
                }
            }
        }
        else
        {
            static if (length >= 2)
            {
                size_t lo = 0;
                size_t hi = len - 1;
                for (; lo < hi; ++lo, --hi)
                {
                    immutable t = this[lo];
                    this[lo] = this[hi];
                    this[hi] = t;
                }
            }
        }
        return this;
    }

    ///
    pure unittest
    {
        enum len = 64;
        static immutable bool[len] data = [0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0];
        auto b = BitArrayN!len(data);
        b.reverse();
        for (size_t i = 0; i < data.length; ++i)
        {
            assert(b[i] == data[len - 1 - i]);
        }
    }

    ///
    pure unittest
    {
        enum len = 64*2;
        static immutable bool[len] data = [0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0];
        auto b = BitArrayN!len(data);
        b.reverse();
        for (size_t i = 0; i < data.length; ++i)
        {
            assert(b[i] == data[len - 1 - i]);
        }
    }

    ///
    pure unittest
    {
        enum len = 64*3;
        static immutable bool[len] data = [0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                           0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0];
        auto b = BitArrayN!len(data);
        b.reverse();
        for (size_t i = 0; i < data.length; ++i)
        {
            assert(b[i] == data[len - 1 - i]);
        }
    }

    /** Sorts the $(D BitArrayN)'s elements. */
    @property BitArrayN sort()
        @safe pure nothrow @nogc
    out (result)
    {
        assert(result == this);
    }
    body
    {
        if (len >= 2)
        {
            size_t lo, hi;
            lo = 0;
            hi = len - 1;
            while (1)
            {
                while (1)
                {
                    if (lo >= hi)
                        goto Ldone;
                    if (this[lo] == true)
                        break;
                    lo++;
                }
                while (1)
                {
                    if (lo >= hi)
                        goto Ldone;
                    if (this[hi] == false)
                        break;
                    hi--;
                }
                this[lo] = false;
                this[hi] = true;
                lo++;
                hi--;
            }
        }
    Ldone:
        return this;
    }

    /* unittest */
    /*     { */
    /*         __gshared size_t x = 0b1100011000; */
    /*         __gshared BitArrayN ba = { 10, &x }; */
    /*         ba.sort(); */
    /*         for (size_t i = 0; i < 6; ++i) */
    /*             assert(ba[i] == false); */
    /*         for (size_t i = 6; i < 10; ++i) */
    /*             assert(ba[i] == true); */
    /*     } */


    /** Support for operators == and != for $(D BitArrayN). */
    bool opEquals(Block2)(in BitArrayN!(len, Block2) a2) const
        @trusted pure nothrow @nogc
        if (isUnsigned!Block2)
    {
        size_t i;

        if (this.length != a2.length) { return 0; } // not equal
        auto p1 = this.ptr;
        auto p2 = a2.ptr;
        auto n = this.length / bitsPerBlock;
        for (i = 0; i < n; ++i)
        {
            if (p1[i] != p2[i]) { return 0; } // not equal
        }

        n = this.length & (bitsPerBlock-1);
        size_t mask = (1 << n) - 1;
        //printf("i = %d, n = %d, mask = %x, %x, %x\n", i, n, mask, p1[i], p2[i]);
        return (mask == 0) || (p1[i] & mask) == (p2[i] & mask);
    }
    ///
    nothrow unittest
    {
        auto a = BitArrayN!(5, ubyte)([1,0,1,0,1]);
        auto b = BitArrayN!(5, ushort)([1,0,1,1,1]);
        auto c = BitArrayN!(5, uint)([1,0,1,0,1]);
        auto d = BitArrayN!(5, ulong)([1,1,1,1,1]);
        assert(a != b);
        assert(a == c);
        assert(a != d);
    }

    /** Supports comparison operators for $(D BitArrayN). */
    int opCmp(Block2)(in BitArrayN!(len, Block2) a2) const
        @trusted pure nothrow @nogc
        if (isUnsigned!Block2)
    {
        uint i;

        auto len = this.length;
        if (a2.length < len) { len = a2.length; }
        auto p1 = this.ptr;
        auto p2 = a2.ptr;
        auto n = len / bitsPerBlock;
        for (i = 0; i < n; ++i)
        {
            if (p1[i] != p2[i]) { break; } // not equal
        }
        for (size_t j = 0; j < len-i * bitsPerBlock; j++)
        {
            size_t mask = cast(size_t)(1 << j);
            auto c = (cast(long)(p1[i] & mask) - cast(long)(p2[i] & mask));
            if (c) { return c > 0 ? 1 : -1; }
        }
        return cast(int)this.length - cast(int)a2.length;
    }

    ///
    nothrow unittest
    {
        auto a = BitArrayN!(5, ubyte)([1,0,1,0,1]);
        auto b = BitArrayN!(5, ushort)([1,0,1,1,1]);
        auto c = BitArrayN!(5, uint)([1,0,1,0,1]);
        auto d = BitArrayN!(5, ulong)([1,1,1,1,1]);
        assert(a <  b);
        assert(a <= b);
        assert(a == c);
        assert(a <= c);
        assert(a >= c);
        assert(c < d);
    }

    /** Support for hashing for $(D BitArrayN). */
    extern(D) size_t toHash() const
        @trusted pure nothrow
    {
        size_t hash = 3557;
        auto n  = len / 8;
        for (size_t i = 0; i < n; ++i)
        {
            hash *= 3559;
            hash += (cast(byte*)this.ptr)[i];
        }
        for (size_t i = 8*n; i < len; ++i)
        {
            hash *= 3571;
            hash += this[i];
        }
        return hash;
    }

    /** Set this $(D BitArrayN) to the contents of $(D ba). */
    this(bool[] ba)
        @safe pure nothrow @nogc
    in
    {
        assert(length == ba.length);
    }
    body
    {
        foreach (immutable i, const b; ba)
        {
            this[i] = b;
        }
    }

    /** Set this $(D BitArrayN) to the contents of $(D ba). */
    this(const ref bool[len] ba)
        @safe pure nothrow @nogc
    {
        foreach (immutable i, const b; ba)
        {
            this[i] = b;
        }
    }

    bool opCast(T : bool)() const
        @safe pure nothrow @nogc
    {
        return !this.allZero;
    }

    /// construct from dynamic array
    @safe nothrow @nogc unittest
    {
        static bool[] ba = [1,0,1,0,1];
        auto a = BitArrayN!5(ba);
        assert(a);
        assert(!a.allZero);
    }
    /// ditto
    @safe nothrow @nogc unittest
    {
        static bool[] ba = [0,0,0];
        auto a = BitArrayN!3(ba);
        assert(!a);
        assert(a.allZero);
    }
    /// construct from static array
    @safe nothrow @nogc unittest
    {
        static bool[3] ba = [0,0,0];
        auto a = BitArrayN!3(ba);
        assert(!a);
        assert(a.allZero);
    }

    /** Check if this $(D BitArrayN) has only zeros (is empty). */
    bool allZero() const
        @safe pure nothrow @nogc
    {
        foreach (const block; _blocks) // TODO array operation
        {
            if (block != 0) { return false; }
        }
        return true;
    }

    /** Check if this $(D BitArrayN) has only ones in range [ $(d low), $(d high) [. */
    bool allOneBetween(size_t low, size_t high) const
        @safe pure nothrow @nogc
    in
    {
        assert(low + 1 <= len && high <= len);
    }
    body
    {
        foreach (immutable i; low .. high)
        {
            if (!this[i]) { return false; }
        }
        return true;
    }
    alias allSetBetween = allOneBetween;
    alias fullBetween = allOneBetween;

    static if (len >= 1)
    {
        /** Lazy range of the indices of set bits.

            Similar to: `std.bitmanip.bitsSet`

            See also: https://dlang.org/phobos/std_bitmanip.html#bitsSet
         */
        struct OneIndexes
        {
            @safe pure nothrow @nogc:

            this(BitArrayN store)
            {
                this._store = store;

                // pre-adjust front index. TODO make lazy and move to front
                while (_i < length && !_store[_i])
                {
                    ++_i;
                }

                // pre-adjust back index. TODO make lazy and move to front
                while (_j > 1 && !_store[_j])
                {
                    --_j;
                }
            }

            bool empty() const
            {
                return _i > _j;
            }

            Mod!len front() const
            {
                assert(!empty);     // TODO use enforce when it's @nogc
                return typeof(return)(_i);
            }

            Mod!len back() const
            {
                assert(!empty);     // TODO use enforce when it's @nogc
                return typeof(return)(_j);
            }

            void popFront()
            {
                assert(!empty);
                while (++_i <= _j)
                {
                    if (_store[_i]) { break; }
                }
            }

            void popBack()
            {
                assert(!empty);
                while (_i <= --_j)
                {
                    if (_store[_j]) { break; }
                }
            }

        private:
            BitArrayN _store;               // copy of store
            int _i = 0;                  // front index into `_store`
            int _j = _store.length - 1;  // back index into `_store`
        }

        /** Returns: a lazy range of the indices of set bits.
         */
        auto oneIndexes() const @safe pure nothrow @nogc
        {
            return OneIndexes(this);
        }

        alias bitsSet = OneIndexes;

        /** Get number of bits set in $(D this). */
        Mod!(len + 1) countOnes() const @safe pure nothrow @nogc
        {
            typeof(return) n = 0;
            foreach (immutable ix, const ref block; _blocks)
            {
                if (block != 0)
                {
                    import std.conv : to;
                    import core.bitop : popcnt;
                    static      if (block.sizeof == 1) { n += cast(uint)block.popcnt; } // TODO do we need to force `uint`-overload of `popcnt`?
                    else static if (block.sizeof == 2) { n += cast(uint)block.popcnt; } // TODO do we need to force `uint`-overload of `popcnt`?
                    else static if (block.sizeof == 4) { n += cast(uint)block.popcnt; } // TODO do we need to force `uint`-overload of `popcnt`?
                    else static if (block.sizeof == 8) n += (cast(ulong)((cast(uint)(block)).popcnt) +
                                                             cast(ulong)((cast(uint)(block >> 32)).popcnt));
                    else
                        assert(false, "Unsupported Block size " ~ to!string(block.sizeof));
                }
            }
            return typeof(return)(n);
        }
        alias count = countOnes;

        alias Q = Rational!ulong;

        /** Get number of bits set in $(D this). */
        pragma(inline, true) Q denseness(int depth = -1) const @safe pure nothrow @nogc
        {
            return Q(countOnes, length);
        }

        /** Get number of Bits Unset in $(D this). */
        pragma(inline, true) Q sparseness(int depth = -1) const @safe pure nothrow @nogc
        {
            return 1 - denseness(depth);
        }

        /** Check if this $(D BitArrayN) has only ones. */
        bool allOne() const @safe pure nothrow
        {
            const restCount = len % bitsPerBlock;
            const hasRest = restCount != 0;
            if (_blocks.length >= 1)
            {
                foreach (const ref block; _blocks[0 .. $ - hasRest])
                {
                    if (block != Block.max) { return false; }
                }
            }
            if (hasRest)
            {
                return _blocks[$ - 1] == 2^^restCount - 1;
            }
            else
            {
                return true;
            }
        }

        /** Find index (starting at `currIx`) of first bit that equals `value`.
            Returns: `true` if index was found (hit index is put into `nextIx`), `false` otherwise.
            TODO block-optimize for large BitSets
         */
        bool canFindIndexOf(ModUInt)(bool value,
                                     Mod!(len, ModUInt) currIx,
                                     out Mod!(len, ModUInt) nextIx) const @safe pure nothrow @nogc
            if (isUnsigned!ModUInt)
        {
            if (currIx >= length) { return false; }
            bool hit = false;
            foreach (immutable ix_; cast(uint)currIx .. cast(uint)length)
            {
                const bool bit = this[ix_];
                if (bit == value)
                {
                    nextIx = typeof(nextIx)(ix_);
                    hit = true;
                    break;
                }
            }
            return hit;
        }

    }

    /**
     * Map the $(D BitArrayN) onto $(D v), with $(D numbits) being the number of bits
     * in the array. Does not copy the data.
     *
     * This is the inverse of $(D opCast).
     */
    /* void init(void[] v, size_t numbits) in { */
    /*     assert(numbits <= v.length * 8); */
    /*     assert((v.length & 3) == 0); // must be whole bytes */
    /* } body { */
    /*     _blocks[] = cast(size_t*)v.ptr[0..v.length]; */
    /* } */

    /** Convert to $(D void[]). */
    void[] opCast(T : void[])()
    {
        return cast(void[])ptr[0 .. dim];
    }

    /** Convert to $(D size_t[]). */
    size_t[] opCast(T : size_t[])()
    {
        return ptr[0 .. dim];
    }
    ///
    nothrow unittest
    {
        static bool[] ba = [1,0,1,0,1];
        auto a = BitArrayN!5(ba);
        void[] v = cast(void[])a;
        assert(v.length == a.dim * size_t.sizeof);
    }

    /** Support for unary operator ~ for $(D BitArrayN). */
    BitArrayN opCom() const
    {
        BitArrayN result;
        for (size_t i = 0; i < dim; ++i)
            result.ptr[i] = ~this.ptr[i];
        immutable rem = len & (bitsPerBlock-1); // number of rest bits in last block
        if (rem < bitsPerBlock) // rest bits in last block
            // make remaining bits zero in last block
            result.ptr[dim - 1] &= ~(~(cast(Block)0) << rem);
        return result;
    }

    /** Support for binary operator & for $(D BitArrayN). */
    BitArrayN opAnd(in BitArrayN e2) const
    {
        BitArrayN result;
        result._blocks[] = this._blocks[] & e2._blocks[];
        return result;
    }
    ///
    nothrow unittest
    {
        const a = BitArrayN!5([1,0,1,0,1]);
        auto b = BitArrayN!5([1,0,1,1,0]);
        const c = a & b;
        auto d = BitArrayN!5([1,0,1,0,0]);
        assert(c == d);
    }

    /** Support for binary operator | for $(D BitArrayN). */
    BitArrayN opOr(in BitArrayN e2) const
    {
        BitArrayN result;
        result._blocks[] = this._blocks[] | e2._blocks[];
        return result;
    }
    ///
    nothrow unittest
    {
        const a = BitArrayN!5([1,0,1,0,1]);
        auto b = BitArrayN!5([1,0,1,1,0]);
        const c = a | b;
        auto d = BitArrayN!5([1,0,1,1,1]);
        assert(c == d);
    }

    /** Support for binary operator ^ for $(D BitArrayN). */
    BitArrayN opXor(in BitArrayN e2) const
    {
        BitArrayN result;
        result._blocks[] = this._blocks[] ^ e2._blocks[];
        return result;
    }
    ///
    nothrow unittest
    {
        const a = BitArrayN!5([1,0,1,0,1]);
        auto b = BitArrayN!5([1,0,1,1,0]);
        const c = a ^ b;
        auto d = BitArrayN!5([0,0,0,1,1]);
        assert(c == d);
    }

    /** Support for binary operator - for $(D BitArrayN).
     *
     * $(D a - b) for $(D BitArrayN) means the same thing as $(D a &amp; ~b).
     */
    BitArrayN opSub(in BitArrayN e2) const
    {
        BitArrayN result;
        result._blocks[] = this._blocks[] & ~e2._blocks[];
        return result;
    }
    ///
    nothrow unittest
    {
        const a = BitArrayN!5([1,0,1,0,1]);
        auto b = BitArrayN!5([1,0,1,1,0]);
        const c = a - b;
        auto d = BitArrayN!5([0,0,0,0,1]);
        assert(c == d);
    }

    /** Support for operator &= for $(D BitArrayN).
     */
    BitArrayN opAndAssign(in BitArrayN e2)
    {
        _blocks[] &= e2._blocks[];
        return this;
    }
    ///
    nothrow unittest
    {
        auto a = BitArrayN!5([1,0,1,0,1]);
        const b = BitArrayN!5([1,0,1,1,0]);
        a &= b;
        const c = BitArrayN!5([1,0,1,0,0]);
        assert(a == c);
    }

    /** Support for operator |= for $(D BitArrayN).
     */
    BitArrayN opOrAssign(in BitArrayN e2)
    {
        _blocks[] |= e2._blocks[];
        return this;
    }
    ///
    nothrow unittest
    {
        auto a = BitArrayN!5([1,0,1,0,1]);
        const b = BitArrayN!5([1,0,1,1,0]);
        a |= b;
        const c = BitArrayN!5([1,0,1,1,1]);
        assert(a == c);
    }

    /** Support for operator ^= for $(D BitArrayN).
     */
    BitArrayN opXorAssign(in BitArrayN e2)
    {
        _blocks[] ^= e2._blocks[];
        return this;
    }
    ///
    nothrow unittest
    {
        auto a = BitArrayN!5([1,0,1,0,1]);
        const b = BitArrayN!5([1,0,1,1,0]);
        a ^= b;
        const c = BitArrayN!5([0,0,0,1,1]);
        assert(a == c);
    }

    /** Support for operator -= for $(D BitArrayN).
     *
     * $(D a -= b) for $(D BitArrayN) means the same thing as $(D a &amp;= ~b).
     */
    BitArrayN opSubAssign(in BitArrayN e2)
    {
        _blocks[] &= ~e2._blocks[];
        return this;
    }
    ///
    nothrow unittest
    {
        auto a = BitArrayN!5([1,0,1,0,1]);
        const b = BitArrayN!5([1,0,1,1,0]);
        a -= b;
        const c = BitArrayN!5([0,0,0,0,1]);
        assert(a == c);
    }

    /** Return a string representation of this BitArrayN.
     *
     * Two format specifiers are supported:
     * $(LI $(B %s) which prints the bits as an array, and)
     * $(LI $(B %b) which prints the bits as 8-bit byte packets)
     * separated with an underscore.
     */
    void toString(scope void delegate(const(char)[]) sink,
                  FormatSpec!char fmt) const
    {
        switch(fmt.spec)
        {
        case 'b':
            return formatBitString(sink);
        case 's':
            return formatBitSet(sink);
        default:
            throw new Exception("Unknown format specifier: %" ~ fmt.spec);
        }
    }
    ///
    unittest
    {
        const b = BitArrayN!16(([0, 0, 0, 0, 1, 1, 1, 1,
                             0, 0, 0, 0, 1, 1, 1, 1]));
        const s1 = format("%s", b);
        assert(s1 == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");

        const s2 = format("%b", b);
        assert(s2 == "00001111_00001111");
    }

    private void formatBitString(scope void delegate(const(char)[]) sink) const
    {
        import std.range : put;

        static if (length)
        {
            const leftover = len % 8;
            foreach (immutable ix; 0 .. leftover)
            {
                const bit = this[ix];
                const char[1] res = cast(char)(bit + '0');
                sink.put(res[]);
            }

            if (leftover && len > 8) { sink.put("_"); } // separator

            size_t cnt;
            foreach (immutable ix; leftover .. len)
            {
                const bit = this[ix];
                const char[1] res = cast(char)(bit + '0');
                sink.put(res[]);
                if (++cnt == 8 && ix != len - 1)
                {
                    sink.put("_");  // separator
                    cnt = 0;
                }
            }
        }
    }

    private void formatBitSet(scope void delegate(const(char)[]) sink) const
    {
        sink("[");
        foreach (immutable ix; 0 .. len)
        {
            const bit = this[ix];
            const char[1] res = cast(char)(bit + '0');
            sink(res[]);
            if (ix+1 < len) { sink(", "); } // separator
        }
        sink("]");
    }
}

/// compile-time
@safe pure nothrow unittest
{
    import nesses: denseness, sparseness;
    import rational : Rational;
    alias Q = Rational!ulong;

    enum m = 256, n = 256;

    static assert(BitArrayN!m.init ==
                  BitArrayN!m.init);
    static assert(BitArrayN!m.init.denseness == Q(0, m));
}

/// run-time
@safe pure nothrow @nogc unittest
{
    import std.algorithm : equal;
    import nesses: denseness;
    import rational : Rational;

    alias Q = Rational!ulong;
    enum m = 256;

    BitArrayN!m b0;

    import modulo : Mod;
    static assert(is(typeof(b0.oneIndexes.front()) == Mod!m));

    b0[1] = 1;
    b0[2] = 1;

    b0[m/2 - 11] = 1;
    b0[m/2 - 1] = 1;
    b0[m/2] = 1;
    b0[m/2 + 1] = 1;
    b0[m/2 + 11] = 1;

    b0[m - 3] = 1;
    b0[m - 2] = 1;

    assert(b0.oneIndexes.equal([1, 2,
                                m/2 - 11, m/2 - 1, m/2, m/2 + 1, m/2 + 11,
                                m - 3,
                                m - 2].s[]));
    assert(b0.countOnes == 9);
    assert(b0.denseness == Q(9, m));
}

/// run-time
@safe pure nothrow @nogc unittest
{
    import std.algorithm : equal;
    import nesses: denseness;
    import rational : Rational;

    alias Q = Rational!ulong;
    enum m = 256;

    BitArrayN!m b0;

    import modulo : Mod;
    static assert(is(typeof(b0.oneIndexes.front()) == Mod!m));

    b0[0] = 1;
    b0[1] = 1;
    b0[m/2 - 11] = 1;
    b0[m/2 - 1] = 1;
    b0[m/2] = 1;
    b0[m/2 + 1] = 1;
    b0[m/2 + 11] = 1;
    b0[m - 2] = 1;
    b0[m - 1] = 1;

    assert(b0.oneIndexes.equal([0, 1,
                                m/2 - 11, m/2 - 1, m/2, m/2 + 1, m/2 + 11,
                                m - 2,
                                m - 1].s[]));
    assert(b0.countOnes == 9);
    assert(b0.denseness == Q(9, m));
}

/// run-time
@safe pure nothrow @nogc unittest
{
    import std.algorithm : equal;
    import nesses: denseness;
    import rational : Rational;
    alias Q = Rational!ulong;

    enum m = 256, n = 256;

    BitArrayN!m[n] b1;
    b1[0][0] = 1;
    assert(b1.denseness == Q(1, m*n));

    BitArrayN!m[n][n] b2;
    b2[0][0][0] = 1;
    b2[0][0][1] = 1;
    b2[0][0][4] = 1;
    assert(b2.denseness == Q(3, m*n*n));
}

/// ditto
@safe pure nothrow @nogc unittest
{
    import std.traits : isIterable;
    static assert(isIterable!(BitArrayN!256));
}

/// test ubyte access
@safe pure nothrow @nogc unittest
{
    auto b8 = BitArrayN!(8, ubyte)();
    b8[0] = 1;
    b8[1] = 1;
    b8[3] = 1;
    b8[6] = 1;

    assert(b8.ubytes == [64 + 8 + 2 + 1].s[]);

    alias Ix = b8.Index;
    Ix nextIx;

    assert(b8.canFindIndexOf(true, Ix(0), nextIx));
    assert(nextIx == 0);

    assert(b8.canFindIndexOf(true, Ix(1), nextIx));
    assert(nextIx == 1);

    assert(b8.canFindIndexOf(true, Ix(2), nextIx));
    assert(nextIx == 3);

    assert(b8.canFindIndexOf(true, Ix(3), nextIx));
    assert(nextIx == 3);

    assert(b8.canFindIndexOf(true, Ix(4), nextIx));
    assert(nextIx == 6);

    assert(!b8.canFindIndexOf(true, Ix(7), nextIx));
}

/// test all zero and all one predicates
@safe pure nothrow @nogc unittest
{
    static void test(size_t restCount)()
    {
        enum n = 8*size_t.sizeof + restCount;

        auto bs = BitArrayN!(n, size_t)();

        assert(bs.allZero);
        assert(!bs.allOne);

        foreach (immutable i; 0 .. n - 1)
        {
            bs[i] = true;
            assert(!bs.allZero);
            assert(!bs.allOne);
        }
        bs[n - 1] = true;

        assert(bs.allOne);
    }
    test!0;
    test!1;
    test!2;
    test!37;
    test!62;
    test!63;
}

/// ditto
unittest
{
    import std.format : format;

    const b0_ = BitArrayN!0([]);
    const b0 = b0_;
    assert(format("%s", b0) == "[]");
    assert(format("%b", b0) is null);

    const b1_ = BitArrayN!1([1]);
    const b1 = b1_;
    assert(format("%s", b1) == "[1]");
    assert(format("%b", b1) == "1");

    const b4 = BitArrayN!4([0, 0, 0, 0]);
    assert(format("%b", b4) == "0000");

    const b8 = BitArrayN!8([0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b8) == "[0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b8) == "00001111");

    const b16 = BitArrayN!16([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b16) == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b16) == "00001111_00001111");

    const b9 = BitArrayN!9([1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b9) == "1_00001111");

    const b17 = BitArrayN!17([1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b17) == "1_00001111_00001111");
}

/// test range
@safe pure nothrow unittest
{
    static testRange(Block)()
    {
        BitArrayN!(6, Block) bs = [false, 1, 0, 0, true, 0];
        bs.put(3, true);

        import std.algorithm : equal;

        assert(bs[0] == false);
        assert(bs[1] == true);
        assert(bs[2] == false);
        assert(bs[3] == true);
        assert(bs[4] == true);
        assert(bs[5] == false);

        assert(bs.at!0 == false);
        assert(bs.at!1 == true);
        assert(bs.at!2 == false);
        assert(bs.at!3 == true);
        assert(bs.at!4 == true);
        assert(bs.at!5 == false);

        // test slicing
        assert(bs[].equal([0, 1, 0, 1, 1, 0].s[]));
        assert(bs[1 .. 4].equal([1, 0, 1].s[]));

        auto rs = bs[1 .. 6 - 1]; // TODO Use opDollar
        assert(rs.length == 4);
        assert(rs.front == 1);
        assert(rs.back == 1);

        rs.popFront();
        assert(rs.front == 0);
        assert(rs.back == 1);

        rs.popBack();
        assert(rs.front == 1);
        assert(rs.back == 1);

        rs.popFront();
        rs.popBack();

        assert(rs.length == 0);
        assert(rs.empty);
    }

    import std.meta : AliasSeq;
    foreach (Block; AliasSeq!(ubyte, ushort, uint, ulong, size_t))
    {
        testRange!Block;
    }
}
