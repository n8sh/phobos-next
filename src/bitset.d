#!/usr/bin/env rdmd-dev-module

/** Fixed-Sized Bit-Array.
    Copyright: Per Nordlöw 2014-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
 */
module bitset;

version(unittest)
{
    import dbg;
}

/** BitSet, a statically sized `BitArray`.

    TODO opSlice, opSliceAssign
    TODO Infer `Block` from `len` as is done for `Bound` and `Mod`.
 */
struct BitSet(size_t len, Block = size_t)
{
    import rational : Rational;
    import std.format : FormatSpec, format;
    import core.bitop : bt, bts, btr, bitswap;
    import modulo : Mod;

    import std.traits : isUnsigned;
    static assert(isUnsigned!Block, "Block must be a builtin unsigned integer");

    /** Number of bits per `Block`. */
    enum bitsPerBlock = 8*Block.sizeof;
    /** Number of `Block`s. */
    enum blockCount = (len + (bitsPerBlock-1)) / bitsPerBlock;

    /** Data stored as `Block`s. */
    private Block[blockCount] _data;

    @property inout (Block*) ptr() inout { return _data.ptr; }

    /** Reset all bits (to zero). */
    void reset() @safe nothrow { _data[] = 0; }

    /** Gets the amount of native words backing this $(D BitSet). */
    @property static size_t dim() @safe @nogc pure nothrow { return blockCount; }

    /** Number of bits in the $(D BitSet). */
    enum length = len;

    BitSet opAssign(BitSet rhs) @safe nothrow { this._data = rhs._data; return this; }

    /** Gets the $(D i)'th bit in the $(D BitSet). */
    pragma(inline) bool opIndex(size_t i) const @trusted pure nothrow
    in
    {
        assert(i < len);        // TODO nothrow or not?
    }
    body
    {
        // Andrei: review for @@@64-bit@@@
        static if (is(Block == size_t))
        {
            return cast(bool)bt(ptr, i);
        }
        else
        {
            import bitop_ex : bt;
            return bt(*ptr, i);
        }
    }

    /** Gets the $(D i)'th bit in the $(D BitSet). No range checking needed. */
    static if (len >= 1)
    {
        /** Get the $(D i)'th bit in the $(D BitSet).

            Avoids range-checking because `i` of type is bound to (0 .. len-1).
        */
        pragma(inline) bool opIndex(Mod!len i) const @trusted pure nothrow
        {
            import bitop_ex : bt;
            return cast(bool)bt(ptr, cast(size_t)i);
        }

        /** Get the $(D i)'th bit in the $(D BitSet).
            Statically verifies that i is < BitSet length.
        */
        pragma(inline) bool at(size_t i)() const @trusted pure nothrow
        in
        {
            static assert(i < len);
        }
        body
        {
            return this[i];
        }
    }

    /** Puts the $(D i)'th bit in the $(D BitSet) to $(D b). */
    pragma(inline) auto ref put()(size_t i, bool b) @trusted pure nothrow
    {
        this[i] = b;
        return this;
    }

    ///
    @safe pure nothrow unittest
    {
        BitSet!6 bs = [0, 1, 0, 0, 1, 0];
        bs.put(3, true);

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
    }

    /** Sets the $(D i)'th bit in the $(D BitSet). */
    import std.traits: isIntegral;
    pragma(inline) bool opIndexAssign(Index)(bool b, Index i) @trusted pure nothrow
        if (isIntegral!Index)
    in
    {
        // import std.traits: isMutable;
        // See also: http://stackoverflow.com/questions/19906516/static-parameter-function-specialization-in-d
        /* static if (!isMutable!Index) { */
        /*     import std.conv: to; */
        /*     static assert(i < len, */
        /*                   "Index " ~ to!string(i) ~ " must be smaller than BitSet length " ~  to!string(len)); */
        /* } */
        assert(i < len);
    }
    body
    {
        static if (is(Block == ulong) && // TODO remove this
                   is(Block == size_t)) // TODO remove this
        {
            b ? bts(ptr, i) : btr(ptr, i);
        }
        else
        {
            import bitop_ex : bts, btr;
            b ? bts(ptr, i) : btr(ptr, i);
        }
        return b;
    }

    static if (len >= 1)
    {
        /** Sets the $(D i)'th bit in the $(D BitSet). No range checking needed. */
        pragma(inline) bool opIndexAssign(bool b, Mod!len i) @trusted pure nothrow
        {
            static if (is(Block == ulong) && // TODO remove this
                       is(Block == size_t)) // TODO remove this
            {
                b ? bts(ptr, cast(size_t)i) : btr(ptr, cast(size_t)i);
            }
            else
            {
                import bitop_ex : bts, btr;
                b ? bts(ptr, cast(size_t)i) : btr(ptr, cast(size_t)i);
            }
            return b;
        }
    }

    ///
    @safe pure nothrow @nogc unittest
    {
        BitSet!2 bs;
        bs[0] = true;
        assert(bs[0]);
    }

    /** Duplicates the $(D BitSet) and its contents. */
    @property BitSet dup() const @safe @nogc pure nothrow { return this; }

    /** Support for $(D foreach) loops for $(D BitSet). */
    int opApply(scope int delegate(ref bool) dg)
    {
        int result;

        for (size_t i = 0; i < len; i++)
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
        for (size_t i = 0; i < len; i++)
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
        for (size_t i = 0; i < len; i++)
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
        for (size_t i = 0; i < len; i++)
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
        debug(bitset) printf("BitSet.opApply unittest\n");
        static bool[] ba = [1,0,1];
        auto a = BitSet!3(ba);
        size_t i;
        foreach (b; a)
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
        foreach (j, b; a)
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

    pragma(inline) @property Block reverseBlock(in Block block)
    {
        static if (Block.sizeof == 4)
        {
            return cast(uint)block.bitswap;
        }
        else static if (Block.sizeof == 8)
        {
            return (((cast(Block)((cast(uint)(block)).bitswap)) << 32) +
                    (cast(Block)((cast(uint)(block >> 32)).bitswap)));
        }
        else
        {
            return block;
        }
    }

    /** Reverses the bits of the $(D BitSet) in place. */
    @property BitSet reverse() out (result) { assert(result == this); }
    body
    {
        static if (length == blockCount * bitsPerBlock)
        {
            static if (blockCount == 1)
            {
                _data[0] = reverseBlock(_data[0]);
            }
            else static if (blockCount == 2)
            {
                const tmp = _data[1];
                _data[1] = reverseBlock(_data[0]);
                _data[0] = reverseBlock(tmp);
            }
            else static if (blockCount == 3)
            {
                const tmp = _data[2];
                _data[2] = reverseBlock(_data[0]);
                _data[1] = reverseBlock(_data[1]);
                _data[0] = reverseBlock(tmp);
            }
            else
            {
                size_t lo = 0;
                size_t hi = _data.length - 1;
                for (; lo < hi; lo++, hi--)
                {
                    immutable t = reverseBlock(_data[lo]);
                    _data[lo] = reverseBlock(_data[hi]);
                    _data[hi] = t;
                }
                if (lo == hi)
                {
                    _data[lo] = reverseBlock(_data[lo]);
                }
            }
        }
        else
        {
            static if (length >= 2)
            {
                size_t lo = 0;
                size_t hi = len - 1;
                for (; lo < hi; lo++, hi--)
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
    unittest
    {
        enum len = 64;
        static bool[len] data = [0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0];
        auto b = BitSet!len(data);
        b.reverse;
        for (size_t i = 0; i < data.length; i++)
        {
            assert(b[i] == data[len - 1 - i]);
        }
    }

    ///
    unittest
    {
        enum len = 64*2;
        static bool[len] data = [0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0];
        auto b = BitSet!len(data);
        b.reverse;
        for (size_t i = 0; i < data.length; i++)
        {
            assert(b[i] == data[len - 1 - i]);
        }
    }

    ///
    unittest
    {
        enum len = 64*3;
        static bool[len] data = [0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0,
                                 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0, 0,1,1,0,1,0,1,0];
        auto b = BitSet!len(data);
        b.reverse;
        for (size_t i = 0; i < data.length; i++)
        {
            assert(b[i] == data[len - 1 - i]);
        }
    }

    /** Sorts the $(D BitSet)'s elements. */
    @property BitSet sort()
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
    /*         debug(bitset) printf("BitSet.sort.unittest\n"); */
    /*         __gshared size_t x = 0b1100011000; */
    /*         __gshared BitSet ba = { 10, &x }; */
    /*         ba.sort(); */
    /*         for (size_t i = 0; i < 6; i++) */
    /*             assert(ba[i] == false); */
    /*         for (size_t i = 6; i < 10; i++) */
    /*             assert(ba[i] == true); */
    /*     } */


    /** Support for operators == and != for $(D BitSet). */
    const bool opEquals(in BitSet a2)
    {
        size_t i;

        if (this.length != a2.length)
            return 0;                // not equal
        auto p1 = this.ptr;
        auto p2 = a2.ptr;
        auto n = this.length / bitsPerBlock;
        for (i = 0; i < n; i++)
        {
            if (p1[i] != p2[i])
                return 0;                // not equal
        }

        n = this.length & (bitsPerBlock-1);
        size_t mask = (1 << n) - 1;
        //printf("i = %d, n = %d, mask = %x, %x, %x\n", i, n, mask, p1[i], p2[i]);
        return (mask == 0) || (p1[i] & mask) == (p2[i] & mask);
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opEquals unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,1]);
        auto c = BitSet!5([1,0,1,0,1]);
        assert(a != b);
        assert(a == c);
    }

    /** Supports comparison operators for $(D BitSet). */
    int opCmp(in BitSet a2) const
    {
        uint i;

        auto len = this.length;
        if (a2.length < len)
            len = a2.length;
        auto p1 = this.ptr;
        auto p2 = a2.ptr;
        auto n = len / bitsPerBlock;
        for (i = 0; i < n; i++)
        {
            if (p1[i] != p2[i])
                break;                // not equal
        }
        for (size_t j = 0; j < len-i * bitsPerBlock; j++)
        {
            size_t mask = cast(size_t)(1 << j);
            auto c = (cast(long)(p1[i] & mask) - cast(long)(p2[i] & mask));
            if (c)
                return c > 0 ? 1 : -1;
        }
        return cast(int)this.length - cast(int)a2.length;
    }

    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opCmp unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,1]);
        auto c = BitSet!5([1,0,1,0,1]);
        assert(a <  b);
        assert(a <= b);
        assert(a == c);
        assert(a <= c);
        assert(a >= c);
    }

    /** Support for hashing for $(D BitSet). */
    extern(D) size_t toHash() const @trusted pure nothrow
    {
        size_t hash = 3557;
        auto n  = len / 8;
        for (size_t i = 0; i < n; i++)
        {
            hash *= 3559;
            hash += (cast(byte*)this.ptr)[i];
        }
        for (size_t i = 8*n; i < len; i++)
        {
            hash *= 3571;
            import bitop_ex : bt;
            hash += bt(this.ptr, i);
        }
        return hash;
    }

    /** Set this $(D BitSet) to the contents of $(D ba). */
    this(bool[] ba) in { assert(length == ba.length); }
    body
    {
        foreach (const i, const b; ba)
        {
            this[i] = b;
        }
    }

    bool opCast(T : bool)() const @safe @nogc pure nothrow { return !this.empty ; }

    ///
    @safe nothrow @nogc unittest
    {
        static bool[] ba = [1,0,1,0,1];
        auto a = BitSet!5(ba);
        assert(a);
        assert(!a.empty);
    }
    ///
    @safe nothrow @nogc unittest
    {
        static bool[] ba = [0,0,0];
        auto a = BitSet!3(ba);
        assert(!a);
        assert(a.empty);
    }

    /** Check if this $(D BitSet) has only zeros. */
    bool allZero() const @safe @nogc pure nothrow
    {
        foreach (const block; _data)
        {
            if (block != 0)
            {
                return false;
            }
        }
        return true;
    }
    alias empty = allZero;

    /** Check if this $(D BitSet) has only ones in range [ $(d low), $(d high) [. */
    bool allOneBetween(size_t low, size_t high)
        const @safe @nogc pure nothrow
    in
    {
        assert(low + 1 <= len && high <= len);
    }
    body
    {
        foreach (const i; low .. high)
        {
            if (!this[i]) { return false; }
        }
        return true;
    }
    alias allSetBetween = allOneBetween;
    alias fullBetween = allOneBetween;

    /** Get indexes of all bits set.
        TODO when opSlice is implemented use: return this[].filter!(bit => bit);
     */
    size_t[] oneIndexes() const @safe pure nothrow
    {
        typeof(return) ixs;
        foreach (const ix; 0 .. length)
        {
            if (this[ix])
            {
                ixs ~= ix;
            }
        }
        return ixs;
    }
    alias indexesOfOnes = oneIndexes;

    /** Get number of bits set in $(D this). */
    static if (len >= 1)
    {
        Mod!(len + 1) countOnes() const @safe @nogc pure nothrow
        {
            ulong n = 0;
            foreach (const ix, const block; _data)
            {
                if (block != 0)
                {
                    import std.conv : to;
                    import core.bitop : popcnt;
                    static      if (block.sizeof == 1) n += cast(uint)block.popcnt;
                    else static if (block.sizeof == 2) n += cast(uint)block.popcnt;
                    else static if (block.sizeof == 4) n += cast(uint)block.popcnt;
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
        pragma(inline) Q denseness(int depth = -1) const @safe @nogc pure nothrow
        {
            return Q(countOnes, length);
        }

        /** Get number of Bits Unset in $(D this). */
        pragma(inline) Q sparseness(int depth = -1) const @safe @nogc pure nothrow
        {
            return 1 - denseness(depth);
        }
    }

    /**
     * Map the $(D BitSet) onto $(D v), with $(D numbits) being the number of bits
     * in the array. Does not copy the data.
     *
     * This is the inverse of $(D opCast).
     */
    /* void init(void[] v, size_t numbits) in { */
    /*     assert(numbits <= v.length * 8); */
    /*     assert((v.length & 3) == 0); // must be whole bytes */
    /* } body { */
    /*     _data[] = cast(size_t*)v.ptr[0..v.length]; */
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
        debug(bitset) printf("BitSet.opCast unittest\n");
        static bool[] ba = [1,0,1,0,1];
        auto a = BitSet!5(ba);
        void[] v = cast(void[])a;
        assert(v.length == a.dim * size_t.sizeof);
    }

    /** Support for unary operator ~ for $(D BitSet). */
    BitSet opCom() const
    {
        BitSet result;
        for (size_t i = 0; i < dim; i++)
            result.ptr[i] = ~this.ptr[i];
        immutable rem = len & (bitsPerBlock-1); // number of rest bits in last block
        if (rem < bitsPerBlock) // rest bits in last block
            // make remaining bits zero in last block
            result.ptr[dim - 1] &= ~(~(cast(Block)0) << rem);
        return result;
    }

    /** Support for binary operator & for $(D BitSet). */
    BitSet opAnd(in BitSet e2) const
    {
        BitSet result;
        for (size_t i = 0; i < dim; i++)
            result.ptr[i] = this.ptr[i] & e2.ptr[i];
        return result;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opAnd unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        auto c = a & b;
        auto d = BitSet!5([1,0,1,0,0]);
        assert(c == d);
    }

    /** Support for binary operator | for $(D BitSet). */
    BitSet opOr(in BitSet e2) const
    {
        BitSet result;
        for (size_t i = 0; i < dim; i++)
            result.ptr[i] = this.ptr[i] | e2.ptr[i];
        return result;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opOr unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        auto c = a | b;
        auto d = BitSet!5([1,0,1,1,1]);
        assert(c == d);
    }

    /** Support for binary operator ^ for $(D BitSet). */
    BitSet opXor(in BitSet e2) const
    {
        BitSet result;
        for (size_t i = 0; i < dim; i++)
            result.ptr[i] = this.ptr[i] ^ e2.ptr[i];
        return result;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opXor unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        auto c = a ^ b;
        auto d = BitSet!5([0,0,0,1,1]);
        assert(c == d);
    }

    /** Support for binary operator - for $(D BitSet).
     *
     * $(D a - b) for $(D BitSet) means the same thing as $(D a &amp; ~b).
     */
    BitSet opSub(in BitSet e2) const
    {
        BitSet result;
        for (size_t i = 0; i < dim; i++)
            result.ptr[i] = this.ptr[i] & ~e2.ptr[i];
        return result;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opSub unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        auto c = a - b;
        auto d = BitSet!5([0,0,0,0,1]);
        assert(c == d);
    }

    /** Support for operator &= for $(D BitSet).
     */
    BitSet opAndAssign(in BitSet e2)
    {
        for (size_t i = 0; i < dim; i++)
            ptr[i] &= e2.ptr[i];
        return this;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opAndAssign unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        a &= b;
        auto c = BitSet!5([1,0,1,0,0]);
        assert(a == c);
    }

    /** Support for operator |= for $(D BitSet).
     */
    BitSet opOrAssign(in BitSet e2)
    {
        for (size_t i = 0; i < dim; i++)
            ptr[i] |= e2.ptr[i];
        return this;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opOrAssign unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        a |= b;
        auto c = BitSet!5([1,0,1,1,1]);
        assert(a == c);
    }

    /** Support for operator ^= for $(D BitSet).
     */
    BitSet opXorAssign(in BitSet e2)
    {
        for (size_t i = 0; i < dim; i++)
            ptr[i] ^= e2.ptr[i];
        return this;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opXorAssign unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        a ^= b;
        auto c = BitSet!5([0,0,0,1,1]);
        assert(a == c);
    }

    /** Support for operator -= for $(D BitSet).
     *
     * $(D a -= b) for $(D BitSet) means the same thing as $(D a &amp;= ~b).
     */
    BitSet opSubAssign(in BitSet e2)
    body
    {
        for (size_t i = 0; i < dim; i++)
            ptr[i] &= ~e2.ptr[i];
        return this;
    }
    ///
    nothrow unittest
    {
        debug(bitset) printf("BitSet.opSubAssign unittest\n");
        auto a = BitSet!5([1,0,1,0,1]);
        auto b = BitSet!5([1,0,1,1,0]);
        a -= b;
        auto c = BitSet!5([0,0,0,0,1]);
        assert(a == c);
    }

    /** Return a string representation of this BitSet.
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
        auto b = BitSet!16(([0, 0, 0, 0, 1, 1, 1, 1,
                             0, 0, 0, 0, 1, 1, 1, 1]));
        auto s1 = format("%s", b);
        assert(s1 == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");

        auto s2 = format("%b", b);
        assert(s2 == "00001111_00001111");
    }

    private void formatBitString(scope void delegate(const(char)[]) sink) const
    {
        import std.range : put;

        static if (length)
        {
            const leftover = len % 8;
            foreach (const ix; 0 .. leftover)
            {
                const bit = this[ix];
                const char[1] res = cast(char)(bit + '0');
                sink.put(res[]);
            }

            if (leftover && len > 8) { sink.put("_"); } // separator

            size_t cnt;
            foreach (const ix; leftover .. len)
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
        foreach (const ix; 0 .. len)
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

    static assert(BitSet!m.init ==
                  BitSet!m.init);
    static assert(BitSet!m.init.denseness == Q(0, m));
}

/// run-time
@safe pure nothrow unittest
{
    import nesses: denseness, sparseness;
    import rational : Rational;
    alias Q = Rational!ulong;

    enum m = 256, n = 256;

    BitSet!m b0;
    b0[0] = 1;
    b0[m/2] = 1;
    b0[m - 1] = 1;
    assert(b0.oneIndexes == [0, m/2, m - 1]);
    assert(b0.countOnes == 3);
    assert(b0.denseness == Q(3, m));

    BitSet!m[n] b1;
    b1[0][0] = 1;
    assert(b1.denseness == Q(1, m*n));

    BitSet!m[n][n] b2;
    b2[0][0][0] = 1;
    b2[0][0][1] = 1;
    b2[0][0][4] = 1;
    assert(b2.denseness == Q(3, m*n*n));
}

/// ditto
@safe pure nothrow @nogc unittest
{
    import std.traits : isIterable;
    static assert(isIterable!(BitSet!256));
}

@safe pure nothrow @nogc unittest
{
    import std.traits : isIterable;
    import std.meta : AliasSeq;
    foreach (Block; AliasSeq!(ubyte, ushort, uint))
    {
        BitSet!(256, Block) x;
    }
}

/// ditto
unittest
{
    import std.format : format;

    const b0 = BitSet!0([]);
    assert(format("%s", b0) == "[]");
    assert(format("%b", b0) is null);

    const b1 = BitSet!1([1]);
    assert(format("%s", b1) == "[1]");
    assert(format("%b", b1) == "1");

    const b4 = BitSet!4([0, 0, 0, 0]);
    assert(format("%b", b4) == "0000");

    const b8 = BitSet!8([0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b8) == "[0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b8) == "00001111");

    const b16 = BitSet!16([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b16) == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b16) == "00001111_00001111");

    const b9 = BitSet!9([1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b9) == "1_00001111");

    const b17 = BitSet!17([1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b17) == "1_00001111_00001111");
}
