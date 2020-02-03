module nxt.sso_string;

/** Small-size-optimized (SSO) variant of `string`.
 *
 * Storage is placed on the stack if the number of `char`s is <=
 * `smallCapacity`, otherwise as a normal `string`. If the large case the stored
 * `string` will be allocated on the GC-heap if the `SSOString` is constructed
 * from a non-`string` (non-`immutable`) parameter.
 *
 * Because `SSOString` doesn't have a destructor it can safely allocate using a
 * GC-backed region allocator without relying on a GC finalizer.
 *
 * In order to mimic `string/array/slice`-behaviour, opCast should return
 * - `false` for `S()` and
 * - `true` for `S("")`
 * This requires `S()` to default to a large string in which large pointer is set to null
 *
 * NOTE big-endian platforms should be supported but this hasn't been verified.
 *
 * TODO Add to Phobos' std.typecons or std.array or std.string
 *
 * See_Also: https://forum.dlang.org/post/pb87rn$2icb$1@digitalmars.com
 *
 * TODO Use extra bits in Short.length for
 * - lowercase English into 128/5 = 25 chars
 * - uppercase English into 120/5 = 25 chars
 * - mixedcase English into 120/6 = 20 chars
 */
struct SSOString
{
@safe:

    private alias E = char;     // element type

    @property void toString(scope void delegate(const(E)[]) @safe sink) const
    {
        sink(opSlice());
    }

pure:

    /** Return `this` lowercased. */
    typeof(this) toLower()() const @trusted // template-lazy
    {
        if (isSmallASCIIClean())
        {
            typeof(return) result = void;
            result.small.length = small.length;
            foreach (const index; 0 .. smallCapacity)
            {
                import std.ascii : toLower;
                (cast(E[])(result.small.data))[index] = toLower(small.data[index]); // TODO can this be parallelized?
            }
            return result;
        }
        else
        {
            if (isLarge)
            {
                import std.uni : asLowerCase;
                import std.conv : to;
                return typeof(return)(opSlice().asLowerCase.to!string); // TODO make .to!string nothrow
            }
            else
            {
                typeof(return) result = this; // copy
                import std.uni : toLowerInPlace;
                auto slice = cast(E[])(result.opSlice()); // need ref to slice
                toLowerInPlace(slice);
                if (slice is result.opSlice() || // no reallocation
                    slice.length == result.length) // or same length (happens for German double-s)
                {
                    return result;
                }
                else
                {
                    version(none)
                    {
                        import nxt.dbgio;
                        dbg(`toLowerInPlace reallocated from "`,
                            result.opSlice(), `" of length `, result.opSlice().length,
                            ` to "`
                            , slice, `" of length `, slice.length);
                    }
                    return typeof(return)(slice); // reallocation occurred
                }
            }
        }
    }

    /** Return `this` uppercased. */
    typeof(this) toUpper()() const @trusted // template-lazy
    {
        if (isSmallASCIIClean())
        {
            typeof(return) result = void;
            result.small.length = small.length;
            foreach (const index; 0 .. smallCapacity)
            {
                import std.ascii : toUpper;
                (cast(E[])(result.small.data))[index] = toUpper(small.data[index]); // TODO can this be parallelized?
            }
            return result;
        }
        else
        {
            if (isLarge)
            {
                import std.uni : asUpperCase;
                import std.conv : to;
                return typeof(return)(opSlice().asUpperCase.to!string); // TODO make .to!string nothrow
            }
            else
            {
                typeof(return) result = this; // copy
                import std.uni : toUpperInPlace;
                auto slice = cast(E[])(result.opSlice()); // need ref to slice
                toUpperInPlace(slice);
                if (slice is result.opSlice() || // no reallocation
                    slice.length == result.length) // or same length (happens for German double-s)
                {
                    return result;
                }
                else
                {
                    version(none)
                    {
                        import nxt.dbgio;
                        dbg(`toUpperInPlace reallocated from "`,
                            result.opSlice(), `" of length `, result.opSlice().length,
                            ` to "`
                            , slice, `" of length `, slice.length);
                    }
                    return typeof(return)(slice); // reallocation occurred
                }
            }
        }
    }

    pure nothrow:

    /** Construct from `source`, which potentially needs GC-allocation (iff
     * `source.length > smallCapacity` and `source` is not a `string`).
     */
    this(Chars)(const scope auto ref Chars source) @trusted
    if (isCharArray!(typeof(source[]))) // not immutable `E`
    {
        static if (__traits(isStaticArray, Chars))
        {
            static if (source.length <= smallCapacity) // inferred @nogc
            {
                // pragma(msg, "Small static array source of length ", Chars.length);
                small.data[0 .. source.length] = source;
                small.length = cast(typeof(small.length))(encodeSmallLength(source.length));
            }
            else
            {
                // pragma(msg, "Large static array source of length ", Chars.length);
                static if (is(typeof(source[0]) == immutable(E)))
                {
                    large = source; // already immutable so no duplication needed
                }
                else
                {
                    large = source.idup; // GC-allocate
                }
                raw.length = encodeLargeLength(raw.length);
            }
        }
        else                    // `Chars` is a (dynamic) array slice
        {
            if (source.length <= smallCapacity)
            {
                (cast(char*)small.data.ptr)[0 .. source.length] = source;
                small.length = cast(typeof(small.length))(encodeSmallLength(source.length));
            }
            else
            {
                static if (is(typeof(source[0]) == immutable(E)))
                {
                    large = source; // already immutable so no duplication needed
                }
                else
                {
                    large = source.idup; // GC-allocate
                }
                raw.length = encodeLargeLength(raw.length);
            }
        }
    }

    import std.traits : isIterable;

    this(Source)(Source source)
    if (isIterable!(Source) &&
        is(ElementType!Source : dchar))
    {
        static assert(0, "TODO complete this function");
        size_t precount = 0;
        foreach (dch; source)
        {
            import std.utf : encode;
            char[4] chars;
            precount += encode(chars, dch);
        }

        if (precount <= smallCapacity)
        {
            size_t offset = 0;
            foreach (dch; source)
            {
                import std.utf : encode;
                char[4] chars;
                const count = encode(chars, dch);
                offset += count;
            }
            small.length = offset;
        }
        else
        {
            assert(0);
        }
    }

    /** Return `this` converted to a `string`, without any GC-allocation because
     * `this` is `immutable`.
     */
    @property string toString() immutable @trusted pure nothrow @nogc // never allocates
    {
        return opSlice();
    }

    /** Return `this` converted to a `string`, which potentially needs
     * GC-allocation (iff `length > smallCapacity`).
     *
     * implementation kept in sync with `opSlice`.
     */
    @property string toString() const return @trusted pure nothrow // may GC-allocate
    {
        if (isLarge)
        {
            // GC-allocated slice has immutable members so ok to cast
            return cast(typeof(return))raw.ptr[0 .. decodeRawLength(raw.length)]; // no allocation
        }
        else
        {
            return small.data.ptr[0 .. decodeRawLength(small.length)].idup; // need duplicate to make `immutable`
        }
    }

    @nogc:

    /** Get hash of `this`, with extra fast computation for the small case.
     */
    @property hash_t toHash() const scope @trusted
    {
        version(LDC) pragma(inline, true);
        if (isLarge)
        {
            import core.internal.hash : hashOf;
            return hashOf(opSlice());
        }
        else                    // fast path for small string
        {
            import nxt.hash_functions : wangMixHash64;
            return (wangMixHash64(words[0] >> 1) ^ // shift away LS-bit always being zero
                    wangMixHash64(words[1]));
        }
    }

    /** Get length. */
    @property size_t length() const scope @trusted
    {
        pragma(inline, true);
        if (isLarge)
        {
            return decodeRawLength(large.length); // skip first bit
        }
        else
        {
            return decodeRawLength(small.length); // skip fist bit
        }
    }
    /// ditto
    alias opDollar = length;

    @property bool empty() const scope @safe pure nothrow @nogc { return length() == 0; }

    @property bool isNull() const scope @safe pure nothrow @nogc { return this == typeof(this).init; }

    /** Return a slice to either the whole large or whole small `string`.
     *
     * Implementation is kept in sync with `toString`.
     */
    inout(E)[] opSlice() inout return scope @trusted @nogc
    {
        if (isLarge)
        {
            return cast(typeof(return))raw.ptr[0 .. decodeRawLength(raw.length)]; // no allocation
            // alternative:  return large.ptr[0 .. large.length/2];
        }
        else
        {
            return cast(typeof(return))small.data.ptr[0 .. decodeRawLength(small.length)]; // scoped
        }
    }

    /** Return a slice at `[i .. j]` to either the internally stored large or small `string`.
     *
     * Implementation is kept in sync with `toString`.
     */
    inout(E)[] opSlice(size_t i, size_t j) inout return @safe
    {
        pragma(inline, true);
        return opSlice()[i .. j];
    }

    /** Return the `index`ed `char` of `this`.
     */
    ref inout(E) opIndex(size_t index) inout return @trusted
    {
        pragma(inline, true);
        return opSlice()[index]; // does range check
    }

    /// Get pointer to the internally stored `char`s.
    @property private immutable(E)* ptr() const return @trusted
    {
        if (isLarge)
        {
            return large.ptr;   // GC-heap pointer
        }
        else
        {
            return small.data.ptr; // stack pointer
        }
    }

    /** Check if `this` is equal to `rhs`. */
    bool opEquals()(const scope auto ref typeof(this) rhs) const scope @trusted
    {
        pragma(inline, true);
        return opSlice() == rhs.opSlice();
    }

    /** Check if `this` is equal to `rhs`. */
    bool opEquals()(const scope const(E)[] rhs) const scope @trusted
    {
        pragma(inline, true);
        return opSlice() == rhs;
    }

    /** Compare `this` with `that`.
     *
     * See_Also: https://forum.dlang.org/post/muhfypwftdivluqdbmdf@forum.dlang.org
     */
    @property int opCmp()(const scope typeof(this) that) const scope // template-lazy
    {
        pragma(inline, true);
        auto a = this[];
        auto b = that[];
        return a < b ? -1 : (a > b);
        // import core.internal.array.comparison : __cmp; // instead of `std.algorithm.comparison : cmp`;
        // return __cmp(this[], that[]);
    }

    bool opCast(T : bool)() const scope @trusted
    {
        pragma(inline, true);
        if (isLarge)
        {
            return large !is null;
        }
        else
        {
            return small.length != 0;
        }
    }

    /** Check if is the same as to `rhs`.
     *
     * See_Also: https://forum.dlang.org/post/agzznbzkacfhyqvoezht@forum.dlang.org.
     */
    version(none)               // `is` operator cannot be overloaded. See: https://forum.dlang.org/post/prmrli$1146$1@digitalmars.com
    bool opBinary(string op)(const scope auto ref typeof(this) rhs) const scope @trusted
    if (op == `is`)         // TODO has not effect
    {
        pragma(inline, true);
        return opSlice() == rhs.opSlice();
    }

    /** Support trait `isNullable`. */
    static immutable nullValue = typeof(this).init;

    /** Support trait `isHoleable`. */
    static immutable holeValue = typeof(this).asHole();
    /** Check if this a hole, meaning a removed/erase value. */
    bool isHole() const scope @safe nothrow @nogc
    {
        return words[0] == size_t.max;
    }
    void holeify() @system @nogc scope
    {
        words[0] = size_t.max;
        words[1] = size_t.max;
    }
    /** Create a hole, meaning a removed/erase value. */
    private static typeof(this) asHole() @system
    {
        typeof(return) result = void;
        result.holeify();
        return result;
    }

    /** Check if `this` is a small ASCII string. */
    bool isSmallASCIIClean() const scope @trusted
    {
        pragma(inline, true);
        static assert(largeLengthTagBit == 0);// bit 0 of lsbyte not set => small
        // should be fast on 64-bit platforms:
        return ((words[0] & 0x_80_80_80_80__80_80_80_01UL) == 0 && // bit 0 of lsbyte not set => small
                (words[1] & 0x_80_80_80_80__80_80_80_80UL) == 0);
    }

private:

    /** Returns: `true` iff this is a large string, otherwise `false.` */
    @property bool isLarge() const @trusted
    {
        pragma(inline, true);
        return large.length & (1 << largeLengthTagBit); // first bit discriminates small from large
    }

    alias Large = immutable(E)[];

    public enum smallCapacity = Large.sizeof - Small.length.sizeof;
    static assert(smallCapacity > 0, "No room for small source for immutable(E) being " ~ immutable(E).stringof);

    enum largeLengthTagBit = 0; ///< bit position for large tag in length.
    enum smallLengthBitCount = 4;
    static assert(smallCapacity == 2^^smallLengthBitCount-1);

    enum metaBits = 3;               ///< Number of bits used for metadata.
    enum metaMask = (2^^metaBits-1); ///< Mask for metadata shifted to bottom.
    enum tagsBitCount = 1 + metaBits; ///< Number of bits used for small discriminator plus extra meta data.
    static assert(smallLengthBitCount + tagsBitCount == 8);

    /// Get metadata byte with first `metaBits` bits set.
    @property ubyte metadata() const @safe pure nothrow @nogc
    {
        return (small.length >> (1 << largeLengthTagBit)) & metaMask; // git bits [1 .. 1+metaBits]
    }

    /// Set metadata.
    @property void metadata(ubyte data) @trusted pure nothrow @nogc
    {
        assert(data < (1 << metaBits));
        if (isLarge)
        {
            raw.length = encodeLargeLength(length) | ((data & metaMask) << (largeLengthTagBit + 1));
        }
        else
        {
            small.length = cast(ubyte)encodeSmallLength(length) | ((data & metaMask) << (largeLengthTagBit + 1));
        }
    }

    /// Decode raw length `rawLength` by shifting away tag bits.
    static size_t decodeRawLength(size_t rawLength) @safe pure nothrow @nogc
    {
        return rawLength >> tagsBitCount;
    }

    /// Encode `Large` length from `Length`.
    static size_t encodeLargeLength(size_t length) @safe pure nothrow @nogc
    {
        return (length << tagsBitCount) | 1;
    }

    /// Encode `Small` length from `Length`.
    static size_t encodeSmallLength(size_t length) @safe pure nothrow @nogc
    {
        assert(length <= smallCapacity);
        return length << tagsBitCount;
    }

    version(LittleEndian) // see: http://forum.dlang.org/posting/zifyahfohbwavwkwbgmw
    {
        struct Small
        {
            /* TODO only first 4 bits are needed to represent a length between
             * 0-15, use other 4 bits.
             */
            ubyte length = 0;
            immutable(E)[smallCapacity] data = [0,0,0,0,0,
                                                0,0,0,0,0,
                                                0,0,0,0,0]; // explicit init needed for `__traits(isZeroInit)` to be true.
        }
    }
    else
    {
        struct Small
        {
            immutable(E)[smallCapacity] data = [0,0,0,0,0,
                                                0,0,0,0,0,
                                                0,0,0,0,0]; // explicit init needed for `__traits(isZeroInit)` to be true.
            /* TODO only first 4 bits are needed to represent a length between
             * 0-15, use other 4 bits.
             */
            ubyte length;
        }
        static assert(0, "TODO add BigEndian support and test");
    }

    struct Raw                  // same memory layout as `immutable(E)[]`
    {
        size_t length = 0;      // can be bit-fiddled without GC allocation
        immutable(E)* ptr = null;
    }

    union
    {
        Raw raw;
        Large large;
        Small small;
        size_t[2] words;
    }
}
version(unittest) static assert(SSOString.sizeof == string.sizeof);

/// construct from non-immutable source is allowed in non-`@nogc` context
@safe pure nothrow unittest
{
    alias S = SSOString;

    const char[] x0;
    const s0 = S(x0);           // no .idup

    const char[] x16 = new char[16];
    const s16 = S(x16);         // will call .idup
}

/// construct from non-immutable source is not allowed in `@nogc` context
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    const char[] s;
    static assert(__traits(compiles, { const s0_ = S(s); }));
}

/// test behaviour of `==` and `is` operator
@trusted pure nothrow @nogc unittest
{
    alias S = SSOString;

    const S x = "42";
    assert(!x.isNull);
    assert(x == "42");

    const S y = "42";
    assert(!y.isNull);
    assert(y == "42");

    assert(x == y);
    assert(x == y[]);
    assert(x[] == y);
    assert(x[] == y[]);
    assert(x[] is x[]);
    assert(y[] is y[]);
    assert(x[] !is y[]);
    assert(x.ptr !is y.ptr);

    const S z = "43";
    assert(!z.isNull);
    assert(z == "43");
    assert(x != z);
    assert(x[] != z[]);
    assert(x !is z);
    assert(x[] !is z[]);
}

///
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;

    static assert(S.smallCapacity == 15);

    import nxt.gc_traits : mustAddGCRange;
    static assert(mustAddGCRange!S); // `Large large.ptr` must be scanned

    static assert(__traits(isZeroInit, S));
    // TODO static assert(S.init == S.nullValue);

    auto s0 = S.init;
    assert(s0.isNull);
    assert(s0.length == 0);
    assert(!s0.isLarge);
    assert(s0[] == []);

    char[S.smallCapacity] charsSmallCapacity = "123456789_12345"; // fits in small string
    const sSmallCapacity = S(charsSmallCapacity);
    assert(!sSmallCapacity.isLarge);
    assert(sSmallCapacity.length == S.smallCapacity);
    assert(sSmallCapacity == charsSmallCapacity);

    const s0_ = S("");
    assert(s0_.isNull);         // cannot distinguish
    assert(s0 == s0_);

    const s7 = S("0123456");
    assert(!s7.isNull);

    const s7_ = S("0123456_"[0 .. $ - 1]);
    assert(s7.ptr !is s7_.ptr); // string data shall not overlap
    assert(s7 == s7_);

    const _s7 = S("_0123456"[1 .. $]); // source from other string literal
    assert(s7.ptr !is _s7.ptr); // string data shall not overlap
    assert(s7 == _s7);

    assert(!s7.isLarge);
    assert(s7.length == 7);
    assert(s7[] == "0123456");
    assert(s7[] == "_0123456"[1 .. $]);
    assert(s7[] == "0123456_"[0 .. $ - 1]);
    assert(s7[0 .. 4] == "0123");

    const s15 = S("0123456789abcde");
    assert(!s15.isNull);
    static assert(is(typeof(s15[]) == const(char)[]));
    assert(!s15.isLarge);
    assert(s15.length == 15);
    assert(s15[] == "0123456789abcde");
    assert(s15[0 .. 4] == "0123");
    assert(s15[10 .. 15] == "abcde");
    assert(s15[10 .. $] == "abcde");

    const s16 = S("0123456789abcdef");
    assert(!s16.isNull);
    static assert(is(typeof(s16[]) == const(char)[]));
    assert(s16.isLarge);

    const s16_ = S("0123456789abcdef_"[0 .. s16.length]);
    assert(s16.length == s16_.length);
    assert(s16[] == s16_[]);
    assert(s16.ptr !is s16_.ptr); // string data shall not overlap
    assert(s16 == s16_);              // but contents is equal

    const _s16 = S("_0123456789abcdef"[1 .. $]);
    assert(s16.length == _s16.length);
    assert(s16[] == _s16[]);    // contents is equal
    assert(s16 == _s16);        // contents is equal

    assert(s16.length == 16);
    assert(s16[] == "0123456789abcdef");
    assert(s16[0] == '0');
    assert(s16[10] == 'a');
    assert(s16[15] == 'f');
    assert(s16[0 .. 4] == "0123");
    assert(s16[10 .. 16] == "abcdef");
    assert(s16[10 .. $] == "abcdef");
}

/// metadata for null string
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    auto s = S.init;
    assert(s.isNull);
    foreach (const i; 0 .. 8)
    {
        s.metadata = i;
        assert(s.metadata == i);
        assert(s.length == 0);
        // TODO assert(!s.isNull);
    }
}

/// metadata for small string
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    auto s = S("0123456");
    assert(!s.isNull);
    assert(!s.isLarge);
    foreach (const i; 0 .. 8)
    {
        s.metadata = i;
        assert(s.metadata == i);
        assert(s.length == 7);
        assert(!s.isLarge);
        assert(!s.isNull);
    }
}

/// metadata for small string with maximum length
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    auto s = S("0123456789abcde");
    assert(s.length == S.smallCapacity);
    assert(!s.isNull);
    assert(!s.isLarge);
    foreach (const i; 0 .. 8)
    {
        s.metadata = i;
        assert(s.metadata == i);
        assert(s.length == 15);
        assert(!s.isLarge);
        assert(!s.isNull);
    }
}

/// metadata for large string with minimum length
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    auto s = S("0123456789abcdef");
    assert(s.length == S.smallCapacity + 1);
    assert(!s.isNull);
    assert(s.isLarge);
    foreach (const i; 0 .. 8)
    {
        s.metadata = i;
        assert(s.metadata == i);
        assert(s.length == 16);
        assert(s.isLarge);
        assert(!s.isNull);
    }
}

/// construct from static array larger than `smallCapacity`
@safe pure nothrow unittest
{
    alias S = SSOString;
    char[S.smallCapacity + 1] charsMinLargeCapacity;
    const _ = S(charsMinLargeCapacity);
}

/// hole handling
@trusted pure nothrow @nogc unittest
{
    alias S = SSOString;
    assert(!S.init.isHole);
    assert(!S("").isHole);
    assert(!S("a").isHole);
    assert(S.asHole.isHole);
}

/// DIP-1000 return ref escape analysis
@safe pure nothrow unittest
{
    static if (isDIP1000)
    {
        alias S = SSOString;
        static assert(!__traits(compiles, { immutable(char)* f1() @safe pure nothrow { S x; return x.ptr; } }));
        static assert(!__traits(compiles, { string f1() @safe pure nothrow { S x; return x[]; } }));
        static assert(!__traits(compiles, { string f2() @safe pure nothrow { S x; return x.toString; } }));
        static assert(!__traits(compiles, { ref immutable(char) g() @safe pure nothrow @nogc { S x; return x[0]; } }));
    }
}

/// ASCII purity and case-conversion
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;

    // these are all small ASCII
    assert( S("a").isSmallASCIIClean);
    assert( S("b").isSmallASCIIClean);
    assert( S("z").isSmallASCIIClean);
    assert( S("_").isSmallASCIIClean);
    assert( S("abcd").isSmallASCIIClean);
    assert( S("123456789_12345").isSmallASCIIClean);

    // these are not
    assert(!S("123456789_123456").isSmallASCIIClean); // too large
    assert(!S("123456789_123ö").isSmallASCIIClean);
    assert(!S("ö").isSmallASCIIClean);
    assert(!S("Ö").isSmallASCIIClean);
    assert(!S("åäö").isSmallASCIIClean);
    assert(!S("ö-värld").isSmallASCIIClean);
}

/// ASCII purity and case-conversion
@safe pure unittest
{
    alias S = SSOString;
    assert(S("A").toLower[] == "a");
    assert(S("a").toUpper[] == "A");
    assert(S("ABCDEFGHIJKLMNO").toLower[] == "abcdefghijklmno"); // small
    assert(S("abcdefghijklmno").toUpper[] == "ABCDEFGHIJKLMNO"); // small
    assert(S("ÅÄÖ").toLower[] == "åäö");
    assert(S("åäö").toUpper[] == "ÅÄÖ");
    assert(S("ABCDEFGHIJKLMNOP").toLower[] == "abcdefghijklmnop"); // large
    assert(S("abcdefghijklmnop").toUpper[] == "ABCDEFGHIJKLMNOP"); // large

    char[6] x = "ÅÄÖ";
    import std.uni : toLowerInPlace;
    auto xref = x[];
    toLowerInPlace(xref);
    assert(x == "åäö");
    assert(xref == "åäö");
}

/// lexicographic comparison
@safe pure unittest
{
    alias S = SSOString;

    const S a = S("a");
    assert(a == S("a"));

    immutable S b = S("b");

    assert(a < b);
    assert(b > a);
    assert(a[] < b[]);

    assert("a" < "b");
    assert("a" < "å");
    assert("Å" < "å");
    assert(S("a") < S("å"));
    assert(S("ÅÄÖ") < S("åäö"));
}

/// cast to bool
@safe pure unittest
{
    alias S = SSOString;

    assert(S.init.small.length == 0);
    assert(!S());
    // TODO assert(S(""));
    assert(S("abc"));
}

/// to string conversion
@safe pure unittest
{
    alias S = SSOString;

    // mutable small will GC-allocate
    {
        S s = S("123456789_12345");
        assert(s.ptr is s.opSlice.ptr);
        assert(s.ptr !is s.toString.ptr);
    }

    // const small will GC-allocate
    {
        const S s = S("123456789_12345");
        assert(s.ptr is s.opSlice.ptr);
        assert(s.ptr !is s.toString.ptr);
    }

    // immutable small will not allocate
    {
        immutable S s = S("123456789_12345");
        assert(s.ptr is s.opSlice.ptr);
        assert(s.ptr is s.toString.ptr);
        // TODO check return via -dip1000
    }

    /* Forbid return of possibly locally scoped `Smll` small stack object
     * regardless of head-mutability.
     */
    static if (isDIP1000)
    {
        static assert(!__traits(compiles, { immutable(char)* f1() @safe pure nothrow { S x; return x.ptr; } }));
        static assert(!__traits(compiles, { immutable(char)* f1() @safe pure nothrow { const S x; return x.ptr; } }));
        static assert(!__traits(compiles, { immutable(char)* f1() @safe pure nothrow { immutable S x; return x.ptr; } }));
    }

    // large will never allocate regardless of head-mutability
    {
        S s = S("123456789_123456");
        assert(s.ptr is s.opSlice.ptr);
        assert(s.ptr is s.toString.ptr); // shouldn't this change?
    }
}

@safe pure unittest
{
    // TODO static immutable any = SSOString(`alpha`);
}

///
version(show)
@safe unittest
{
    import std.stdio;
    writeln(SSOString("alpha"));
}

private enum isCharArray(T) = (is(T : const(char)[]));

version(unittest)
{
    import nxt.dip_traits : isDIP1000;
}
