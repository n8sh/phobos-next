module sso_string;

/** Small-size-optimized (SSO) variant of `string`.
 *
 * Store on the stack if constructed with <= `smallCapacity` number of
 * characters, otherwise on the GC heap.
 *
 * Because `SSOString` doesn't have a destructor it can safely allocate using a
 * GC-backed region allocator without relying on a GC finalizer.
 *
 * TODO: Add Phobos' std.typecons or std.array or std.string
 * See_Also: https://forum.dlang.org/post/pb87rn$2icb$1@digitalmars.com
 */
struct SSOString
{
    private alias E = char;     // element type

    @property void toString(scope void delegate(const(E)[]) @safe sink) const @trusted // TODO ok to have sink @safe?
    {
        sink(opSlice());        // opSlice is @trusted here
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
                (cast(char[])(result.small.data))[index] = toLower(small.data[index]);
            }
            return result;
        }
        else
        {
            enum useToLowerInPlace = false;
            static if (useToLowerInPlace)
            {
                typeof(return) result = this; // copy
                import std.uni : toLowerInPlace;
                auto slice = cast(char[])(result.opSlice());
                toLowerInPlace(slice);
                return result;
            }
            else
            {
                import std.uni : asLowerCase;
                import std.conv : to;
                return typeof(return)(opSlice().asLowerCase.to!string); // TODO make .to!string nothrow
            }
        }
    }

    pure nothrow:

    /** Construct from `source`, which potentially needs GC-allocation (iff
     * `source.length > smallCapacity` and `source` is not a `string`).
     */
    this(SomeCharArray)(const scope auto ref SomeCharArray source) @trusted
    if (isCharsSlice!(typeof(source[]))) // not immutable char
    {
        static if (__traits(isStaticArray, SomeCharArray))
        {
            static if (source.length <= smallCapacity) // inferred @nogc
            {
                small.data[0 .. source.length] = source;
                small.length = cast(typeof(small.length))(2*source.length);
            }
            else
            {
                static if (is(typeof(source[0]) == immutable(char)))
                {
                    large = source; // already immutable so no duplication needed
                }
                else
                {
                    large = source.idup; // GC-allocate
                }
                raw.length *= 2;  // shift up
                raw.length |= 1;  // tag as large
            }
        }
        else
        {
            if (source.length <= smallCapacity)
            {
                small.data[0 .. source.length] = source;
                small.length = cast(typeof(small.length))(2*source.length);
            }
            else
            {
                static if (is(typeof(source[0]) == immutable(char)))
                {
                    large = source; // already immutable so no duplication needed
                }
                else
                {
                    large = source.idup; // GC-allocate
                }
                raw.length *= 2;  // shift up
                raw.length |= 1;  // tag as large
            }
        }
    }

    /** Return `this` converted to a `string`, which potentially needs
     * GC-allocation (iff `length > smallCapacity`).
     */
    @property string toString() const return @trusted pure nothrow // may GC-allocate
    {
        if (isLarge)
        {
            return opSlice(); // already immutable
        }
        else
        {
            // TODO can we return immutable slice if `this` is `immutable`
            return opSlice().idup; // need duplicate to make `immutable`
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
            import hash_functions : wangMixHash64;
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
            return large.length/2; // skip first bit
        }
        else
        {
            return small.length/2; // skip fist bit
        }
    }
    /// ditto
    alias opDollar = length;

    @property bool empty() const scope @safe pure nothrow @nogc { return length == 0; }

    @property bool isNull() const scope @safe pure nothrow @nogc { return this == typeof(this).init; }

    immutable(E)[] opSlice() const return @trusted
    {
        if (isLarge)
        {
            return raw.ptr[0 .. raw.length/2]; // no allocation
            // alternative:  return large.ptr[0 .. large.length/2];
        }
        else
        {
            return small.data.ptr[0 .. small.length/2]; // scoped
        }
    }

    ref immutable(E) opIndex(size_t index) const return @trusted
    {
        pragma(inline, true);
        return opSlice()[index]; // does range check
    }

    /// ditto
    immutable(E)[] opSlice(size_t i, size_t j) const return @safe
    {
        pragma(inline, true);
        return opSlice()[i .. j];
    }

    /// Get pointer to elements.
    @property immutable(E)* ptr() const return @trusted
    {
        if (isLarge)
        {
            return large.ptr;   // GC-heap pointer
        }
        else
        {
            return &small.data[0]; // stack pointer
        }
    }

    /** Check if equal to `rhs`. */
    bool opEquals()(const scope auto ref typeof(this) rhs) const scope @trusted
    {
        pragma(inline, true);
        return opSlice() == rhs.opSlice();
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

    /** Check if equal to `rhs`. */
    bool opEquals()(const scope const(char)[] rhs) const scope @trusted
    {
        pragma(inline, true);
        return opSlice() == rhs;
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

    /** Check if `this` is a small ASCII pure string.
     */
    bool isSmallASCIIClean() const scope @trusted
    {
        pragma(inline, true);
        // should be fast on 64-bit platforms:
        return ((words[0] & 0x_80_80_80_80__80_80_80_01UL) == 0 && // bit 0 of lsbyte not set => small
                (words[1] & 0x_80_80_80_80__80_80_80_80UL) == 0);
    }

private:

    /** Returns: `true` iff this is a large string, otherwise `false.` */
    @property bool isLarge() const @trusted
    {
        pragma(inline, true);
        return large.length & 1; // first bit discriminates small from large
    }

    alias Large = immutable(E)[];

    public enum smallCapacity = Large.sizeof - Small.length.sizeof;
    static assert(smallCapacity > 0, "No room for small source for immutable(E) being " ~ immutable(E).stringof);
    version(LittleEndian) // see: http://forum.dlang.org/posting/zifyahfohbwavwkwbgmw
    {
        struct Small
        {
            enum bitIndexASCII = 4;
            /* TODO only first 4 bits are needed to represent a length between
             * 0-15, use other 4 bits
             */
            ubyte length = 0;
            immutable(E)[smallCapacity] data = [0,0,0,0,0,
                                                0,0,0,0,0,
                                                0,0,0,0,0]; // explicit init needed for `__traits(isZeroInit)` to be true.
        }
        struct Raw                  // same memory layout as `immutable(E)[]`
        {
            size_t length = 0;      // can be bit-fiddled without GC allocation
            immutable(E)* ptr = null;
        }
    }
    else
    {
        struct Small
        {
            immutable(E)[smallCapacity] data;
            /* TODO only last 4 bits are needed to represent a length between
             * 0-15, use other 4 bits */
            ubyte length;
        }
        static assert(0, "TODO add BigEndian support and test");
    }

    union
    {
        Raw raw;
        Large large;
        Small small;
        size_t[2] words;
    }
}
static assert(SSOString.sizeof == string.sizeof);

/// construct from non-immutable source is allowed in non-@nogc context
@safe pure nothrow unittest
{
    alias S = SSOString;

    const char[] x0;
    const s0 = S(x0);           // no .idup

    const char[] x16 = new char[16];
    const s16 = S(x16);         // will call .idup
}

/// construct from non-immutable source is not allowed in @nogc context
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    const char[] s;
    static assert(__traits(compiles, { const s0_ = S(s); }));
}

/// test behaviour of `is` operator
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
    import gc_traits : mustAddGCRange;
    static assert(mustAddGCRange!S); // `Large large.ptr` must be scanned

    static assert(__traits(isZeroInit, S));
    // TODO static assert(S.init == S.nullValue);

    auto s0 = S.init;
    assert(s0.isNull);
    assert(s0.length == 0);
    assert(!s0.isLarge);
    assert(s0[] == []);

    char[S.smallCapacity] charsSmallCapacity; // fits in small string
    const sSmallCapacity = S(charsSmallCapacity);

    char[S.smallCapacity + 1] charsMinLargeCapacity;
    // TODO:
    // static assert(!__traits(compiles, {
    //             const _ = S(charsMinLargeCapacity);
    //         }));

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

    static assert(is(typeof(s7[]) == string));
    assert(!s7.isLarge);
    assert(s7.length == 7);
    assert(s7[] == "0123456");
    assert(s7[] == "_0123456"[1 .. $]);
    assert(s7[] == "0123456_"[0 .. $ - 1]);
    assert(s7[0 .. 4] == "0123");

    const s15 = S("0123456789abcde");
    assert(!s15.isNull);
    static assert(is(typeof(s15[]) == string));
    assert(!s15.isLarge);
    assert(s15.length == 15);
    assert(s15[] == "0123456789abcde");
    assert(s15[0 .. 4] == "0123");
    assert(s15[10 .. 15] == "abcde");
    assert(s15[10 .. $] == "abcde");

    const s16 = S("0123456789abcdef");
    assert(!s16.isNull);
    static assert(is(typeof(s16[]) == string));
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
    alias S = SSOString;
    static assert(!__traits(compiles, { immutable(char)* f1() @safe pure nothrow { S x; return x.ptr; } }));
    static assert(!__traits(compiles, { string f1() @safe pure nothrow { S x; return x[]; } }));
    static assert(!__traits(compiles, { string f2() @safe pure nothrow { S x; return x.toString; } }));
    static assert(!__traits(compiles, { ref immutable(char) g() @safe pure nothrow @nogc { S x; return x[0]; } }));
}

/// ASCII purity and case-conversion
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    assert( S("a").isSmallASCIIClean);
    assert( S("b").isSmallASCIIClean);
    assert( S("z").isSmallASCIIClean);
    assert( S("_").isSmallASCIIClean);
    assert( S("abcd").isSmallASCIIClean);
    assert( S("123456789_12345").isSmallASCIIClean);
    assert(!S("123456789_123ö").isSmallASCIIClean);
    assert(!S("123456789_123456").isSmallASCIIClean);
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
    assert(S("ABCDEFGHIJKLMNO").toLower[] == "abcdefghijklmno"); // small
    assert(S("ÅÄÖ").toLower[] == "åäö");
    assert(S("ABCDEFGHIJKLMNOP").toLower[] == "abcdefghijklmnop"); // large
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

private enum isCharsSlice(T) = (is(T : const(char)[]));
