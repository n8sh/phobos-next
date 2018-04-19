module sso_string;

/** Small-size-optimized (SSO) variant of `string`.
 *
 * Store on the stack if constructed with <= `smallCapacity` number of
 * characters, otherwise on the GC heap.
 *
 * See_Also: https://forum.dlang.org/post/pb87rn$2icb$1@digitalmars.com
 */
struct SSOString
{
    private alias E = immutable(char); // immutable element type
    private alias ME = char;           // mutable element type

    pure nothrow:

    /** Construct from `elements`, with potential GC-allocation (iff
     * `elements.length > smallCapacity`).
     */
    this(const scope ME[] elements) @trusted
    {
        if (elements.length <= smallCapacity)
        {
            small.data[0 .. elements.length] = elements;
            small.length = cast(typeof(small.length))(2*elements.length);
        }
        else
        {
            large = elements.idup; // GC-allocate
            raw.length *= 2;  // shift up
            raw.length |= 1;  // tag as large
        }
    }

    /** Construct from `elements` without any kind of heap allocation.
     */
    version(none)
    this(size_t n)(const scope ME[n] elements) @trusted // inferred @nogc
    {
        static if (elements.length <= smallCapacity)
        {
            // @nogc
            small.data[0 .. elements.length] = elements;
            small.length = cast(typeof(small.length))(2*elements.length);
        }
        else
        {
            large = elements.idup; // GC-allocate
            raw.length *= 2;  // shift up
            raw.length |= 1;  // tag as large
        }
    }

    @nogc:

    /** Construct from `elements` without any kind of heap allocation.
     */
    this(const scope E[] elements) @trusted
    {
        if (elements.length <= smallCapacity)
        {
            small.data[0 .. elements.length] = elements;
            small.length = cast(typeof(small.length))(2*elements.length);
        }
        else
        {
            large = elements;   // @nogc
            raw.length *= 2;    // shift up
            raw.length |= 1;    // tag as large
        }
    }

    @property size_t length() const @trusted
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

    scope ref inout(E) opIndex(size_t index) inout return @trusted
    {
        pragma(inline, true);
        return opSlice()[index]; // automatic range checking
    }

    scope inout(E)[] opSlice() inout return @trusted
    {
        if (isLarge)
        {
            union RawLarge
            {
                Raw raw;
                Large large;
            }
            RawLarge copy = void;
            copy.large = cast(Large)large;
            copy.raw.length /= 2; // adjust length
            return copy.large;
        }
        else
        {
            return small.data[0 .. small.length/2]; // scoped
        }
    }

    private @property bool isLarge() const @trusted
    {
        pragma(inline, true);
        return large.length & 1; // first bit discriminates small from large
    }

private:
    struct Raw                  // same memory layout as `E[]`
    {
        size_t length;          // can be bit-fiddled without GC allocation
        E* ptr;
    }

    alias Large = E[];

    enum smallCapacity = Large.sizeof - Small.length.sizeof;
    static assert(smallCapacity > 0, "No room for small elements for E being " ~ E.stringof);
    version(LittleEndian) // see: http://forum.dlang.org/posting/zifyahfohbwavwkwbgmw
    {
        struct Small
        {
            ubyte length; // TODO only first 4 bits are needed to represent a length between 0-15, use other 4 bits
            E[smallCapacity] data;
        }
    }
    else
    {
        static assert(0, "TODO Add BigEndian support and test");
    }

    union
    {
        Raw raw;
        Large large;
        Small small;
    }
}

///
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;

    static assert(S.sizeof == 2*size_t.sizeof); // two words
    static assert(S.smallCapacity == 15);
    import container_traits : mustAddGCRange;
    static assert(mustAddGCRange!S); // `Large large.ptr` must be scanned

    auto s0 = S.init;
    assert(s0.length == 0);
    assert(!s0.isLarge);
    assert(s0[] == []);

    const s0_ = S("");
    assert(s0 == s0_);

    // TODO assert(s0 !is s0_);

    const s7 = S("0123456");
    static assert(is(typeof(s7[]) == string));
    assert(!s7.isLarge);
    assert(s7.length == 7);
    assert(s7[] == "0123456");
    // TODO assert(s7[0 .. 4] == "0123");

    const s15 = S("012345678901234");
    static assert(is(typeof(s15[]) == string));
    assert(!s15.isLarge);
    assert(s15.length == 15);
    assert(s15[] == "012345678901234");

    const s16 = S("0123456789abcdef");
    static assert(is(typeof(s16[]) == string));
    assert(s16.isLarge);
    assert(s16.length == 16);
    assert(s16[] == "0123456789abcdef");
    assert(s16[0] == '0');
    assert(s16[10] == 'a');
    assert(s16[15] == 'f');

    // TODO static assert(!__traits(compiles, { auto _ = S((char[]).init); }));

    string f() @safe pure nothrow @nogc
    {
        S x;
        return x[];             // TODO should fail with -dip1000
    }

    // TODO activate
    // ref char g() @safe pure nothrow @nogc
    // {
    //     S x;
    //     return x[0];             // TODO should fail with -dip1000
    // }
}
