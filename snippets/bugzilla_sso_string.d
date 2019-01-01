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

    pure nothrow @nogc:

    scope inout(E)* ptr() inout return @trusted
    {
        if (isLarge)
        {
            return &large.ptr[0]; // GC-heap pointer
        }
        else
        {
            return &small.data[0]; // stack pointer
        }
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
            return small.data.ptr[0 .. small.length/2];
        }
    }

    /// ditto
    scope E[] opSlice(size_t i, size_t j) const return @trusted // TODO @safe for -dip1000?
    {
        return opSlice()[i .. j];
    }

private:

    /** Returns: `true` iff this is a large string, otherwise `false.` */
    @property bool isLarge() const @trusted
    {
        return large.length & 1; // first bit discriminates small from large
    }

    struct Raw                  // same memory layout as `E[]`
    {
        size_t length;          // can be bit-fiddled without GC allocation
        E* ptr;
    }

    alias Large = E[];

    enum smallCapacity = Large.sizeof - Small.length.sizeof;
    static assert(smallCapacity > 0, "No room for small source for E being " ~ E.stringof);
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
    immutable(char)* ptrFail1() @safe pure nothrow @nogc
    {
        SSOString x;
        return x.ptr;           // TODO should fail with -dip25 or -dip1000
    }
    string opSliceFail1() @safe pure nothrow @nogc
    {
        SSOString x;
        return x[];             // TODO should fail with -dip25 or -dip1000
    }
    string opSliceFail2() @safe pure nothrow @nogc
    {
        SSOString x;
        return x[0 .. 0];       // TODO should fail with -dip25 or -dip1000
    }
}
