module sso_string;

struct SSOArray(E)
{
    this(scope E[] elements) @trusted
    {
        if (elements.length <= smallCapacity)
        {
            small.data[0 .. elements.length] = elements;
            small.length = cast(typeof(small.length))(2*elements.length);
        }
        else
        {
            large = elements;
            raw.length *= 2;  // shift up
            raw.length |= 1;  // tag as large
        }
    }

    pure nothrow @nogc:

    ref inout(E) opIndex(size_t index) inout return scope @trusted
    {
        return opSlice()[index];
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

    @property bool isLarge() const @trusted
    {
        return large.length & 1; // first bit is discriminator between small and large
    }

    @property size_t length() const @trusted
    {
        if (isLarge)
        {
            return large.length/2;
        }
        else
        {
            return small.length/2;
        }
    }

private:
    struct Raw
    {
        size_t length;          // can be changed without GC allocation
        E* ptr;
    }

    alias Large = E[];

    enum smallCapacity = Large.sizeof - Small.length.sizeof;
    static assert(smallCapacity > 0, "No room for small elements for E being " ~ E.stringof);
    version(LittleEndian) // see: http://forum.dlang.org/posting/zifyahfohbwavwkwbgmw
    {
        struct Small
        {
            ubyte length;
            E[smallCapacity] data;
        }
    }
    else
    {
        static assert(0, "BigEndian support and test");
    }

    union
    {
        Raw raw;
        Large large;
        Small small;
    }
}

alias SSOString = SSOArray!(immutable(char));
alias SSOMutString = SSOArray!(char);

///
@safe pure nothrow @nogc unittest
{
    alias S = SSOString;
    static assert(S.smallCapacity == 15);

    auto s7 = S("0123456");
    static assert(is(typeof(s7[]) == string)); // TODO DIP-1000
    assert(!s7.isLarge);
    assert(s7.length == 7);
    assert(s7[] == "0123456");
    // TODO assert(s7[0 .. 4] == "0123");

    auto s15 = S("012345678901234");
    static assert(is(typeof(s15[]) == string)); // TODO DIP-1000
    assert(!s15.isLarge);
    assert(s15.length == 15);
    assert(s15[] == "012345678901234");

    auto s16 = S("0123456789abcdef");
    static assert(is(typeof(s16[]) == string)); // TODO DIP-1000
    assert(s16.isLarge);
    assert(s16.length == 16);
    assert(s16[] == "0123456789abcdef");
    assert(s16[0] == '0');
    assert(s16[10] == 'a');
    assert(s16[15] == 'f');
}
