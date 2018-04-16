module sso_string;

struct SSOArray(T)
{
    this(scope T[] elements) @trusted
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

    scope inout(T)[] opSlice() inout return @trusted
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
        return large.length & 1; // first bit is discriminator
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
        T* ptr;
    }

    alias Large = T[];

    enum smallCapacity = Large.sizeof - Small.length.sizeof;
    static assert(smallCapacity > 0, "No room for small elements for T being " ~ T.stringof);
    version(LittleEndian) // see: http://forum.dlang.org/posting/zifyahfohbwavwkwbgmw
    {
        struct Small
        {
            ubyte length;
            T[smallCapacity] data;
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

    auto s15 = S("012345678901234");
    static assert(is(typeof(s15[]) == string)); // TODO scoped
    assert(!s15.isLarge);
    assert(s15.length == 15);
    assert(s15[] == "012345678901234");

    auto s16 = S("0123456789012345");
    static assert(is(typeof(s16[]) == string)); // TODO scoped
    assert(s16.isLarge);
    assert(s16.length == 16);
    // TODO assert(s16[] == "0123456789012345");
}

version(unittest)
{
    import dbgio;
}
