module sso_string;

struct SSOArray(T)
{
    this(scope T[] elements)
    {
        if (elements.length <= smallCapacity)
        {
            small.data[0 .. elements.length] = elements;
            small.length *= 2;  // shift up
        }
        else
        {
            large = elements;
            large.length *= 2;  // shift up
            small.length |= 1;  // tag as large
        }
    }

    pure nothrow @nogc:

    scope inout(T)[] opSlice() inout return
    {
        if (isLarge)
        {
            assert(0, "shift length of slice down before returning");
            //return large;       // as is
        }
        else
        {
            return small.data[0 .. small.length];
        }
    }

    @property bool isLarge() const @trusted
    {
        return large.length & 1;
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
            ubyte length;           // little-endian first
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
    alias X = SSOString;
    static assert(X.smallCapacity == 15);
    X x;
    static assert(is(typeof(x[]) == string)); // TODO scoped
}

///
@safe pure nothrow @nogc unittest
{
    alias X = SSOMutString;
    static assert(X.smallCapacity == 15);
    X x;
    static assert(is(typeof(x[]) == char[])); // TODO scoped
}
