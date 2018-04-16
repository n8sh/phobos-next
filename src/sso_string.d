module sso_string;

struct SSOArray(T)
{
    this(scope T[] elements)
    {
        if (elements.length <= smallCapacity)
        {
            small.data[0 .. elements.length] = elements;
        }
        else
        {
            large = elements;
        }
    }

    pure nothrow @nogc:

    scope inout(T)[] opSlice() inout return
    {
        if (isLarge)
        {
            return large;       // as is
        }
        else
        {
            return small.data[0 .. small.length];
        }
    }

    @property bool isLarge() const @safe
    {
        return true;
    }

private:
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
