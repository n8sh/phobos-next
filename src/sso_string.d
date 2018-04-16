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

    scope inout(T)[] opSlice() inout
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
    struct Small
    {
        T[smallCapacity] data;
        ubyte length;
    }
    union
    {
        Large large;
        Small small;
    }
}

alias SSOString = SSOArray!char;

///
@safe pure nothrow @nogc unittest
{
    alias X = SSOString;
    static assert(X.smallCapacity == 15);
    X x;
}
