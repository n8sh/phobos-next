module sso_string;

struct SSOArray(T)
{
    this(const scope T[] elements)
    {
        assert(0, "construct from elements");
    }

    pure nothrow @nogc:

    scope inout(T)[] opSlice() inout
    {
        if (isLarge)
        {
            return large;
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
    enum smallSize = Large.sizeof - Small.length.sizeof;
    struct Small
    {
        T[smallSize] data;
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
    static assert(X.smallSize == 15);
    X x;
}
