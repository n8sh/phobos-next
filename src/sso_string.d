module sso_string;

struct SSOArray(T, uint smallSize)
{
    this(T[] elements)
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

///
@safe pure nothrow @nogc unittest
{
    alias X = SSOArray!(char, 15);
    X x;
}
