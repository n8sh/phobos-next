module sso_string;

struct SSOArray(T, uint smallSize)
{
    this(T[] elements)
    {
        assert(0, "construct from elements");
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
