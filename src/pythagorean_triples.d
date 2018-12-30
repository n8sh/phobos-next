module pythogorean_triples;

struct PossiblePythags(T)
{
    struct Triple
    {
        T x, y, z;
    }
    T z = 2;
    T x = 1;
    T y = 1;

    auto front()
    {
        return Triple(x, y, z);
    }

    void popFront()
    {
        if (++y == z)
        {
            if (++x == z)
            {
                ++z;
                x = 1;
            }
            y = x;
        }
    }

    enum empty = false;
}

auto pythagrange(T = size_t)()
{
    import std.algorithm : filter;
    return PossiblePythags!T().filter!(p => p.x * p.x + p.y * p.y == p.z * p.z);
}

@safe pure unittest
{
    foreach (e; pythagrange)
    {
    }
}
