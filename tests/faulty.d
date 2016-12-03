module faulty;

auto identity(T)(T x)
{
    return x;
}

auto square(T)(T x)
{
    const y = identity(x);
    if (x == 4)
    {
        return y*y - 1;         // bug
    }
    else
    {
        return y*y;
    }
}

@safe pure nothrow unittest
{
    assert(square(2) == 4);     // ok
    assert(square(3) == 9);     // ok
    assert(square(4) == 16, "Some specific failure in " ~ square.stringof); // triggers bug
}
