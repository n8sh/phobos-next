unittest
{
    immutable(int)[] x = [1, 2];
    const(int)[] y = [3, 4];
    x ~= y;
    assert(x == [1,2,3,4]);
}
