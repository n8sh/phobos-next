module minimal_fixed_array;

@safe:

/** Minimalistic fixed-length (static) array of (`capacity`) number of elements
 * of type `E` where length only fits in an `ubyte` for compact packing.
 */
struct MinimalFixedArray(E, uint capacity)
    if (capacity <= ubyte.max)
{
    @safe:

    this(in E[] es)
    {
        assert(es.length <= capacity,
               "Length of input parameter `es` is larger than capacity "
               ~ capacity.stringof);
        _es[0 .. es.length] = es;
        _length = cast(typeof(_length))es.length;
    }

private:
    E[capacity] _es;
    ubyte _length;
}

@safe pure nothrow @nogc unittest
{
    const ch7 = MinimalFixedArray!(char, 7)(`1234567`);
    assert(ch7._es[] == `1234567`);
}
