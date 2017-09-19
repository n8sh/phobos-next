/** Minimal array storage. */
module fixed_array;

/** A few (`capacity`) elements of type `E`.
 */
struct FixedArray(E, uint capacity)
    if (capacity <= ubyte.max)
{
    this(in E[] Es)
        @trusted
    {
        assert(Es.length <= capacity,
               "Input parameter `Es` too large");
        _Es.ptr[0 .. Es.length] = Es;
        _length = cast(typeof(_length))Es.length;
    }

private:
    E[capacity] _Es;
    ubyte _length;
}

@safe pure nothrow @nogc unittest
{
    const ch7 = FixedArray!(char, 7)(`1234567`);
    assert(ch7._Es[] == `1234567`);
}
