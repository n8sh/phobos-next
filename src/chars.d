/** Minimal stores. */
module chars;

/** A few (`capacity`) chars. */
struct FewChars(uint capacity)
    if (capacity <= 255)
{
    this(in char[] chars)
        @trusted
    {
        assert(chars.length <= capacity,
               "Input parameter `chars` too large");
        _chars.ptr[0 .. chars.length] = chars;
        _length = cast(typeof(_length))chars.length;
    }

private:
    char[capacity] _chars;
    ubyte _length;
}

@safe pure nothrow @nogc unittest
{
    const ch7 = FewChars!(7)(`1234567`);
    assert(ch7._chars[] == `1234567`);
}
