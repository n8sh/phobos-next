module string_traits;

@safe pure nothrow @nogc:

/** Returns: `true` iff `x` is an ASCII (7-bit clean) set of `char`s.
 *
 * See_Also: `std.ascii.isASCII`.
 */
bool isASCII(scope const(char)[] input)
{
    foreach (e; cast(const(ubyte)[])input)
    {
        if (e >= 0x7F) { return false; }
    }
    return true;
}

@safe pure unittest
{
    assert(`a`.isASCII);
    assert(`ab`.isASCII);
    assert(`abc`.isASCII);
    assert(!`aåö`.isASCII);
}
