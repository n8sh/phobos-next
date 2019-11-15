module string_traits;

@safe pure nothrow @nogc:

/** Returns: `true` iff `x` is an ASCII (7-bit clean) set of `char`s.
 *
 * See_Also: `std.ascii.isASCII`.
 * See_Also: https://forum.dlang.org/post/syaqzkpybhvdehbhffjn@forum.dlang.org
 */
bool isASCII(scope const(char)[] input)
{
    foreach (e; cast(const(ubyte)[])input) // no decoding to `dchar` needed
    {
        if (e >= 0x7F) { return false; }
    }
    return true;
}

///
@safe pure unittest
{
    assert(``.isASCII);
    assert(`_`.isASCII);
    assert(`a`.isASCII);
    assert(`ab`.isASCII);
    assert(`abc`.isASCII);
    assert(!`å`.isASCII);
    assert(!`åä`.isASCII);
    assert(!`åäö`.isASCII);
}
