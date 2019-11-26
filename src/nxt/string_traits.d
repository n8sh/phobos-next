module nxt.string_traits;

@safe pure nothrow @nogc:

/** Returns: `true` iff `x` is an ASCII (7-bit clean) set of `char`s.
 *
 * See_Also: `std.ascii.isASCII`.
 * See_Also: https://forum.dlang.org/post/syaqzkpybhvdehbhffjn@forum.dlang.org
 */
bool isASCIIString(scope const(char)[] input)
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
    assert(``.isASCIIString);
    assert(`_`.isASCIIString);
    assert(`a`.isASCIIString);
    assert(`ab`.isASCIIString);
    assert(`abc`.isASCIIString);
    assert(!`å`.isASCIIString);
    assert(!`åä`.isASCIIString);
    assert(!`åäö`.isASCIIString);
}
