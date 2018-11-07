module char_traits;

/** Is `true` iff `x` is an ASCII character compile-time constant.
 *
 * See_Also: `std.ascii.isASCII`.
 *
 * TODO: Extend to array of chars.
 */
template isASCII(alias x)
if (is(typeof(x) : char) ||
    is(typeof(x) : wchar) ||
    is(typeof(x) : dchar))
{
    enum isASCII = x < 128;
}

///
@safe pure nothrow @nogc unittest
{
    static assert(isASCII!'a');
    static assert(!isASCII!'ä');

    immutable ch = 'a';
    static assert(isASCII!ch);

    const cch = 'a';
    static assert(isASCII!cch);

    const wchar wch = 'a';
    static assert(isASCII!wch);

    const wchar wch_ = 'ä';
    static assert(!isASCII!wch_);

    const dchar dch = 'a';
    static assert(isASCII!dch);

    const dchar dch_ = 'ä';
    static assert(!isASCII!dch_);
}
