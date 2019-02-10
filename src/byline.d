module byline;

/** Type of NewLine Encoding.
 */
enum Newline
{
    any,                        // Any of win, mac or unix
    win,                        // Windows: "\r\n"
    mac,                        // Mac OS: '\r'
    unix,                       // UNIX/Linux: '\n'
    native,                     // Current OS decides: '\n'
}

import std.traits : isNarrowString;
import std.range : hasSlicing, hasLength;

/** Split Input by line.
 *
 * See_Also: http://forum.dlang.org/thread/fjqpdfzmitcxxzpwlbgb@forum.dlang.org#post-rwxrytxqqurrazifugje:40forum.dlang.org
 *
 * TODO Restrict using isSomeString!Range?
 *
 * TODO This should fail with better error message:
 * assert(equal((cast(ubyte[])"a\nb").byLine!(Newline.any), ["a", "b"]));
 */
auto byLine(Newline nl, Range)(Range input)
if ((hasSlicing!Range &&
     hasLength!Range) ||
    isNarrowString!Range)
{
    static if (nl == Newline.native)
    {
        import std.ascii: newline;
        import std.algorithm: splitter;
        static if (newline.length == 1)
        {
            import std.range: front;
            return input.splitter(newline.front);
        }
        else
        {
            return input.splitter(newline);
        }
    }
    else
    {
        static if (nl == Newline.unix)
        {
            import splitter_ex : splitterASCIIAmong;
            return input.splitterASCIIAmong!('\n');
        }
        else static if (nl == Newline.win)
        {
            import std.algorithm: splitter;
            return input.splitter("\r\n");
        }
        else static if (nl == Newline.mac)
        {
            import splitter_ex : splitterASCIIAmong;
            return input.splitterASCIIAmong!('\r');
        }
        else static if (nl == Newline.any)
        {
            // TODO Use ctRegex instead?
            import std.regex: splitter, regex;
            return input.splitter(regex("\n|\r\n|\r"));
        }
    }
}

///
@safe pure nothrow unittest
{
    // assert(equal("a\nb".byLine!(Newline.any), ["a", "b"]));
    assert(equal("a\r\nb".byLine!(Newline.win), ["a", "b"]));
    assert(equal("a\rb".byLine!(Newline.mac), ["a", "b"]));
    assert(equal("a\nb".byLine!(Newline.unix), ["a", "b"]));
}

version(unittest)
{
    import std.algorithm: equal;
}
