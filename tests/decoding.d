/** Decoding Algorithms.
    See also: https://forum.dlang.org/post/na7ov2$1np5$1@digitalmars.com
*/
module decoding;

import std.stdio;
import std.string;
import std.regex;
import std.typecons;
import std.conv;
import std.algorithm;
import std.range;

@safe:

template regexClass(T)
{
    static if (is(T == int))
    {
        // Warning: Treats "012" as int (value 12), not octal (value 10).
        enum regexClass = `[0-9]+`;

    }
    else static if (is(T == char))
    {
        enum regexClass = `.`;

    }
    else static if (is(T == double))
    {
        enum regexClass = `[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?`;

    }
    else
    {
        static assert(0, format("Unsupported type %s", arg));
    }
}

string regexEscape(string s)
{
    // TODO: Expand the array and fix the logic.
    enum specialRegexChars = ['(', ')'];

    return s.map!(c => (specialRegexChars.canFind(c) ? format("[%s]", c) : format("%s",
        c))).joiner.text;
}

auto parseDecodeArgs(Args...)(string matchedElementName)
{
    string regexString;
    string tupleString = "return tuple(";

    size_t selectionId = 1;

    foreach (arg; Args)
    {
        static if (is(arg))
        {
            regexString ~= format("(%s)", regexClass!arg);
            tupleString ~= format("%s[%s].to!%s, ", matchedElementName, selectionId,
                arg.stringof);
            ++selectionId;

        }
        else static if (is(typeof(arg) == string))
        {
            regexString ~= regexEscape(arg);

        }
        else
        {
            static assert(0, format("Unsupported type %s", typeof(arg)));
        }
    }

    tupleString ~= ");";
    return tuple(regexString, tupleString);
}

auto decode(Args...)(string s)
{
    enum parseResult = parseDecodeArgs!Args("e");
    enum r = ctRegex!(parseResult[0]);

    // pragma(msg, parseResult[0]);
    // pragma(msg, parseResult[1]);

    auto matched = s.match(r);

    if (matched)
    {
        foreach (e; matched)
        {
            mixin(parseResult[1]);
        }
    }

    return typeof(return)();
}

unittest
{
    auto t = decode!("(", int, ")", char, "(", double, ")")("(1)-(2.5)");
    static assert(is(typeof(t) == Tuple!(int, char, double)));
    assert(t == Tuple!(int, char, double)(1, '-', 2.5));

    // Create a decoder for repeated use
    auto decoder = (string s) => decode!(int, "/", double)(s);

    // Decode each with the same decoder
    auto decoded = ["1/1.5", "2/2.5", "3/3.5"].map!decoder;

    // writeln(decoded);
}
