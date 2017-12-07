#!/usr/bin/env rdmd-dev

import backtrace.backtrace;
import std.stdio;

string encodeHTML(Char)(Char c) @safe pure if (isSomeChar!Char)
{
    import std.conv: to;
    if      (c == '&')  return "&amp;"; // ampersand
    else if (c == '<')  return "&lt;"; // less than
    else if (c == '>')  return "&gt;"; // greater than
    else if (c == '\"') return "&quot;"; // double quote
//		else if (c == '\'')
//			return ("&#39;"); // if you are in an attribute, it might be important to encode for the same reason as double quotes
    // FIXME: should I encode apostrophes too? as &#39;... I could also do space but if your html is so bad that it doesn't
    // quote attributes at all, maybe you deserve the xss. Encoding spaces will make everything really ugly so meh
    // idk about apostrophes though. Might be worth it, might not.
    else if (0 < c && c < 128)
        return to!string(cast(char)c);
    else
        return "&#" ~ to!string(cast(int)c) ~ ";";
}

/** Copied from arsd.dom */
auto encodeHTML(string data) @safe pure
{
    import std.utf: byDchar;
    import std.algorithm: joiner, map;
    return data.byDchar.map!encodeHTML.joiner("");
}

void main(string[] args)
{
    import std.stdio: stderr;
    backtrace.backtrace.install(stderr);
    immutable ubyte[] x = [101, 120, 32, 83, 111, 102, 116, 119, 97, 114, 101, 32, 50, 48, 49, 49, 0, 88, 89, 90, 32, 0, 0, 0, 0, 0, 0, 181, 90, 0, 0, 188, 103, 0, 0, 146, 48, 109, 102, 116, 50, 0, 0, 0, 0, 4, 3, 9, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0, 2, 36, 4, 29, 5, 218, 7, 105, 8, 217];
    writeln((cast(string)x).encodeHTML); // triggers range violation
}
