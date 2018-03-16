#!/usr/bin/env rdmd-dev-module

/** MathML.
    Copyright: Per Nordlöw 2018-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
*/
module mathml;

import std.traits : isScalarType, isFloatingPoint;

import rational : Rational; // TODO Can we turn this dep into a duck type dep?
import languages : MarkupLang;

/** Horizontal Alignment. */
enum HAlign { left, center, right }

/** Generic case. */
string toMathML(T)(T x) @trusted /* pure nothrow */
    if (isScalarType!T &&
        !isFloatingPoint!T)
{
    import std.conv : to;
    return to!string(x);
}

/** Return x in $(D MarkupLang) format.
    See also: http://forum.dlang.org/thread/awkynfizwqjnbilgddbh@forum.dlang.org#post-awkynfizwqjnbilgddbh:40forum.dlang.org
    See also: https://developer.mozilla.org/en-US/docs/Web/MathML/Element/mn
    See also: https://developer.mozilla.org/en-US/docs/Web/MathML/Element/msup
 */
string toML(T)(T x,
               bool usePowPlus = false,
               bool useLeadZeros = false,
               MarkupLang mlang = MarkupLang.HTML) @trusted /* pure nothrow */
    if (isFloatingPoint!T)
{
    import std.conv : to;
    import std.algorithm : findSplit;
    const parts = to!string(x).findSplit("e"); // TODO Use std.bitmanip.FloatRep instead
    if (parts[2].length >= 1)
    {
        // mantissa
        const mant = parts[0];

        // TODO These format fixes for the exponent are not needed if we use
        // std.bitmanip.FloatRep instead

        // exponent
        auto exp = ((!usePowPlus &&
                      parts[2][0] == '+') ? // if leading plus
                     parts[2][1..$] : // skip plus
                     parts[2]); // otherwise whole
        import algorithm_ex : dropWhile;
        auto zexp = useLeadZeros ? exp : exp.dropWhile('0');

        final switch (mlang)
        {
            case MarkupLang.unknown:
            case MarkupLang.HTML:
                return (mant ~ `&middot;` ~ `10` ~ `<msup>` ~ zexp ~ `</msup>`);
            case MarkupLang.MathML:
                return (`<math>` ~ mant ~ `&middot;` ~
                        `<msup>` ~
                        `<mn>10</mn>` ~
                        `<mn mathsize="80%">` ~ zexp ~ `</mn>` ~
                        `</msup>` ~
                        `</math>`);
        }
        /* NOTE: This doesn't work in Firefox. Why? */
        /* return (`<math>` ~ parts[0] ~ `&middot;` ~ */
        /*         `<apply><power/>` ~ */
        /*         `<ci>10</ci>` ~ */
        /*         `<cn>` ~ parts[2] ~ `</cn>` */
        /*         `</apply>` ~ */
        /*         `</math>`); */
    }
    else
    {
        return parts[0];
    }
}

auto toMathML(T)(T x, bool usePowPlus = false, bool useLeadZeros = false) @trusted /* pure nothrow */
    if (isFloatingPoint!T)
{
    return toML(x, usePowPlus, useLeadZeros, MarkupLang.MathML);
}

auto toHTML(T)(T x, bool usePowPlus = false, bool useLeadZeros = false) @trusted /* pure nothrow */
    if (isFloatingPoint!T)
{
    return toML(x, usePowPlus, useLeadZeros, MarkupLang.HTML);
}

/**
   Returns: MathML Representation of $(D x).
   See also: https://developer.mozilla.org/en-US/docs/Web/MathML/Element/mfrac
 */
string toMathML(T)(Rational!T x,
                   bool bevelled = false,
                   HAlign numalign = HAlign.center,
                   HAlign denomalign = HAlign.center,
                   string href = null) @safe pure
{
    import std.conv : to;
    return (`<math><mfrac` ~
            (bevelled ? ` bevelled="true"` : ``) ~
            (numalign != HAlign.center ? ` numalign="` ~ to!string(numalign) ~ `"` : ``) ~
            (denomalign != HAlign.center ? ` denomalign="` ~ to!string(denomalign) ~ `"` : ``) ~
            `><mi>`
            ~ to!string(x.numerator) ~ `</mi><mi>` ~
            to!string(x.denominator) ~
            `</mi></mfrac></math>`);
}

unittest
{
    alias Q = Rational;
    auto x = Q!ulong(11, 22);
    // import dbgio : dln;
    /** dln(x.toMathML); */
    /** dln(x.toMathML(true)); */
    /** dln(x.toMathML(true, HAlign.left, HAlign.left)); */
}
