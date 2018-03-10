#!/usr/bin/env rdmd-dev-module

import algorithm_ex: overlap;

/** Show Format. */
enum Format { Text, HTML, };

@trusted void browse(Args...)(string[] names, inout (Args) args) {
    return fshow(Format.HTML, names, args);
}
@trusted void show(Args...)(string[] names, inout (Args) args) {
    return fshow(Format.Text, names, args);
}

/** Group-Aligned Show of Slices `args`.

    Copyright: Per Nordlöw 2014-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)

    Use as debug print in algorithms where we need to present slices and their
    relations.

    TODO Calculate `names` from `args` or `args` from `names` using Mixins.
    TODO Generalize to Ranges other than slices!?
    TODO Support HTML
*/
@trusted void fshow(Args...)(Format format, string[] names, inout (Args) args)
{
    import std.stdio: wr = write, wrln = writeln;
    import std.algorithm: map, reduce, min, max;
    import std.range : repeat, join;
    import std.string: leftJustify, rightJustify;
    import std.conv: to;

    // determine overlap
    static if (args.length == 2) {
        const a01 = overlap(args[0], args[1]);
    }

    // maximum variable name length
    const namesLengthMax = names.map!"a.length".reduce!max;

    // smallest slice pointer
    static if (true) { // if all args of same type
    }
    static if (args.length == 2) { auto unionPtr = min(args[0].ptr, args[1].ptr); }
    static if (args.length == 3) { auto unionPtr = min(args[0].ptr, args[1].ptr, args[2].ptr); }
    static if (args.length == 4) { auto unionPtr = min(args[0].ptr, args[1].ptr, args[2].ptr, args[3].ptr); }

    // calculate maximum length of number
    auto elementLengthMax = size_t.min;
    size_t[][args.length] elementOffsets; // element offsets
    string[][args.length] elementStrings_;
    foreach (ix, arg; args) {
        auto elementStrings = arg.map!(to!string); // calculate lengths of elements as strings
        elementLengthMax = elementStrings.map!"a.length".reduce!max; // length of longest number as string
        elementOffsets[ix] = new size_t[arg.length];
    }

    // print arguments
    foreach (ix, arg; args) { // for each argument
        enum sep = ", ";      // separator
        const off = arg.ptr - unionPtr; // offset
        wr(names[ix].leftJustify(namesLengthMax), ": "); // name header
        if (off) {
            wr(" ".repeat(1 + (off + 1) * sep.length +
                          off * namesLengthMax).join);
        }
        foreach (elt; arg) {
            wr(to!string(elt).rightJustify(namesLengthMax), sep);
        }
        wrln("");
    }

    // print overlaps
    static if (args.length == 2) {
        /* wrln("overlap(", names[0], ",", names[1], "): ", a01); */
    }
    static if (args.length == 3) {
        foreach (i, arg; args) {
            enum j = (i+1) % args.length;
            /* wrln("overlap(" ~ names[i].leftJustify(namesLengthMax) ~ ", " ~ names[j].leftJustify(namesLengthMax) ~ "): ", overlap(args[i], args[j])); */
        }
    }
    wrln("");
}
unittest {
    auto x = [-11111, -11, 22, 333333];

    auto x01 = x[0..1];
    auto x12 = x[1..2];
    auto x23 = x[2..3];
    auto x34 = x[3..4];

    show(["x12", "x"], x12, x);
    show(["x", "x12"], x, x12);
    show(["x", "x12", "x23"], x, x12, x23);
    show(["x", "x23", "x12", "x34"], x, x23, x12, x34);
}
