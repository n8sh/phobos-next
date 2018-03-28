#!/usr/bin/env rdmd-dev-module

/** Various debug printing tools for debug printing in `@safe pure nothrow @nogc` code.
    Copyright: Per Nordlöw 2018-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
*/
module dbgio;

import assuming : assumeNogc;

mixin template dump(Names ... )
{
    auto _unused_dump =
    {
        import std.stdio : writeln, write;
        foreach(i,name; Names)
        {
            write(name, " = ", mixin(name), (i<Names.length-1)?", ": "\n");
        }
        return false;
    }();
}

@trusted:

/* http://stackoverflow.com/questions/19413340/escaping-safety-with-debug-statements */
debug auto trustedPureDebugCall(alias fn, Args...) (Args args)
    pure
{
    debug return fn(args);
}

nothrow pure:

/** Debug print `args` followed by a newline. */
void dln(string file = __FILE__,
         uint line = __LINE__,
         string fun = __FUNCTION__,
         Args...)(Args args)
{
    try
    {
        import std.stdio : writeln;
        debug assumeNogc!writeln(file, ":", line, ":", " debug: ", args);
    }
    catch (Exception) { }
}

/** Show the symbol name and variable of $(D Args).
    See_Also: http://forum.dlang.org/thread/yczwqrbkxdiqijtiynrh@forum.dlang.org?page=1

    TODO is using this https://dlang.org/changelog/2.079.0.html#default_after_variadic preferred?

    TODO instead use

    void show_(Args...)(Args args,
                    string file = __FILE__,
                    uint line = __LINE__,
                    string fun = __FUNCTION__)

 */
template show(Args...)
    if (Args.length >= 1)
{
    void show(string file = __FILE__,
              uint line = __LINE__,
              string fun = __FUNCTION__)
    {
        import std.stdio: write, writeln;
        try
        {
            debug assumeNogc!write(file, ":",line, ":" /* , ": in ",fun */, " debug: ");
            foreach (const i, Arg; Args)
            {
                if (i) debug assumeNogc!write(", "); // separator
                debug assumeNogc!write(Args[i].stringof, ":", Arg);
            }
            debug assumeNogc!writeln();
        }
        catch (Exception) { }
    }
}

// version = show;

version(show) unittest
{
    const x = 11;
    const y = 12;
    const z = 13;
    show!x;
    show!y;
    show!z;
}

version(show) unittest
{
    const x = 11, y = 12, z = 13;
    show!(x, y, z);
}
