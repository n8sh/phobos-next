#!/usr/bin/env rdmd-dev-module

/** Various debug tools.
    Copyright: Per Nordlöw 2014-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
*/
module dbg;

version = show;

void assumeNogc(alias Func, T...)(T xs) @nogc
{
    import std.traits : isFunctionPointer, isDelegate, functionAttributes, FunctionAttribute, SetFunctionAttributes, functionLinkage;
    static auto assumeNogcPtr(T)(T f) if (isFunctionPointer!T ||
                                          isDelegate!T)
    {
        enum attrs = functionAttributes!T | FunctionAttribute.nogc;
        return cast(SetFunctionAttributes!(T, functionLinkage!T, attrs)) f;
    } {}
    assumeNogcPtr(&Func!T)(xs);
};

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

// unittest
// {
//     int x = 5;
//     int y = 3;
//     int z = 15;
//     mixin dump!("x", "y");  // x = 5, y = 3
//     mixin dump!("z");       // z = 15
//     mixin dump!("x+y");     // x+y = 8
//     mixin dump!("x+y < z"); // x+y < z = true
// }

@trusted:

/* http://stackoverflow.com/questions/19413340/escaping-safety-with-debug-statements */
debug auto trustedPureDebugCall(alias fn, Args...) (Args args) pure
{
    debug return fn(args);
}

nothrow pure:

void dln(string file = __FILE__, uint line = __LINE__, string fun = __FUNCTION__, Args...)(Args args)
{
    try
    {
        import std.stdio : writeln;
        debug assumeNogc!writeln(file, ":", line, ":", " debug: ", args);
    }
    catch (Exception) { }
}

version(show) @safe pure nothrow @nogc unittest
{
    dln("x:", " ", 12);
}

// /** Show the symbol name and variable of $(D Args).
//     See also: http://forum.dlang.org/thread/yczwqrbkxdiqijtiynrh@forum.dlang.org?page=1
//  */
// template show(Args...)
//     if (Args.length >= 1)
// {
//     void show(string file = __FILE__, uint line = __LINE__, string fun = __FUNCTION__)
//     {
//         import std.stdio: write, writeln;
//         try
//         {
//             debug write(file, ":",line, ":" /* , ": in ",fun */, " debug: ");
//             foreach (const i, Args; Args)
//             {
//                 if (i) debug write(", "); // separator
//                 debug write(Args[i].stringof, ":", Args);
//             }
//             debug writeln();
//         }
//         catch (Exception) { }
//     }
// }

// version(show) unittest
// {
//     int x = 11;
//     int y = 12;
//     int z = 13;
//     show!x;
//     show!y;
//     show!z;
// }

// version(show) unittest
// {
//     int x = 11, y = 12, z = 13;
//     show!(x, y, z);
// }
