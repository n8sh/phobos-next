#!/usr/bin/env rdmd-dev-module

/** Various debug tools.
    Copyright: Per Nordlöw 2014-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
*/
module dbg;

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

nothrow:

void contextual_writeln(string file = __FILE__, uint line = __LINE__, string fun = __FUNCTION__, Args...)(Args args)
{
    import std.stdio: writeln;
    try { writeln(file, ":",line, ":"/* , ": in ",fun */, " debug: ", args); }
    catch (Exception) { }
}
alias pln = contextual_writeln;

pure:

void dln(string file = __FILE__, uint line = __LINE__, string fun = __FUNCTION__, Args...)(Args args)
{
    // pretend that writeln is @nogc

    import std.traits : isFunctionPointer, isDelegate, functionAttributes, FunctionAttribute, SetFunctionAttributes, functionLinkage;

    static auto assumeNogc(T)(T f)
        if (isFunctionPointer!T ||
            isDelegate!T)
    {
    	return cast(SetFunctionAttributes!(T, functionLinkage!T,
                                           functionAttributes!T | FunctionAttribute.nogc)) f;
    } {}

    import std.stdio : writeln;
    try { debug assumeNogc((ref Args args) => writeln(file, ":", line, ":", " debug: ",
                                                      args))(args); }
    catch (Exception) { }
}

@safe pure nothrow @nogc unittest
{
    dln("x:", " ", 12);
}

/** Show the symbol name and variable of $(D Args).
    See also: http://forum.dlang.org/thread/yczwqrbkxdiqijtiynrh@forum.dlang.org?page=1
 */
template show(Args...)
    if (Args.length >= 1)
{
    void show(string file = __FILE__, uint line = __LINE__, string fun = __FUNCTION__)
    {
        import std.stdio: write, writeln;
        try
        {
            debug write(file, ":",line, ":" /* , ": in ",fun */, " debug: ");
            foreach (const i, Args; Args)
            {
                if (i) debug write(", "); // separator
                debug write(Args[i].stringof, ":", Args);
            }
            debug writeln();
        }
        catch (Exception) { }
    }
}

// version = show;

version(show) unittest
{
    int x = 11;
    int y = 12;
    int z = 13;
    show!x;
    show!y;
    show!z;
}

version(show) unittest
{
    int x = 11, y = 12, z = 13;
    show!(x, y, z);
}

// uint fwr(A...)(A a)
// {
//     import std.stdio : LockingTextWriter;
//     import std.stdio : stdout;
//     import std.format : formattedWrite;
//     return formattedWrite!(stdout.lockingTextWriter)(a);
// }

// unittest
// {
//     fwr(42, " ", 43);
// }
