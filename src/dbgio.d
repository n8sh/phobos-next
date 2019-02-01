/** Various debug printing tools for debug printing in `@safe pure nothrow @nogc` code.

 * Copyright: Per Nordlöw 2018-.
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors: $(WEB Per Nordlöw)
 *
 * See_Also: https://forum.dlang.org/post/svjjawiezudnugdyriig@forum.dlang.org
 */
module dbgio;

// version = show;

@safe pure:

/** Debug print `Names` all being compile-time strings that are mixed in.
 *
 * Similar to Rust's `dbg` macro introduced in version 1.32.
 *
 * See_Also: https://blog.rust-lang.org/2019/01/17/Rust-1.32.0.html#the-dbg-macro
 * See_Also: https://forum.dlang.org/post/svjjawiezudnugdyriig@forum.dlang.org
 */
mixin template dump(Names...)
{
    auto dump =
    {
        import std.stdio : stderr, write;
        debug write(__FILE__, ":", __LINE__, ": Info: ");
        foreach (immutable i, name; Names)
        {
            debug write(typeof(name).stringof, " ", name, ": ", mixin(name), (i < Names.length-1) ? ", " : "\n");
        }
        return false;
    }();
}

///
@safe pure unittest
{
    const int x = 42;
    int[2] y = [42, 43];
    mixin dump!("x", "y");
}

/** Debug print `args` followed by a newline.
 *
 * Similar to Rust's `dbg` macro introduced in version 1.32.
 *
 * See_Also: https://blog.rust-lang.org/2019/01/17/Rust-1.32.0.html#the-dbg-macro
 * See_Also: https://forum.dlang.org/post/svjjawiezudnugdyriig@forum.dlang.org
 */
void dbg(string file = __FILE__,
         uint line = __LINE__,
         string fun = __FUNCTION__,
         Args...)(Args args) @safe pure nothrow @nogc
{
    try
    {
        import std.stdio : stderr, writeln;
        debug stderr.writeln(file, ":", line, ":", " Info: ", args);
    }
    catch (Exception) { }
}

///
@safe pure nothrow @nogc unittest
{
    // int x = 42;
    // dbg("x: ", x);
    static assert(__traits(compiles, { dbg(); })); // ok for dln to discard function qualifiers
}

/** Show the symbol name and variable of $(D Args).
 *
 * See_Also: http://forum.dlang.org/thread/yczwqrbkxdiqijtiynrh@forum.dlang.org?page=1
 *
 * TODO use https://forum.dlang.org/post/ypxsqtddxvdxunsoluas@forum.dlang.org
 *
 * TODO is using this https://dlang.org/changelog/2.079.0.html#default_after_variadic preferred?
 *
 * TODO instead use
 *
 * void show_(Args...)(Args args,
 * string file = __FILE__,
 * uint line = __LINE__,
 * string fun = __FUNCTION__)
 *
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
            debug write(file, ":",line, ":" /* , ": in ",fun */, " debug: ");
            foreach (const i, Arg; Args)
            {
                if (i) debug write(", "); // separator
                debug write(Args[i].stringof, ":", Arg);
            }
            debug writeln();
        }
        catch (Exception) { }
    }
}

version(show)
@safe pure unittest
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
