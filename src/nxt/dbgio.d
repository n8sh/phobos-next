/** Various debug printing tools for debug printing in `@safe pure nothrow @nogc` code.
 *
 * Copyright: Per Nordlöw 2018-.
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors: $(WEB Per Nordlöw)
 *
 * See_Also: https://forum.dlang.org/post/svjjawiezudnugdyriig@forum.dlang.org
 */
module nxt.dbgio;

// version = show;

@safe pure:

/** Debug print `Names` all being compile-time strings that are mixed in.
 *
 * Similar to Rust's `dbg` macro introduced in version 1.32.
 *
 * See_Also: https://blog.rust-lang.org/2019/01/17/Rust-1.32.0.html#the-dbg-macro
 * See_Also: https://forum.dlang.org/post/svjjawiezudnugdyriig@forum.dlang.org
 */
mixin template dumpObseleted(Names...)
{
    auto dumpObseleted =
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
version(show)
@safe pure unittest
{
    const int x = 42;
    int[2] y = [42, 43];
    mixin dumpObseleted!("x", "y");
}

/** Debug print `args` followed by a newline.
 *
 * Similar to Rust's `dbg` macro introduced in version 1.32.
 *
 * See_Also: https://blog.rust-lang.org/2019/01/17/Rust-1.32.0.html#the-dbg-macro
 * See_Also: https://forum.dlang.org/post/svjjawiezudnugdyriig@forum.dlang.org
 */
void dbg(Args...)(Args args,
                  const string file = __FILE__,
                  const uint line = __LINE__,
                  const string fun = __FUNCTION__) @safe pure nothrow @nogc
{
    import std.stdio : stderr, writeln;
    try
        debug stderr.writeln(file, ":", line, ":", " Info: ", args);
    catch (Exception) { }
}

///
@safe pure nothrow @nogc unittest
{
    int x = 42;
    dbg("x: ", x);
    static assert(__traits(compiles, { dbg(); })); // ok for dln to discard function qualifiers
}

/** Show the symbol name and variable of $(D Args).
 *
 * See_Also: http://forum.dlang.org/thread/yczwqrbkxdiqijtiynrh@forum.dlang.org?page=1
 *
 * TODO: use https://forum.dlang.org/post/ypxsqtddxvdxunsoluas@forum.dlang.org
 *
 * TODO: is using this https://dlang.org/changelog/2.079.0.html#default_after_variadic preferred?
 *
 * TODO: instead use
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
            debug
            {
                write(file, ":",line, ":" /* , ": in ",fun */, " debug: ");
                foreach (const i, Arg; Args)
                {
                    if (i)
                        write(", "); // separator
                    write(Args[i].stringof, ":", Arg);
                }
                writeln();
            }
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

import std.stdio;

/** Debug dump arguments `args` to standard error output (`stderr`).
 *
 * See_Also: https://forum.dlang.org/post/myxzyfgtcewixwbhvalp@forum.dlang.org
 */
template dump(args...)
{
    alias dbgImpl!(args).print dump;
}

template dbgImpl(args...)
{
    import std.traits : isBuiltinType, isAggregateType, FieldNameTuple;

    private void print(string file = __FILE__, uint line = __LINE__, string fun = __FUNCTION__)
    {
        static foreach (arg; args)
        {
            static if (isBuiltinType!(typeof(arg)))
            {
                debug stderr.writefln("[%s:%s (%s)] %s = %s",
                                      file, line, fun,
                                      __traits(identifier, arg), arg);
            }
            else static if (isAggregateType!(typeof(arg)))
            {
                debug stderr.writefln("[%s:%s (%s)] %s = %s",
                                      file, line, fun,
                                      __traits(identifier, arg),
                                      toDbgString(arg));
            }
        }
    }

    private string toDbgString(Arg)(Arg o)
    {
        string dbgstr = "(";
        import std.format;
        static foreach(f; FieldNameTuple!(typeof(o)))
        {
            static if (isBuiltinType!(typeof(__traits(getMember, o, f))))
                dbgstr ~= format("%s:%s, ", f, __traits(getMember, o, f));
            else static if (isAggregateType!(typeof(__traits(getMember, o, f))))
                dbgstr ~= format("%s = %s, ", f, toDbgString(__traits(getMember, o, f)));
        }
        return dbgstr[0..$-2] ~ ")";
    }
}

///
version(show)
@safe pure unittest
{
    struct Bar { auto c = 'c';}
    struct Foo { int s = 2; bool b = false; Bar bar;}
    class FooBar { int t; Foo f; }

    int i;
    float f = 3.14;
    string s = "some string";
    Foo foo;
    Bar bar;

    dump!(i, f, s, foo, 1+3, foo, bar);

    // prints:
    // [dump.d:54 (dump.main)] i = 0
    // [dump.d:54 (dump.main)] f = 3.14
    // [dump.d:54 (dump.main)] s = some string
    // [dump.d:54 (dump.main)] foo = (s:2, b:false, bar = (c:c))
    // [dump.d:54 (dump.main)] _ = 4
    // [dump.d:54 (dump.main)] foo = (s:2, b:false, bar = (c:c))
    // [dump.d:54 (dump.main)] bar = (c:c)
    // [dump.d:54 (dump.main)] fb = (t:0, f = (s:2, b:false, bar = (c:c)))
}
