/** Test multiple overloads of toString()
 */
module to_string;

class Int64Node
{
    @safe:

    @property void toString(scope void delegate(const(char)[]) @safe sink) const
    {
        import std.conv : to;
        sink(toString());
    }

    pure nothrow:

    this(long data) @nogc
    {
        _data = data;
    }

    @property override string toString() @safe pure const
    {
        import std.conv : to;
        return _data.to!(typeof(return));
    }

    private long _data;
}

@safe pure nothrow unittest
{
    auto i = new Int64Node(42);
    assert(i.toString == `42`);
}

@safe unittest
{
    auto i = new Int64Node(42);
    import std.stdio;
    writeln(i);
}
