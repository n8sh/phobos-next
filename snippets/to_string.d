/** Test multiple overloads of toString()
 */
module to_string;

class Int64Node
{
    @safe pure nothrow:

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

@safe pure unittest
{
    auto i = new Int64Node(42);
    assert(i.toString == `42`);
}
