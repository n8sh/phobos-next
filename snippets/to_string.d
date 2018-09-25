/** Test multiple overloads of toString()
 */
module to_string;

import std.stdio;

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
    long _data;
}

void main(string[] args)
{
    auto i = new Int64Node(42);
    assert(i.toString == `42`);
}
