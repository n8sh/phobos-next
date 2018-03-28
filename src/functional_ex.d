import std.traits;
import std.conv;
import std.variant;

struct CMatch(T...)
    if(T.length == 1)
{
    alias U = typeof(T[0]);
    static bool match(Variant v)
    {
        if (auto p = v.peek!U)
            return *p == T[0];
        return false;
    }
}

/** Pattern Matching.
    See_Also: http://forum.dlang.org/post/ijjthwfezebkszkzrcgt@forum.dlang.org
 */
auto ref match(Handlers...)(Variant v)
{
    foreach (Handler; Handlers)
    {
        alias P = Parameters!Handler;
        static if (P.length == 1)
        {
            static if (isInstanceOf!(CMatch, P[0]))
            {
                if (P[0].match(v))
                    return Handler(P[0].init);
            }
            else
            {
                if (auto p = v.peek!(P[0]))
                    return Handler(*p);
            }
        }
        else
        {
            return Handler();
        }
    }
    assert(0, "No matching pattern");
}

unittest
{
    Variant v = 5;
    string s = v.match!((CMatch!7) => "Lucky number seven",
                        (int n)    => "Not a lucky number: " ~ n.to!string,
                        ()         => "No value found!");
    import std.stdio;
    writeln(s);
}
