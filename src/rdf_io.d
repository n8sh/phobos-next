module rdf_io;

import rdf;

@safe:

struct TurtleFile          // TODO generalize to take template parameter `Range` where0 `isCharsSlice!(typeof(Range.init.front))`
{
    import std.stdio : File;
    this(scope const char[] path) // path file naem usually has extension ".ttl"
    {
        _file = File(path, "r");
    }
    auto byNTriple()
    {
        static struct Result
        {
            File fileCopy;
            @safe:

            bool empty() pure nothrow @nogc
            {
                return true;
            }

            const(char)[] front() return scope
            {
                return null;
            }

            void popFront()
            {

            }
        }
        return Result(_file);
    }
    private File _file;
}

@safe unittest
{
    auto ttf = TurtleFile("");
    foreach (const line; ttf.byNTriple)
    {
    }
}
