/** Parsing of RDF-files.
 *
 * Currently supports N-Triples (.nt).
 *
 * Planned support for RDF Turtle (.ttl).
 *
 * TODO can we make inout operator only on the members of the returned `NTriple` in `parseNTriple`?
 * TODO parse Turtle .ttl-files (https://en.wikipedia.org/wiki/Turtle_(syntax))
 * TODO parse N-Quads for use in Wikidata
 * TODO parse RDF/XML
 *
 * See_Also: https://en.wikipedia.org/wiki/Resource_Description_Framework
 * See_Also: https://en.wikipedia.org/wiki/Turtle_(syntax)
 * See_Also: https://en.wikipedia.org/wiki/N-Triples#N-Quads
 *
 * See_Also: https://www.ida.liu.se/~robke04/include/publications.shtml
 */
module nxt.rdf_io;

import nxt.rdf;

@safe:

struct TurtleFile          // TODO generalize to take template parameter `Range` where0 `isCharArray!(typeof(Range.init.front))`
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
