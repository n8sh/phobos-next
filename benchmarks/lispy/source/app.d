import lispy;

import std.path : expandTilde;
import std.stdio : write, writeln;
import std.file: dirEntries, SpanMode;
import std.conv : to;
import std.datetime.stopwatch : StopWatch, AutoStart, Duration;
import std.utf;
import std.algorithm.searching : endsWith, canFind;
import std.path : pathSplitter;

import file_ex : rawReadNullTerminated;

/** Read all SUO-KIF files (.kif) located under `rootDirPath`.
 */
void benchmarkSUMO(const scope string rootDirPath)
{
    auto totalSw = StopWatch(AutoStart.yes);
    auto entries = dirEntries(rootDirPath.expandTilde, SpanMode.breadth, false); // false: skip symlinks
    foreach (dent; entries)
    {
        const filePath = dent.name;
        try
        {
            benchmarkSUMOFile(filePath);
        }
        catch (std.utf.UTFException e)
        {
            import std.file : read;
            writeln("Failed because of invalid UTF-8 encoding starting with ", filePath.read(16));
        }
    }

    totalSw.stop();
    writeln(`Reading all files took `, totalSw.peek.to!Duration);
}

/** Benchark reading of SUMO. */
void benchmarkSUMOFile(const scope string filePath) @safe
{
    if (filePath.endsWith(`.kif`) &&
        !filePath.pathSplitter.canFind(`.git`)) // invalid UTF-8 encodings
    {
        write(`Reading SUO-KIF `, filePath, ` ... `);
        import std.file : readText;
        auto sw = StopWatch(AutoStart.yes);
        foreach (const ref topExpr; LispParser(cast(LispParser.Input)filePath.expandTilde.rawReadNullTerminated())) // TODO avoid cast
        {
            // TOOD use topExpr
        }
        sw.stop();
        writeln(`took `, sw.peek.to!Duration);
    }
}

/** Benchark reading of Emacs-Lisp. */
void benchmarkEmacsLisp(const scope string filePath) @safe
{
    import std.stdio;
    import std.file : readText;
    import std.conv : to;
    import std.datetime.stopwatch : StopWatch, AutoStart, Duration;

    const includeComments = false;
    const includeWhitespace = false;

    const disallowEmptyLists = false;

    write(`Reading Emacs-Lisp `, filePath, ` ... `);
    auto sw = StopWatch(AutoStart.yes);
    foreach (const ref expr; LispParser(cast(LispParser.Input)filePath.expandTilde.rawReadNullTerminated(),
                                        includeComments,
                                        includeWhitespace,
                                        disallowEmptyLists))
    {
        // writeln(expr);
    }
    writeln(`took `, sw.peek.to!Duration);
}

void main(string[] args)
{
    benchmarkEmacsLisp(`~/Work/knet/knowledge/relangs.el`);
    benchmarkSUMO(`~/Work/sumo`);
}
