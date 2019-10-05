import lispy;

import std.path : expandTilde;
import std.stdio : write, writeln;
import std.file: dirEntries, SpanMode;
import std.conv : to;
import std.datetime.stopwatch : StopWatch, AutoStart, Duration;
import std.utf;
import std.algorithm.searching : endsWith, canFind;
import std.path : pathSplitter;

/** Read all SUO-KIF files (.kif) located under `rootDirPath`.
 */
void benchmarkSUMOTreeRead(const scope string rootDirPath)
{
    auto totalSw = StopWatch(AutoStart.yes);
    auto entries = dirEntries(rootDirPath.expandTilde, SpanMode.breadth, false); // false: skip symlinks
    foreach (dent; entries)
    {
        const filePath = dent.name;
        if (filePath.endsWith(`.kif`) &&
            !filePath.pathSplitter.canFind(`.git`)) // invalid UTF-8 encodings
        {
            try
            {
                benchmarkSUMOFileRead(filePath);
            }
            catch (std.utf.UTFException e)
            {
                import std.file : read;
                writeln("Failed because of invalid UTF-8 encoding starting with ", filePath.read(16));
            }
        }
    }
    totalSw.stop();
    writeln(`Reading all files took `, totalSw.peek.to!Duration);
}

/** Benchark reading of SUMO. */
void benchmarkSUMOFileRead(const scope string filePath) @safe
{
    write(`Reading SUO-KIF `, filePath, ` ... `);
    auto sw = StopWatch(AutoStart.yes);
    foreach (const ref expr; LispFileParser(filePath))
    {
        // writeln(expr);
    }
    sw.stop();
    writeln(`took `, sw.peek.to!Duration);
}

/** Benchark reading of Emacs-Lisp. */
void benchmarkEmacsLisp(const scope string filePath) @safe
{
    write(`Reading Emacs-Lisp `, filePath, ` ... `);
    auto sw = StopWatch(AutoStart.yes);
    foreach (const ref expr; LispFileParser(filePath))
    {
        // writeln(expr);
    }
    writeln(`took `, sw.peek.to!Duration);
}

void main(string[] args)
{
    benchmarkEmacsLisp(`~/Work/knet/knowledge/relangs.el`);
    benchmarkSUMOTreeRead(`~/Work/sumo`);
}
