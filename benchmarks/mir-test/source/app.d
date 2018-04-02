import std.stdio;
import mir.ndslice;

void main()
{
    auto matrix = slice!int(2, 2);
    assert(matrix == [[0, 0], [0, 0]]);
    pragma(msg, typeof(matrix));
    writeln(matrix[]);
}
