/+dub.sdl:
 dependency "mir-algorithm" version="~>1.1.1"
 +/
void main()
{
    import mir.ndslice;
    import std.stdio : writefln;

    enum fmt = "%(%(%.2f %)\n%)\n";

    // Magic sqaure.
    // `a` is lazy, each element is computed on-demand.
    auto a = magic(5).as!float;
    writefln(fmt, a);

    // 5x5 grid on sqaure [1, 2] x [0, 1] with values x * x + y.
    // `b` is lazy, each element is computed on-demand.
    auto b = linspace!float([5, 5], [1f, 2f], [0f, 1f]).map!"a * a + b";
    writefln(fmt, b);

    // allocate a 5 x 5 contiguous matrix
    auto c = slice!float(5, 5);

    c[] = transposed(a + b / 2); // no memory allocations here
    // 1. b / 2 - lazy element-wise operation with scalars
    // 2. a + (...) - lazy element-wise operation with other slices
    // Both slices must be `contiguous` or one-dimensional.
    // 3. transposed(...) - trasposes matrix view. The result is `universal` (numpy-like) matrix.
    // 5. c[] = (...) -- performs element-wise assignment.
    writefln(fmt, c);
}
