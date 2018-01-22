void main(string[] args)
{
    import std.datetime.stopwatch : benchmark;
    import std.meta : AliasSeq;
    import std.variant : Algebraic;
    import vary;
    import std.stdio : writeln;

    alias Types = AliasSeq!(long, double);
    alias P = PackedVariant!Types;
    alias F = FastVariant!Types;
    alias A = Algebraic!Types;

    writeln(P.sizeof);
    writeln(F.sizeof);
    writeln(A.sizeof);

    void test(T)()
    {
        T x;
        foreach (long i; 0 .. 100)
        {
            x = i;
            auto y = x;
            auto z = y;
            assert(x == y);
            assert(y == z);
        }
    }

    enum n = 10_000;
    const results = benchmark!(test!P,
                               test!F,
                               test!A)(n);
    writeln(results);
}
