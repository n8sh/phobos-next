import std.traits : isCopyable;
import std.meta : AliasSeq;

struct W(T, size_t n)
{
    T value;
}

void main()
{
    alias Ts(uint n) = AliasSeq!(W!(byte, n), W!(ubyte, n),
                                 W!(short, n), W!(ushort, n),
                                 W!(int, n), W!(uint, n),
                                 W!(long, n), W!(ulong, n),
                                 W!(float, n), W!(cfloat, n),
                                 W!(double, n), W!(cdouble, n),
                                 W!(real, n), W!(creal, n),
                                 W!(string, n), W!(wstring, n), W!(dstring, n));

    enum n = 5000;              // number of different sets of instantations of Ts
    static foreach (i; 0 .. n)
    {
        static foreach (T; Ts!(n))
        {
            static assert(isCopyable!(T));
        }
    }
}
