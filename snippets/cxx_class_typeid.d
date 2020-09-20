// https://dlang.slack.com/archives/C7FT6TPFG/p1600605823003100

extern(C++) class C
{
    auto typename()
    {
        return typeid(this).name; // Error: Runtime type information is not supported for `extern(C++)` classes
    }
}
