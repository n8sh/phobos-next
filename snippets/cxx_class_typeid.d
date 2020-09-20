extern(C++) class C
{
    auto typename()
    {
        return typeid(this).name; // Error: Runtime type information is not supported for `extern(C++)` classes
    }
}
