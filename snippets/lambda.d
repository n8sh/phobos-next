int main(string[] args)
{
    auto twice = function (int x) => x * 2;
    auto thrice = (int x) { return 3*x; };
    import std.stdio;
    writeln(thrice(3));
    return 0;
}
