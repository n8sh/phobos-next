int main(string[] args)
{
    auto twice = function (int x) => x * 2;

    auto thrice = (int x) { return 3*x; };
    pragma(msg, __FILE__, "(", __LINE__, ",1): Debug: ", typeof(thrice));

    import std.stdio;
    writeln(thrice(3));

    return 0;
}
