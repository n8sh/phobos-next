module inplace_substitution;

void substituteInPlace(scope char[] source, char from, char to)
    @safe pure nothrow @nogc
{
    foreach (ref char ch; source)
    {
        if (ch == from)
        {
            ch = to;
        }
    }
}
