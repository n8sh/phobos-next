module uri_algorithm;

bool skipOverURIProtocolPrefix(scope ref inout(char)[] uri) @safe pure nothrow @nogc
{
    import array_algorithm : skipOver;
    const(char)[] tmp = uri;
    if (tmp.skipOver(`http`))
    {
        tmp.skipOver('s');  // optional s
        if (tmp.skipOver(`://`))
        {
            uri = uri[$ - tmp.length .. $]; // do it
            return true;
        }
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    auto uri = "http://www.sunet.se";
    assert(uri.skipOverURIProtocolPrefix());
    assert(uri  == "www.sunet.se");
}

///
@safe pure nothrow @nogc unittest
{
    auto uri = "https://www.sunet.se";
    assert(uri.skipOverURIProtocolPrefix());
    assert(uri  == "www.sunet.se");
}
