/** Algorithms operating on URIs including URLs.
 *
 * See_Also: https://en.wikipedia.org/wiki/URL
 */
module uri_algorithm;

/** Try skip over leading protocol prefix part of an URL `url`.
 *
 * Currently only skips the prefixes `http://` and `https://`.
 *
 * Returns: `true` iff skip was performed, `false` otherwise.
 *
 * See_Also: https://en.wikipedia.org/wiki/URL
 */
bool skipOverURLProtocolPrefix(scope ref inout(char)[] url) @safe pure nothrow @nogc
{
    import array_algorithm : skipOver;
    const(char)[] tmp = url;
    if (tmp.skipOver(`http`))
    {
        tmp.skipOver('s');  // optional s
        if (tmp.skipOver(`://`))
        {
            url = url[$ - tmp.length .. $]; // do it
            return true;
        }
    }
    return false;
}

///
@safe pure nothrow @nogc unittest
{
    auto url = "http://www.sunet.se";
    assert(url.skipOverURLProtocolPrefix());
    assert(url  == "www.sunet.se");
}

///
@safe pure nothrow @nogc unittest
{
    auto url = "https://www.sunet.se";
    assert(url.skipOverURLProtocolPrefix());
    assert(url  == "www.sunet.se");
}
