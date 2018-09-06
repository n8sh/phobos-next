module knuthhash64;

@safe pure nothrow @nogc:

/** Knuth hash.
 *
 * See_Also: https://stackoverflow.com/a/9545731/683710
 */
struct KnuthHash64
{
    @safe pure nothrow @nogc:

    /** (Re)initialize.
     */
    void start()
    {
        _seed = _seedValue;
    }

    /** Use this to feed the hash with data.
     *
     * Also implements the $(XREF range, OutputRange) interface for $(D ubyte)
     * and $(D const(ubyte)[]).
     */
    void put(scope const(ubyte)[] data...) @trusted
    {
        foreach (elt; data)
        {
            _result += elt;
            _result *= _factor;
        }
    }

    /** Returns: the finished hash.
     *
     * This also calls $(LREF start) to reset the internal _state.
     */
    ubyte[8] finish() @trusted
    {
        typeof(return) bytes = (cast(ubyte*)&_result)[0 .. typeof(return).sizeof];
        start();
        return bytes;
    }

    ulong get()
    {
        return _result;
    }

private:
    private enum _seedValue = 3074457345618258791UL;
    private enum _factor = 3074457345618258799UL;
    ulong _seed = _seedValue;
    ulong _result;
}

/** Compute knuthHash-64 of input `data`, with optional seed `seed`.
 */
ulong knuthhash64Of(in ubyte[] data, ulong seed = 0)
{
    auto hash = KnuthHash64(seed);
    hash.put(data);
    return hash.get();
}

/** Compute knuthHash-64 of input string `data`, with optional seed `seed`.
 */
ulong knuthhash64Of(in char[] data, ulong seed = 0)
    @trusted
{
    return knuthhash64Of(cast(ubyte[])data, seed);
}

/// test simple `knuthhash64Of`
unittest
{
    assert(knuthhash64Of("") == KnuthHash64._seedValue);
}

version(unittest)
{
    import std.digest : isDigest;
    static assert(isDigest!(KnuthHash64));
}
