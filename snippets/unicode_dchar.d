/// See_Also: https://forum.dlang.org/post/bmzybbvdelgspizwfcmq@forum.dlang.org
@safe pure unittest
{
    enum dch0 = dchar(0xa0a0);
    enum dch1 = cast(dchar)0xa0a0;
    enum dch2 = '\ua0a0';
    assert(dch0 == dch1);
    assert(dch1 == dch2);
}

pure unittest
{
    import std.typecons : Yes;
    import std.utf : encode;
    scope char[4] ch4;
    const replacementChar = cast(dchar) 0x110000;
    const n = encode!(Yes.useReplacementDchar)(ch4, replacementChar);
    import std.stdio;
    debug writeln(cast(ubyte[])ch4[0 .. n]);
}
