/// See_Also: https://forum.dlang.org/post/bmzybbvdelgspizwfcmq@forum.dlang.org
@safe pure unittest
{
    enum dch0 = dchar(0xa0a0);
    enum dch1 = cast(dchar)0xa0a0;
    enum dch2 = '\ua0a0';
    assert(dch0 == dch1);
    assert(dch1 == dch2);
}
