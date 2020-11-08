@safe pure unittest
{
    const dch1 = cast(dchar)0x000000a0;
    const dch2 = '\u00a0';
    assert(dch1 == dch2);
}
