@safe pure unittest
{
    const dch1 = cast(dchar)0x0000a0a0;
    const dch2 = '\ua0a0';
    assert(dch1 == dch2);
}
