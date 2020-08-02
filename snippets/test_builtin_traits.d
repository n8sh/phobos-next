@safe pure unittest
{
    static assert(__traits(isStaticArray, int[3]));
    // static assert(__traits(isDynamicArray, int[]));
}
