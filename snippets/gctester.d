import core.stdc.stdio: printf;
import core.memory : GC;
import std.stdio;

void main(string[] args)
{
    const n = 1024*1024;
    alias T = long;
    size_t xx = 0;
    foreach (i; 0 .. n)
    {
        T* x = new T(i);
        xx ^= cast(size_t)x;
        printf("x: i:%d, p:%p\n", i, x);
        x = null;
        GC.collect();
    }
}
