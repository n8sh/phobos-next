import core.stdc.stdio: printf;
import core.memory : GC;
import std.stdio;

void main(string[] args)
{
    const n = 1;
    alias T = long;
    foreach (i; 0 .. n)
    {
        T* x = new T(i);
        printf("x: i:%d, p:%p\n", i, x);
        x = null;
        GC.collect();
    }
}
