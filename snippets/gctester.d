import core.stdc.stdio: printf;
import core.memory : GC;
import std.stdio;

void main(string[] args)
{
    const n = 10;
    foreach (i; 0 .. n)
    {
        int* x = new int(i);
        printf("x: i:%d, p:%p\n", i, x);
        x = null;
        GC.collect();
    }
}
