import core.gc.gcinterface, core.gc.registry;

extern (C) pragma(crt_constructor) void registerMyGC()
{
    registerGCFactory("mygc", &createMyGC);
}

GC createMyGC()
{
    __gshared instance = new MyGC;
    instance.initialize();
    return instance;
}

class MyGC : GC
{
}
