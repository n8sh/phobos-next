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

/* The new GC is added to the list of available garbage collectors that can be
 * selected via the usual configuration options, e.g. by embedding rt_options
 * into the binary:
 */
extern (C) __gshared string[] rt_options = ["gcopt=gc:mygc"];
