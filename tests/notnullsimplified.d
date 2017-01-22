struct NotNull(T)
{
    private T notNullPayload;
    @disable this();
    private this(T t) {
        assert(t !is null);
        notNullPayload = t;
    }
    inout(T) getNotNullPayload() inout {
        return notNullPayload;
    }
    alias getNotNullPayload this;
    bool opCast(T : bool)() { return notNullPayload !is null; }
}

NotNull!T checkNull(T)(inout(T) t) if(!is(T == typeof(null)))
{
    return NotNull!T(cast(T) t);
}

void test(const NotNull!(int*) nn)
{
    import std.stdio;
    writeln(*nn, " was not null");
}

unittest
{
    import std.stdio;
    int item;
    int* ptr = &item;

    NotNull!(int*) lol = void;
    lol = checkNull(ptr = null);

    if(auto nn = ptr.checkNull) {
        test(nn);
    } else {
        writeln("Null");
    }
}
