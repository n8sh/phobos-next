void main()
{
    import std.stdio;
    import std.datetime : MonoTime;

    import hashset : HashSet;
    import hashmap : HashMap;
    import basic_array : BasicArray;

    alias Ix = size_t;
    alias Str = BasicArray!char;
    alias Strs = HashSet!(Str);
    alias IxStr = HashMap!(Ix, Str);

    Strs strs;

    immutable before = MonoTime.currTime();
    foreach (line; File("/usr/share/dict/words").byLine) // TODO make const and fix HashSet.insert
    {
        if (line.length >= 3 &&
            line[$ - 2 .. $] != `'s`)
        {
            strs.insert(Str(line));
        }
    }
    immutable after = MonoTime.currTime();
    const secs = (after - before).total!"msecs";
    const nsecs = (after - before).total!"nsecs";
    writef("Insertion: %1.2s ms, %3.1f ns/op", secs, cast(double)nsecs / strs.length);

}
