void main()
{
    import std.stdio;
    import std.datetime : MonoTime;
    import digestx.fnv : FNV;

    import hashset : HashSet;
    import hashmap : HashMap;
    import basic_array : BasicArray;

    alias Ix = size_t;
    alias Str = BasicArray!(char); // TODO use uint as length
    alias Strs = HashSet!(Str, null, FNV!(64, true));
    alias IxStr = HashMap!(Ix, Str, null, FNV!(64, true));

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

    immutable secs = (after - before).total!"msecs";
    immutable nsecs = (after - before).total!"nsecs";

    immutable n = strs.length;
    immutable binCounts = strs.binCounts;

    writef("Insertion: n:%s %1.2s ms, %3.1f ns/op binCounts:S:%s,L:%s",
           n,
           secs,
           cast(double)nsecs / n,
           binCounts.smallCount,
           binCounts.largeCount);

}
