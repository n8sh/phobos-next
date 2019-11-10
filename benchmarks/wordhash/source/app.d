void main()
{
    import std.stdio;
    import std.datetime : MonoTime;
    import std.algorithm : max;
    import digestx.fnv : FNV;

    import sso_string : String = SSOString;
    import open_hashmap_or_hashset : HashSet = OpenHashSet;
    import open_hashmap_or_hashset : HashMap = OpenHashMap;
    import basic_array : Array = BasicArray;

    alias Ix = size_t;
    alias Strs = HashSet!(String);

    auto strs = Strs.withCapacity(72800);

    size_t maxLength = 0;
    immutable before = MonoTime.currTime();
    foreach (line; File("/usr/share/dict/words").byLine) // TODO make const and fix HashSet.insert
    {
        import array_algorithm : endsWith;
        if (!line.endsWith(`'s`))
        {
            strs.insert(String(line));
            maxLength = max(maxLength, line.length);
        }
    }
    immutable after = MonoTime.currTime();

    immutable secs = (after - before).total!"msecs";
    immutable nsecs = (after - before).total!"nsecs";

    immutable n = strs.length;

    writef("Insertion: n:%s maxLength:%s %1.2smsecs, %3.1fnsecs/op",
           n,
           maxLength,
           secs,
           cast(double)nsecs / n);

}
