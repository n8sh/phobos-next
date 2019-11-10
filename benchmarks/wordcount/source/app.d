void main()
{
    import std.stdio;
    import std.datetime : MonoTime;

    size_t count = 0;
    immutable before = MonoTime.currTime();
    foreach (line; File("/usr/share/dict/words").byLine) // TODO make const and fix HashSet.insert
    {
        count += 1;
    }
    immutable after = MonoTime.currTime();

    immutable secs = (after - before).total!"msecs";
    immutable nsecs = (after - before).total!"nsecs";

    writef("Line: count:%s duration:%1.2smsecs, %3.1fnsecs/op\n",
           count,
           secs,
           cast(double)nsecs / count);

}
