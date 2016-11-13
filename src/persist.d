#!/usr/bin/env rdmd-dev-module

/** Object Data Persistence.
    See also: https://stackoverflow.com/questions/20932921/automatic-object-persistence-in-d/20934647?noredirect=1#20934647
*/
module persist;

// Do not declare two of these on the same line or they'll get mixed up
struct persistent(Type, string file = __FILE__, size_t line = __LINE__)
{
    Type store;
    alias store this;

    @disable this();    // require an initializer

    // with the initializer
    this(Type t)
    {
        // if it is in the file, we should load it here
        // else...
        store = t;
    }

    ~this()
    {
        import std.stdio : writeln;
        // you should actually save it to the file. TODO Import file and
        // calculate its sha1 all at compile-time!
        writeln("Saving ", store, " as key ",
                file,":",line);
    }
}

unittest
{
    persistent!int x = 10;
}
