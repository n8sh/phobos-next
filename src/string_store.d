module string_store;

import variant_arrays;

@safe pure:

/** Store sets of strings with different lengths compactly.
 */
struct StringStore
{
    @safe pure:
    void insertBack(string value) nothrow @nogc
    {
        switch (value.length)
        {
            // TODO static foreach
        case 1: _store.insertBack(cast(char[1])value[0 .. 1]); break;
        case 2: _store.insertBack(cast(char[2])value[0 .. 2]); break;
        case 3: _store.insertBack(cast(char[3])value[0 .. 3]); break;
        case 4: _store.insertBack(cast(char[4])value[0 .. 4]); break;
        case 5: _store.insertBack(cast(char[5])value[0 .. 5]); break;
        case 6: _store.insertBack(cast(char[6])value[0 .. 6]); break;
        case 7: _store.insertBack(cast(char[7])value[0 .. 7]); break;
        case 8: _store.insertBack(cast(char[8])value[0 .. 8]); break;
        default:
            _store.insertBack(value);
        }
    }
private:
    VariantArrays!(char[1],
                   char[2],
                   char[3],
                   char[4],
                   char[5],
                   char[6],
                   char[7],
                   char[8],
                   string) _store;
}

@safe pure nothrow @nogc unittest
{
    StringStore ss;
    ss.insertBack("alpha");
    ss.insertBack("beta");
}
