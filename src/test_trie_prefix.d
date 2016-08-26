#!/usr/bin/env rdmd

// TODO uncomment test code at trie.d:4329 when this works
void main(string[] args)
{
    import trie : radixTreeSet;

    alias Key = string;
    auto set = radixTreeSet!(Key);

    set.insert(`alphabet`);
    set.insert(`alpha`);
    set.insert(`a`);
    set.insert(`al`);
    set.insert(`all`);
    set.insert(`allies`);
    set.insert(`ally`);

    enum show = true;
    if (show)
    {
        import dbg;
        import std.stdio;

        foreach (const e; set[])
        {
            dln(`"`, e, `"`);
        }

        writeln();

        foreach (const e; set.prefix(`a`))
        {
            dln(`"`, e, `"`);
        }

        writeln();

        foreach (const e; set.prefix(`all`))
        {
            dln(`"`, e, `"`);
        }
    }

    import std.algorithm : equal;

    assert(set.prefix(`a`)
              .equal([``,
                      `l`,
                      `ll`,
                      `llies`,
                      `lly`,
                      `lpha`,
                      `lphabet`]));

    assert(set.prefix(`all`)
              .equal([``,
                      `ies`,
                      `y`]));

}
