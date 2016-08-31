#!/usr/bin/env rdmd

// TODO uncomment test code at trie.d:4329 when this works
void main(string[] args)
{
    import std.algorithm.comparison : equal;
    import trie : radixTreeSet;
    import dbgio : dln;

    alias Key = string;
    auto set = radixTreeSet!(Key);

    set.clear();
    set.insert(`alphabet`);
    set.insert(`alpha`);
    set.print;
    dln(set[]);
    // assert(set.prefix(`alpha`)
    //           .equal([``,
    //                   `bet`]));

    set.clear();
    set.insert(`alphabet`);
    set.insert(`alpha`);
    set.insert(`a`);
    set.insert(`al`);
    set.insert(`all`);
    set.insert(`allies`);
    set.insert(`ally`);
    set.insert(`étude`);
    set.insert(`études`);

    enum show = true;
    if (show)
    {
        import dbgio;
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

    assert(set.prefix(`étude`)
              .equal([``,
                      `s`]));

}
