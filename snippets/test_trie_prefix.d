#!/usr/bin/env rdmd

import std.algorithm.comparison : equal;
import trie : radixTreeSet;
import dbgio : dln;

void main(string[] args)
{
    alias Key = string;
    auto set = radixTreeSet!(Key);

    set.clear();
    set.insert(`-----1`);
    set.insert(`-----2`);
    const string[] expected2 = [`1`, `2`];
    assert(set.prefix(`-----`)
              .equal(expected2[]));

    set.insert(`-----3`);
    const string[3] expected3 = [`1`, `2`, `3`];
    assert(set.prefix(`-----`)
              .equal(expected3[]));

    set.clear();
    set.insert(`____alphabet`);
    set.insert(`____alpha`);
    assert(set.prefix(`____alpha`)
              .equal([``,
                      `bet`]));

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

    enum show = false;
    if (show)
    {
        import std.stdio : writeln;

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
