#!/usr/bin/rdmd

pragma(lib, "curl");

void readNordicNames()
{
    import std.string: strip;
    import std.range: empty;
    import std.path: expandTilde, buildNormalizedPath;
    import std.file: dirEntries, SpanMode, readText;
    import std.stdio: writeln, File;
    import std.algorithm: joiner, findSplitBefore, findSplitAfter, endsWith, startsWith, until, split, findSkip;
    import std.conv: to;
    import std.net.curl;
    import std.array: array;
    import arsd.dom;
    import skip_ex: skipOverBack;

    const dirPath = `~/Knowledge/nordic_names/wiki`;
    const fixedPath = dirPath.expandTilde.buildNormalizedPath;

    auto outFile = File("nordic_names.txt", "w");

    size_t nameIx = 0;
    foreach (fileName; fixedPath.dirEntries(SpanMode.shallow))
    {
        writeln(`Scanning `, fileName, ` ...`);
        auto doc = new Document(readText(fileName));

        string[] fields;

        // decode name and gender
        Gender gender;
        if (doc.title.empty)
        {
            continue;
        }
        else
        {
            auto name = doc.title.until(`-`).array.strip;
            if (name.skipOverBack(` m`))
            {
                gender = Gender.male;
            }
            else if (name.skipOverBack(` f`))
            {
                gender = Gender.female;
            }

            fields ~= name.to!string;
            fields ~= gender.to!string;

            writeln("Name: ", name);
            writeln("Gender: ", gender);
        }

        const author = doc.getMeta("author");
        if (!author.empty) writeln(`Author: `, author);

        // foreach (a; doc.querySelectorAll(`a[href]`)) {}

        foreach (h2; doc.querySelectorAll(`h2`)) { /* writeln(h2.children); */ }

        size_t pIx = 0;
        string[] langs;
        string explanation;
        string seeAlso;
        string stat;

        foreach (p; doc.querySelectorAll(`h2 + p`))
        {
            const text = p.innerText.strip;
            switch (pIx)
            {
                case 0:
                    langs = text.split;
                    fields ~= langs.joiner(alternativesSeparator.to!string).to!string;
                    writeln("Languages: ", langs);
                    break;
                case 1:
                    explanation = text.findSplitBefore(`[`)[0].strip;
                    fields ~= explanation;
                    writeln("Explanation: ", explanation);
                    break;
                case 2:
                    seeAlso = text.findSplitAfter(`See `)[1].findSplitBefore(`[`)[0].strip;
                    fields ~= seeAlso;
                    writeln("See: ", seeAlso);
                    break;
                case 3:
                    if (!text.startsWith(`No recent statistics trend found in databases for`))
                    {
                        stat = text;
                        writeln("Stat: ", stat);
                    }
                    break;
                default:
                    writeln(` -- "`, text, `"`); // innerText or directText
                    break;
            }
            ++pIx;
        }

        const line = fields.joiner(roleSeparator.to!string).to!string;
        writeln(line);
        outFile.writeln(line);

        writeln(``);

        if (nameIx >= 1000) break;
        ++nameIx;
    }
}

void main()
{
    readNordicNames();
}
