/** Parsing of RDF-files.
 *
 * Currently supports N-Triples (.nt).
 *
 * Planned support for RDF Turtle (.ttl).
 *
 * TODO parse Turtle .ttl-files (https://en.wikipedia.org/wiki/Turtle_(syntax))
 * TODO parse N-Quads for use in Wikidata
 * TODO parse RDF/XML
 *
 * See_Also: https://en.wikipedia.org/wiki/Resource_Description_Framework
 * See_Also: https://en.wikipedia.org/wiki/Turtle_(syntax)
 * See_Also: https://en.wikipedia.org/wiki/N-Triples#N-Quads
 *
 * See_Also: https://www.ida.liu.se/~robke04/include/publications.shtml
 */
module rdf;

import dbgio;

enum SubjectFormat { URI, undecodedURI, blankNode }
enum ObjectFormat { URI, undecodedURI, blankNode, literal }

@safe:

/** Decode $(D S) into a an N-Triple.
 *
 * TODO Better to call it `asNTriple` or `toNTriple` or support conversion via
 * std.conv: to?
 */
auto parseNTriple(scope return inout(char)[] s) @safe pure
{
    import std.string : indexOf, lastIndexOf;

    /** RDF N-Triple.
     *
     * Parameterized on element type $(D Chars). Use NTriple!(char[]) to avoid
     * GC-allocations when parsing files using File.byLine which returns a volatile
     * reference to a temporary char[] buffer. If The NTriples are to be stored
     * permanently in memory use NTriple!string.
     *
     * See_Also: https://en.wikipedia.org/wiki/N-Triples
     */
    static struct NTriple
    {
        import std.uri : decodeComponent;
        import std.conv : to;
        import array_algorithm : skipOver, skipOverBack, startsWith, endsWith, canFind;

        alias Chars = const(char)[];

        /** Construct using subject, predicate, object.
         *
         * Fails for:
         * - subject: <http://dbpedia.org/resource/CT_Rei_Pel%C3%A9>
         * - predicate: <http://xmlns.com/foaf/0.1/homepage>
         * - object: <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9>
         */
        void parse() @safe pure scope // TODO nothrow
        {
            import std.uri : URIException;

            // subject
            if (subject.skipOver('<')) // URI
            {
                const ok = subject.skipOverBack('>');
                assert(ok);
                SubjectFormat subjectType = SubjectFormat.URI;
                if (subject.canFind('%')) // only if escape-sequences are found
                {
                    try
                    {
                        subject = subject.decodeComponent.to!Chars;
                    }
                    catch (Exception e)
                    {
                        subjectType = SubjectFormat.undecodedURI; // indicate failed decoding
                    }
                }
            }
            else
            {
                subjectType = SubjectFormat.blankNode;
            }

            // predicate
            assert(predicate.startsWith('<'));
            assert(predicate.endsWith('>'));
            predicate = predicate[1 .. $ - 1];

            // object
            if (object.skipOver('<')) // URI
            {
                const ok = object.skipOverBack('>');
                assert(ok);
                ObjectFormat objectType = ObjectFormat.URI;
                if (object.canFind('%')) // only if escape-sequences are found
                {
                    try
                    {
                        object = object.decodeComponent.to!Chars; // TODO do GC-allocation only when a real decoding happens
                    }
                    catch (Exception e)
                    {
                        objectType = ObjectFormat.undecodedURI; // indicate failed decoding
                    }
                }
            }
            else if (object.startsWith('"')) // literal
            {
                const endIx = object.lastIndexOf('"');
                assert(endIx != -1);

                import std.array : replace;
                auto content = object[1 .. endIx].replace(`\"`, `"`).to!Chars;
                objectType = ObjectFormat.literal;

                auto rest = object[endIx + 1 .. $];

                if (rest.length && rest[0] == '@')
                {
                    objectLanguageCode = object[endIx + 2 .. $]; // TODO why can't we use rest[1.. $] here?
                }
                else
                {
                    const hit = rest.indexOf(`^^`);
                    if (hit != -1)
                    {
                        const objectdataType = rest[hit + 2 .. $];
                        assert(objectdataType.startsWith('<'));
                        assert(objectdataType.endsWith('>'));
                        objectDataTypeURI = objectdataType[1 .. $ - 1].decodeComponent;
                    }
                }
                object = content;
            }
            else
            {
                objectType = ObjectFormat.blankNode;
            }
        }

        Chars subject;
        Chars predicate;
        Chars object;

        Chars objectLanguageCode;

        Chars objectDataTypeURI;
        SubjectFormat subjectType;
        ObjectFormat objectType;
    }

    import array_algorithm : skipOverBack;

    assert(s.length >= 4);

    // strip suffix
    s.skipOverBack('.');
    s.skipOverBack(' ');

    // subject
    const ix0 = s.indexOf(' ');
    const subject = s[0 .. ix0];
    s = s[ix0 + 1 .. $];

    // predicate URI
    const ix1 = s.indexOf(' ');
    const predicate = s[0 .. ix1];
    s = s[ix1 + 1 .. $];

    auto nt = inout(NTriple)(subject, predicate, s);

    (cast(NTriple)nt).parse();  // hack to make `inout` work
    return nt;
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/180%C2%B0_(Gerardo_album)> <http://dbpedia.org/ontology/artist> <http://dbpedia.org/resource/Gerardo_Mej%C3%ADa> .`;
    const t = x.parseNTriple;

    assert(t.subject == `http://dbpedia.org/resource/180°_(Gerardo_album)`);
    assert(t.subjectType == SubjectFormat.URI);
    assert(t.predicate == `http://dbpedia.org/ontology/artist`);
    assert(t.object == `http://dbpedia.org/resource/Gerardo_Mejía`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectFormat.URI);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup"@en .`;
    const t = x.parseNTriple;

    assert(t.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(t.subjectType == SubjectFormat.URI);
    assert(t.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(t.object == `Chatham Cup`);
    assert(t.objectLanguageCode == `en`);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup" .`;
    const t = x.parseNTriple;

    assert(t.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(t.subjectType == SubjectFormat.URI);
    assert(t.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(t.object == `Chatham Cup`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/007:_Quantum_of_Solace> <http://dbpedia.org/ontology/releaseDate> "2008-10-31"^^<http://www.w3.org/2001/XMLSchema#date> .`;
    const t = x.parseNTriple;

    assert(t.subject == `http://dbpedia.org/resource/007:_Quantum_of_Solace`);
    assert(t.subjectType == SubjectFormat.URI);

    assert(t.predicate == `http://dbpedia.org/ontology/releaseDate`);

    assert(t.object == `2008-10-31`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI == `http://www.w3.org/2001/XMLSchema#date`);
    assert(t.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/Ceremony_(song)> <http://dbpedia.org/ontology/bSide> "\"In a Lonely Place\"".`;
    const t = x.parseNTriple;

    assert(t.subject == `http://dbpedia.org/resource/Ceremony_(song)`);
    assert(t.subjectType == SubjectFormat.URI);
    assert(t.predicate == `http://dbpedia.org/ontology/bSide`);
    assert(t.object == `"In a Lonely Place"`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectFormat.literal);
}

version(none):

import std.stdio : File;

/** Iterate RDF-File $(D rdfFile) by RDF N-Triple.
 */
auto byNTriple(File rdfFile,
               const char commentPrefix = '#')
{
    import bylinefast : byLineFast, KeepTerminator;
    import std.algorithm.iteration : map, filter;
    // TODO support this somehow:
    // if (line.startsWith('#'))
    // {
    //     if (line.skipOver(`# started `)) { string startTime = line.to!string; }
    //     if (line.skipOver(`# completed `)) { string completionTime = line.to!string; }
    //     continue;
    // }
    return rdfFile.byLineFast(KeepTerminator.no, "\n")
                  .filter!(line =>
                           (line.length >= 1 &&
                            line[0] != commentPrefix)) // skip comments
                  .map!(line => line.parseNTriple);
}

import std.traits : isNarrowString;

import std.range.primitives : hasSlicing, hasLength;

/** Iterate Range by RDF N-Triple.
 */
auto byNTriple(R)(R r)
if ((hasSlicing!R && hasLength!R ||
     isNarrowString!R))
{
    import std.algorithm.iteration : map, filter;
    import std.string : indexOf;
    import splitter_ex : splitterASCIIAmong;
    return r.splitterASCIIAmong!('\n')               // TODO support multiple newlines
            .filter!(line => line.indexOf('#') != 0) // skip comments. TODO non-decoding
            .map!(line => line.parseNTriple);
}

@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/16_@_War> <http://xmlns.com/foaf/0.1/name> "16 @ War"@en .
<http://dbpedia.org/resource/CT_Rei_Pel%C3%A9> <http://xmlns.com/foaf/0.1/homepage> <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9> .`;
    auto t = x.byNTriple;

    assert(t.front.subject == `http://dbpedia.org/resource/16_@_War`);
    assert(t.front.subjectType == SubjectFormat.URI);
    assert(t.front.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(t.front.object == `16 @ War`);
    assert(t.front.objectLanguageCode == `en`);
    assert(t.front.objectDataTypeURI is null);
    assert(t.front.objectType == ObjectFormat.literal);

    t.popFront();

    assert(t.front.subjectType == SubjectFormat.URI);
    assert(t.front.predicate == `http://xmlns.com/foaf/0.1/homepage`);
    assert(t.front.object == `http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9`);
    assert(t.front.objectType == ObjectFormat.undecodedURI);
}
