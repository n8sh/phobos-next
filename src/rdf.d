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

enum SubjectFormat { IRI, blankNode }
enum ObjectFormat { IRI, blankNode, literal }

@safe:

/** Decode $(D S) into an RDF N-Triple.
 *
 * See_Also: https://www.w3.org/TR/n-triples/
 */
auto parseNTriple(scope return inout(char)[] s) @safe pure
{
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
        import std.conv : to;
        import array_algorithm : skipOver, skipOverBack, startsWith, endsWith, canFind;

        alias Chars = const(char)[];

        /** Parse `subject`, `predicate` and `object`.
         *
         * Fails for:
         * - subject: <http://dbpedia.org/resource/CT_Rei_Pel%C3%A9>
         * - predicate: <http://xmlns.com/foaf/0.1/homepage>
         * - object: <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9>
         */
        void parse() @safe pure scope nothrow
        {
            // subject
            if (subject.skipOver('<')) // IRI
            {
                const ok = subject.skipOverBack('>');
                assert(ok);
                subjectType = SubjectFormat.IRI;
            }
            else
            {
                subjectType = SubjectFormat.blankNode;
            }

            // predicate
            if (predicate.skipOver('<')) // IRI
            {
                const ok = predicate.skipOverBack('>');
                assert(ok);
                // predicateType = PredicateFormat.IRI;
            }
            else
            {
                // predicateType = PredicateFormat.blankNode;
            }

            // object
            if (object.skipOver('<')) // IRI
            {
                const ok = object.skipOverBack('>');
                assert(ok);
                objectType = ObjectFormat.IRI;
            }
            else if (object.skipOver('"')) // literal
            {
                if (object.length >= 3 && object[$ - 3] == '@')
                {
                    objectLanguageCode = object[$ - 2 .. $];
                    object = object[0 .. $ - 3];
                }
                else
                {
                    import array_algorithm : findSplit;
                    if (auto hit = object.findSplit(`^^`))
                    {
                        const objectdataType = hit.post;
                        assert(objectdataType.startsWith('<'));
                        assert(objectdataType.endsWith('>'));
                        objectDataTypeIRI = objectdataType[1 .. $ - 1];
                        object = hit.pre;
                    }
                }

                const ok = object.skipOverBack('"');
                assert(ok);

                if (object.canFind(`\"`))
                {
                    import std.array : replace;
                    object = object.replace(`\"`, `"`).to!Chars; // TODO avoid?
                }
                objectType = ObjectFormat.literal;
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

        Chars objectDataTypeIRI;
        SubjectFormat subjectType;
        ObjectFormat objectType;
    }

    import array_algorithm : skipOverBack;

    assert(s.length >= 4);

    // strip suffix
    s.skipOverBack('.');
    s.skipOverBack(' ');

    import array_algorithm : indexOf;

    // subject
    const ix0 = s.indexOf(' '); // TODO use array_algorithm.findSplit(' ')
    assert(ix0 != -1);
    const subject = s[0 .. ix0];
    s = s[ix0 + 1 .. $];

    // predicate IRI
    const ix1 = s.indexOf(' '); // TODO use array_algorithm.findSplit(' ')
    assert(ix1 != -1);
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
    const nt = x.parseNTriple;

    assert(nt.subject == `http://dbpedia.org/resource/180%C2%B0_(Gerardo_album)`);
    assert(nt.subjectType == SubjectFormat.IRI);
    assert(nt.predicate == `http://dbpedia.org/ontology/artist`);
    assert(nt.object == `http://dbpedia.org/resource/Gerardo_Mej%C3%ADa`);
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectType == ObjectFormat.IRI);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup"@en .`;
    const nt = x.parseNTriple;

    assert(nt.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(nt.subjectType == SubjectFormat.IRI);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(nt.object == `Chatham Cup`);
    assert(nt.objectLanguageCode == `en`);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup" .`;
    const nt = x.parseNTriple;

    assert(nt.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(nt.subjectType == SubjectFormat.IRI);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(nt.object == `Chatham Cup`);
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/007:_Quantum_of_Solace> <http://dbpedia.org/ontology/releaseDate> "2008-10-31"^^<http://www.w3.org/2001/XMLSchema#date> .`;
    const nt = x.parseNTriple;

    assert(nt.subject == `http://dbpedia.org/resource/007:_Quantum_of_Solace`);
    assert(nt.subjectType == SubjectFormat.IRI);

    assert(nt.predicate == `http://dbpedia.org/ontology/releaseDate`);

    assert(nt.object == `2008-10-31`);
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI == `http://www.w3.org/2001/XMLSchema#date`);
    assert(nt.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/Ceremony_(song)> <http://dbpedia.org/ontology/bSide> "\"In a Lonely Place\"".`;
    const nt = x.parseNTriple;

    assert(nt.subject == `http://dbpedia.org/resource/Ceremony_(song)`);
    assert(nt.subjectType == SubjectFormat.IRI);
    assert(nt.predicate == `http://dbpedia.org/ontology/bSide`);
    assert(nt.object == `"In a Lonely Place"`);
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/16_@_War> <http://xmlns.com/foaf/0.1/name> "16 @ War"@en .`;
    auto nt = x.parseNTriple;

    assert(nt.subject == `http://dbpedia.org/resource/16_@_War`);
    assert(nt.subjectType == SubjectFormat.IRI);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(nt.object == `16 @ War`);
    assert(nt.objectLanguageCode == `en`);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectType == ObjectFormat.literal);
}

///
@safe pure unittest
{
    const x = `<http://dbpedia.org/resource/CT_Rei_Pel%C3%A9> <http://xmlns.com/foaf/0.1/homepage> <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9> .`;
    auto nt = x.parseNTriple;

    assert(nt.subjectType == SubjectFormat.IRI);
    assert(nt.subject == `http://dbpedia.org/resource/CT_Rei_Pel%C3%A9`);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/homepage`);
    assert(nt.object == `http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9`);
    assert(nt.objectType == ObjectFormat.IRI);
}
