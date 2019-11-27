/** RDF-data model and algorithsm.
 *
 * Currently supports N-Triples (.nt).
 *
 * Planned support for RDF Turtle (.ttl) statements (either single-line or multi-line).
 *
 * TODO can we make inout operator only on the members of the returned `NTriple` in `parseNTriple`?
 * TODO parse Turtle .ttl-files (https://en.wikipedia.org/wiki/Turtle_(syntax))
 * TODO parse N-Quads for use in Wikidata
 *
 * See_Also: https://en.wikipedia.org/wiki/Resource_Description_Framework
 * See_Also: https://en.wikipedia.org/wiki/Turtle_(syntax)
 * See_Also: https://en.wikipedia.org/wiki/N-Triples#N-Quads
 *
 * See_Also: https://www.ida.liu.se/~robke04/include/publications.shtml
 *
 * TODO decode `subject` and `object` (in `Db.getExprURI`) only when their types are IRIs?
 */
module nxt.rdf;

enum SubjectFormat
{
    IRI, // See_Also: https://en.wikipedia.org/wiki/Internationalized_Resource_Identifier
    blankNode
}
enum ObjectFormat
{
    IRI, // See_Also: https://en.wikipedia.org/wiki/Internationalized_Resource_Identifier
    blankNode,
    literal
}

@safe pure nothrow @nogc:

/** RDF N-Triple (data model).
 *
 * Parameterized on element type $(D Chars). Use NTriple!(char[]) to avoid
 * GC-allocations when parsing files using File.byLine which returns a volatile
 * reference to a temporary char[] buffer. If The NTriples are to be stored
 * permanently in memory use NTriple!string.
 *
 * See_Also: https://en.wikipedia.org/wiki/N-Triples
 */
struct NTriple
{
    import nxt.array_algorithm : skipOver, skipOverBack, startsWith, endsWith;

    alias Chars = const(char)[];

    /** Parse `subject`, `predicate` and `object`.
     *
     * Fails for:
     * - subject: <http://dbpedia.org/resource/CT_Rei_Pel%C3%A9>
     * - predicate: <http://xmlns.com/foaf/0.1/homepage>
     * - object: <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9>
     */
    void parse() @safe pure scope nothrow @nogc
    {
        // subject: Ref: https://www.w3.org/TR/n-triples/#grammar-production-subject
        if (subject.skipOver('<')) // IRIREF (https://www.w3.org/TR/n-triples/#grammar-production-IRIREF)
        {
            const ok = subject.skipOverBack('>');
            assert(ok);
            subjectFormat = SubjectFormat.IRI;
        }
        else                // BLANK_NODE_LABEL
        {
            subjectFormat = SubjectFormat.blankNode;
        }

        // predicate: Ref: https://www.w3.org/TR/n-triples/#grammar-production-predicate
        assert(predicate.startsWith('<'));
        assert(predicate.endsWith('>'));
        predicate = predicate[1 .. $ - 1]; // IRIREF (https://www.w3.org/TR/n-triples/#grammar-production-IRIREF)

        // object: Ref: https://www.w3.org/TR/n-triples/#grammar-production-object
        if (object.skipOver('<')) // IRIREF (https://www.w3.org/TR/n-triples/#grammar-production-IRIREF)
        {
            const ok = object.skipOverBack('>');
            assert(ok);
            objectFormat = ObjectFormat.IRI;
        }
        else if (object.skipOver('"')) // literal (https://www.w3.org/TR/n-triples/#grammar-production-literal)
        {
            // import std.ascii : isLower;
            import nxt.array_algorithm : findSplit;
            if (const split = object.findSplit(`"@`))
            {
                objectLanguageCode = split.post;
                object = split.pre;
            }
            else if (auto hit = object.findSplit(`"^^`))
            {
                const objectdataType = hit.post;
                assert(objectdataType.startsWith('<'));
                assert(objectdataType.endsWith('>'));
                objectDataTypeIRI = objectdataType[1 .. $ - 1];
                object = hit.pre;
            }
            else
            {
                const ok = object.skipOverBack('"');
                if (!ok)
                {
                    assert("No matching double-quote in object ");
                }
                assert(ok);
            }

            // dbg(`object:"`, object, `" lang:"`, objectLanguageCode, `" typeIRI:"`, objectDataTypeIRI, `"`);
            objectFormat = ObjectFormat.literal;
        }
        else                // BLANK_NODE_LABEL (https://www.w3.org/TR/n-triples/#grammar-production-BLANK_NODE_LABEL)
        {
            objectFormat = ObjectFormat.blankNode;
        }
    }

    Chars subject;
    Chars predicate;

    Chars object;
    Chars objectLanguageCode;
    Chars objectDataTypeIRI;

    SubjectFormat subjectFormat;
    ObjectFormat objectFormat;
}

/** Decode `line` into an RDF N-Triple.
 *
 * See_Also: https://www.w3.org/TR/n-triples/
 */
auto parseNTriple(scope return inout(char)[] line)
{
    import nxt.array_algorithm : skipOverBack, indexOf;

    assert(line.length >= 4);

    // strip suffix
    line.skipOverBack('.');
    line.skipOverBack(' ');

    // subject IRI
    const ix0 = line.indexOf(' '); // TODO use array_algorithm.findSplit(' ')
    assert(ix0 != -1);
    const subject = line[0 .. ix0];
    line = line[ix0 + 1 .. $];

    // predicate IRI
    const ix1 = line.indexOf(' '); // TODO use array_algorithm.findSplit(' ')
    assert(ix1 != -1);
    const predicate = line[0 .. ix1];
    line = line[ix1 + 1 .. $];

    auto nt = inout(NTriple)(subject, predicate, line);
    (cast(NTriple)nt).parse();  // hack to make `inout` work
    return nt;
}

///
@safe pure nothrow @nogc unittest
{
    const x = `<http://dbpedia.org/resource/180%C2%B0_(Gerardo_album)> <http://dbpedia.org/ontology/artist> <http://dbpedia.org/resource/Gerardo_Mej%C3%ADa> .`;
    auto nt = x.parseNTriple;
    static assert(is(typeof(nt.subject) == immutable(string))); // TODO should be `string` or `const(char)[]`
    static assert(is(typeof(nt.predicate) == immutable(string))); // TODO should be `string` or `const(char)[]`
    static assert(is(typeof(nt.object) == immutable(string))); // TODO should be `string` or `const(char)[]`
    assert(nt.subject == `http://dbpedia.org/resource/180%C2%B0_(Gerardo_album)`);
    assert(nt.subjectFormat == SubjectFormat.IRI);
    assert(nt.predicate == `http://dbpedia.org/ontology/artist`);
    assert(nt.object == `http://dbpedia.org/resource/Gerardo_Mej%C3%ADa`);
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectFormat == ObjectFormat.IRI);
}

///
@safe pure nothrow @nogc unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup"@en .`;
    const nt = x.parseNTriple;
    assert(nt.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(nt.subjectFormat == SubjectFormat.IRI);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(nt.object == `Chatham Cup`);
    assert(nt.objectLanguageCode == `en`);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectFormat == ObjectFormat.literal);
}

///
@safe pure nothrow @nogc unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup" .`;
    const nt = x.parseNTriple;
    assert(nt.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(nt.subjectFormat == SubjectFormat.IRI);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(nt.object == `Chatham Cup`);
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectFormat == ObjectFormat.literal);
}

///
@safe pure nothrow @nogc unittest
{
    const x = `<http://dbpedia.org/resource/007:_Quantum_of_Solace> <http://dbpedia.org/ontology/releaseDate> "2008-10-31"^^<http://www.w3.org/2001/XMLSchema#date> .`;
    const nt = x.parseNTriple;
    assert(nt.subject == `http://dbpedia.org/resource/007:_Quantum_of_Solace`);
    assert(nt.subjectFormat == SubjectFormat.IRI);
    assert(nt.predicate == `http://dbpedia.org/ontology/releaseDate`);
    assert(nt.object == `2008-10-31`);
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI == `http://www.w3.org/2001/XMLSchema#date`);
    assert(nt.objectFormat == ObjectFormat.literal);
}

///
@safe pure nothrow @nogc unittest
{
    const x = `<http://dbpedia.org/resource/Ceremony_(song)> <http://dbpedia.org/ontology/bSide> "\"In a Lonely Place\"".`;
    const nt = x.parseNTriple;
    assert(nt.subject == `http://dbpedia.org/resource/Ceremony_(song)`);
    assert(nt.subjectFormat == SubjectFormat.IRI);
    assert(nt.predicate == `http://dbpedia.org/ontology/bSide`);
    assert(nt.object == `\"In a Lonely Place\"`); // to be unescaped
    assert(nt.objectLanguageCode is null);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectFormat == ObjectFormat.literal);
}

///
@safe pure nothrow @nogc unittest
{
    const x = `<http://dbpedia.org/resource/16_@_War> <http://xmlns.com/foaf/0.1/name> "16 @ War"@en .`;
    auto nt = x.parseNTriple;
    assert(nt.subject == `http://dbpedia.org/resource/16_@_War`);
    assert(nt.subjectFormat == SubjectFormat.IRI);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(nt.object == `16 @ War`);
    assert(nt.objectLanguageCode == `en`);
    assert(nt.objectDataTypeIRI is null);
    assert(nt.objectFormat == ObjectFormat.literal);
}

///
@safe pure nothrow @nogc unittest
{
    const x = `<http://dbpedia.org/resource/CT_Rei_Pel%C3%A9> <http://xmlns.com/foaf/0.1/homepage> <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9> .`;
    auto nt = x.parseNTriple;
    assert(nt.subjectFormat == SubjectFormat.IRI);
    assert(nt.subject == `http://dbpedia.org/resource/CT_Rei_Pel%C3%A9`);
    assert(nt.predicate == `http://xmlns.com/foaf/0.1/homepage`);
    assert(nt.object == `http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9`);
    assert(nt.objectFormat == ObjectFormat.IRI);
}