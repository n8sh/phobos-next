module rdf;

import std.traits: isSomeString, isNarrowString;
import std.range : empty, hasSlicing, hasLength;
import std.string: indexOf, lastIndexOf;
import std.stdio: File;
import std.algorithm.searching: startsWith, endsWith, findSplit;

enum SubjectType { URI, undecodedURI, blankNode }
enum ObjectType { URI, undecodedURI, blankNode, literal }

/**
   Iterate RDF-File $(D rdfFile) by RDF N-Triple.
*/
auto byNTriple(File rdfFile)
{
    import bylinefast: byLineFast, KeepTerminator;
    import std.algorithm: map, filter;
    import std.string: indexOf;
    // TODO support this somehow:
    // if (line.startsWith(`#`))
    // {
    //     if (line.skipOver(`# started `)) { string startTime = line.to!string; }
    //     if (line.skipOver(`# completed `)) { string completionTime = line.to!string; }
    //     continue;
    // }
    return rdfFile.byLineFast(KeepTerminator.no, "\n")
                  .filter!(line => (!line.empty && // skip empty lines
                                    line.indexOf('#') != 0)) // skip comments
                  .map!(line => line.nTriple);
}

/** RDF N-Triple.

   Parameterized on element type $(D ElementType). Use NTriple!(char[]) to avoid
   GC-allocations when parsing files using File.byLine which returns a volatile
   reference to a temporary char[] buffer. If The NTriples are to be stored
   permanently in memory use NTriple!string.

   See_Also: http://wn.wikpedia.org/wiki/N-Triples.
*/
struct NTriple(ElementType)
{
    import std.uri: decodeComponent;
    import std.conv: to;

    /** Construct using subject, predicate, object.

        Fails for:
       - subject: <http://dbpedia.org/resource/CT_Rei_Pel%C3%A9>
       - predicate: <http://xmlns.com/foaf/0.1/homepage>
       - object: <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9>
    */
    this(S)(S subject,
            S predicate,
            S object) if (isSomeString!S)
    in
    {
        assert(!subject.empty);
        assert(!predicate.empty);
        assert(!object.empty);
    }
    body
    {
        import std.uri : URIException;
        // subject
        if (subject.startsWith(`<`)) // URI
        {
            assert(subject.endsWith(`>`));
            SubjectType subjectType;
            try
            {
                this.subject = subject[1 .. $ - 1].decodeComponent.to!ElementType;
                subjectType = SubjectType.URI;
            }
            catch (URIException e)
            {
                this.subject = subject[1 .. $ - 1].to!ElementType;
                subjectType = SubjectType.undecodedURI; // indicate failed decoding
            }
            this.subjectType = subjectType;
        }
        else // blank node
        {
            this.subject = subject.to!ElementType;
            this.subjectType = SubjectType.blankNode;
        }

        // predicate
        assert(predicate.startsWith(`<`));
        assert(predicate.endsWith(`>`));
        this.predicate = predicate[1 .. $ - 1].to!ElementType;

        // object
        if (object.startsWith(`<`)) // URI
        {
            assert(object.endsWith(`>`));
            ObjectType objectType;
            try
            {
                this.object = object[1 .. $ - 1].decodeComponent.to!ElementType;
                objectType = ObjectType.URI;
            }
            catch (URIException e)
            {
                this.object = object[1 .. $ - 1].to!ElementType; // skip decoding
                objectType = ObjectType.undecodedURI; // indicate failed decoding
            }
            this.objectType = objectType;
        }
        else if (object.startsWith(`"`)) // literal
        {
            const endIx = object.lastIndexOf('"');
            assert(endIx != -1); // TODO Use enforce?

            import std.array: replace;
            this.object = object[1 .. endIx].replace(`\"`, `"`).to!ElementType;
            this.objectType = ObjectType.literal;
            auto rest = object[endIx + 1.. $];
            if (!rest.empty && rest[0] == '@')
            {
                this.objectLanguageCode = rest[1 .. $].to!ElementType;
            }
            else
            {
                const hit = rest.indexOf(`^^`);
                if (hit != -1)
                {
                    const objectdataType = rest[hit + 2 .. $];
                    assert(objectdataType.startsWith(`<`));
                    assert(objectdataType.endsWith(`>`));
                    this.objectDataTypeURI = objectdataType[1 .. $ - 1].decodeComponent;
                }
            }
        }
        else                    // blank node
        {
            this.object = object.to!ElementType;
            this.objectType = ObjectType.blankNode;
        }
    }

    ElementType subject;
    ElementType predicate;
    ElementType object;
    ElementType objectLanguageCode;

    const SubjectType subjectType;
    const ObjectType objectType;
    const ElementType objectDataTypeURI;
}

/**
   Decode $(D S) into a an N-Triple.

   TODO Better to call it asNTriple or toNTriple or support conversion via std.conv: to?
 */
NTriple!S nTriple(S)(S s) if (isSomeString!S)
{
    assert(s.length >= 4);

    // strip suffix: either ` .` or `.`
    if (s.endsWith(`.`)) s = s[0 .. $ - 1];
    if (s.endsWith(` `)) s = s[0 .. $ - 1];

    // subject
    const ix0 = s.indexOf(` `);
    const subject = s[0 .. ix0];
    s = s[ix0 + 1 .. $];

    // predicate URI
    const ix1 = s.indexOf(` `);
    const predicate = s[0 .. ix1];
    s = s[ix1 + 1 .. $];

    return typeof(return)(subject, predicate, s);
}
alias asNTriple = nTriple;
alias toNTriple = nTriple;

unittest
{
    const x = `<http://dbpedia.org/resource/180%C2%B0_(Gerardo_album)> <http://dbpedia.org/ontology/artist> <http://dbpedia.org/resource/Gerardo_Mej%C3%ADa> .`;
    const t = x.nTriple;

    assert(t.subject == `http://dbpedia.org/resource/180°_(Gerardo_album)`);
    assert(t.subjectType == SubjectType.URI);

    assert(t.predicate == `http://dbpedia.org/ontology/artist`);

    assert(t.object == `http://dbpedia.org/resource/Gerardo_Mejía`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectType.URI);
}

unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup" .`;
    const t = x.nTriple;

    assert(t.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(t.subjectType == SubjectType.URI);

    assert(t.predicate == `http://xmlns.com/foaf/0.1/name`);

    assert(t.object == `Chatham Cup`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectType.literal);
}

unittest
{
    const x = `<http://dbpedia.org/resource/1950_Chatham_Cup> <http://xmlns.com/foaf/0.1/name> "Chatham Cup"@en .`;
    const t = x.nTriple;

    assert(t.subject == `http://dbpedia.org/resource/1950_Chatham_Cup`);
    assert(t.subjectType == SubjectType.URI);

    assert(t.predicate == `http://xmlns.com/foaf/0.1/name`);

    assert(t.object == `Chatham Cup`);
    assert(t.objectLanguageCode == `en`);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectType.literal);
}

unittest
{
    const x = `<http://dbpedia.org/resource/007:_Quantum_of_Solace> <http://dbpedia.org/ontology/releaseDate> "2008-10-31"^^<http://www.w3.org/2001/XMLSchema#date> .`;
    const t = x.nTriple;

    assert(t.subject == `http://dbpedia.org/resource/007:_Quantum_of_Solace`);
    assert(t.subjectType == SubjectType.URI);

    assert(t.predicate == `http://dbpedia.org/ontology/releaseDate`);

    assert(t.object == `2008-10-31`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI == `http://www.w3.org/2001/XMLSchema#date`);
    assert(t.objectType == ObjectType.literal);
}

unittest
{
    const x = `<http://dbpedia.org/resource/Ceremony_(song)> <http://dbpedia.org/ontology/bSide> "\"In a Lonely Place\"".`;
    const t = x.nTriple;

    assert(t.subject == `http://dbpedia.org/resource/Ceremony_(song)`);
    assert(t.subjectType == SubjectType.URI);

    assert(t.predicate == `http://dbpedia.org/ontology/bSide`);

    assert(t.object == `"In a Lonely Place"`);
    assert(t.objectLanguageCode is null);
    assert(t.objectDataTypeURI is null);
    assert(t.objectType == ObjectType.literal);
}

/**
   Iterate Range by RDF N-Triple.
*/
auto byNTriple(R)(R r)
    if ((hasSlicing!R && hasLength!R || isNarrowString!R))
{
    import std.algorithm: map, filter;
    import std.string: indexOf;
    import std.algorithm.iteration : splitter;
    return r.splitter("\n")
            .filter!(line => line.indexOf('#') != 0) // skip comments
            .map!(line => line.nTriple);
}

unittest
{
    const x = `<http://dbpedia.org/resource/16_@_War> <http://xmlns.com/foaf/0.1/name> "16 @ War"@en .
<http://dbpedia.org/resource/CT_Rei_Pel%C3%A9> <http://xmlns.com/foaf/0.1/homepage> <http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9> .`;
    auto t = x.byNTriple;

    assert(t.front.subject == `http://dbpedia.org/resource/16_@_War`);
    assert(t.front.subjectType == SubjectType.URI);
    assert(t.front.predicate == `http://xmlns.com/foaf/0.1/name`);
    assert(t.front.object == `16 @ War`);
    assert(t.front.objectLanguageCode == `en`);
    assert(t.front.objectDataTypeURI is null);
    assert(t.front.objectType == ObjectType.literal);

    t.popFront();

    assert(t.front.subjectType == SubjectType.URI);
    assert(t.front.predicate == `http://xmlns.com/foaf/0.1/homepage`);
    assert(t.front.object == `http://www.santosfc.com.br/clube/default.asp?c=Sedes&st=CT%20Rei%20Pel%E9`);
    assert(t.front.objectType == ObjectType.undecodedURI);
}
