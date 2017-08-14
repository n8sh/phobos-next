/** SUO-KIF File Format.

    See: https://en.wikipedia.org/wiki/Knowledge_Interchange_Format
    See: http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/sigma/suo-kif.pdf
*/
module suokif;

/** SUO-KIF (Lisp) Token Type. */
enum TOK
{
    unknown,

    leftParen,
    rightParen,

    symbol,

    stringLiteral,

    oneDirInference,            // one-directional inference
    biDirInference,             // bi-directional inference

    equivalence,

    lispComma,
    lispBackQuote,
    lispQuote,

    variable,
    variableList,               // one or more variables (parameters)

    whitespace,

    number,

    comment,

    functionName,

    // keywords
    domain_,
    range_,
    models_,
    format_,
    documentation_,
    meronym_,
    property_,
    subAttribute_,
    abbreviation_,
    result_,
    duration_,
    agent_,
    hasPurpose_,
    finishes_,
    earlier_,
    yield_,
    instrument_,
    destination_,
    material_,
    causes_,
    origin_,
    employs_,
    possesses_,
    disjoint_,
    mother_,
    father_,
    son_,
    daughter_,
    brother_,
    sister_,
    sibling_,

    date_,
    insured_,
    askPrice_,
    outOfTheMoney_,
}

/** SUO-KIF Token. */
struct Token
{
    @safe pure nothrow @nogc:
    TOK tok;
    string src;                 // optional source slice
}

/** SUO_KIF Expression.
    TODO use vary.FastVariant
 */
struct Expr
{
    Token token;                // token
    Expr[] subs;
}

import array_ex : UniqueArray;
alias Exprs = UniqueArray!Expr;
// alias Exprs = Appender!(Expr[]);// import std.array : Appender;

pragma(inline, true)
bool isSymbolChar(dchar x)
    @safe pure nothrow @nogc
{
    import std.uni : isAlphaNum;
    import std.algorithm : among;
    return x.isAlphaNum || x.among!('_', '-'); // TODO merge to single call to among
}

/** Returns: true if `s` is null-terminated (ending with `'\0'`).

    Used to verify input to parser make use of sentinel-based search.

    See also: https://en.wikipedia.org/wiki/Sentinel_value
 */
pragma(inline, true)
bool isNullTerminated(const(char)[] s)
    @safe pure nothrow @nogc
{
    return s[$ - 1] == '\0';
}

/** Parse SUO-KIF from `src` into returned array of expressions (`Expr`). */
Exprs parseSUOKIF(string src) @safe pure
{
    assert(src.isNullTerminated);

    import std.range : empty, front, popFront, popFrontN;
    import std.uni : isWhite, isAlpha;
    import std.ascii : isDigit;
    import std.algorithm : among, skipOver, move;

    alias Src = typeof(src);

    typeof(return) exprs;           // expression stack

    size_t leftParenDepth = 0;

    const whole = src;

    src.skipOver(x"EFBBBF");    // skip magic? header for some files

    /// Skip over `n` bytes in `src`.
    static Src skipOverNBytes(ref Src src, size_t n)
        @safe pure nothrow @nogc
    {
        const part = src[0 .. n];
        src = src[n .. $];
        return part;
    }

    /// Skip comment.
    static void skipComment(ref Src src)
        @safe pure
    {
        assert(src.isNullTerminated);
        while (!src.front.among('\r', '\n')) // until end of line
        {
            src.popFront();
        }
    }

    /// Get symbol.
    static Src getSymbol(ref Src src)
        @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        size_t i = 0;
        while ((!src[i].among!('\0', '(', ')')) &&
               (!src[i].isWhite))
        {
            ++i;
        }
        return skipOverNBytes(src, i);
    }

    /// Get numeric literal (number) in integer or decimal forma.
    static Src getNumber(ref Src src)
        @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        size_t i = 0;
        while (src[i].isDigit ||
               src[i].among!('+', '-', '.')) { ++i; } // TODO merge to single call to among
        return skipOverNBytes(src, i);
    }

    /// Get Src literal.
    static Src getStringLiteral(ref Src src)
        @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        src.popFront();         // pop leading double quote
        size_t i = 0;
        while (!src[i].among('\0', '"')) { ++i; }
        const literal = src[0 .. i]; src = src[i .. $]; // TODO functionize
        src.popFront();         // pop ending double quote
        return literal;
    }

    /// Skip whitespace.
    static Src getWhitespace(ref Src src)
        @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        size_t i = 0;
        while (src[i].isWhite) { ++i; }
        return skipOverNBytes(src, i);
    }

    while (true)
    {
        switch (src.front)
        {
        case ';':
            skipComment(src);   // TODO store comment in Token
            exprs ~= Expr(Token(TOK.comment, src[0 .. 1]));
            break;
        case '(':
            exprs ~= Expr(Token(TOK.leftParen, src[0 .. 1]));
            src.popFront();
            ++leftParenDepth;
            break;
        case ')':
            exprs ~= Expr(Token(TOK.rightParen, src[0 .. 1]));
            src.popFront();
            --leftParenDepth;

            exprs.popBack();   // pop right paren
            assert(!exprs.empty);

            // TODO retroIndexOf
            size_t count = 0; // number of elements between parens
            while (exprs[$ - 1 - count].token.tok != TOK.leftParen)
            {
                ++count;
            }
            // assert(count != 0);

            if (count >= 1)
            {
                Expr newExpr = Expr(exprs[$ - count].token,
                                    exprs[$ - count + 1 .. $].dup); // TODO moveAll or moveEmplaceAll from `exprs` instead
                exprs.popBackN(1 + count); // forget tokens including leftParen
                exprs ~= newExpr.move;
            }

            break;
        case '"':
            const stringLiteral = getStringLiteral(src); // TODO tokenize
            exprs ~= Expr(Token(TOK.stringLiteral, stringLiteral));
            break;
        case '=':
            if (src.length >= 2 + 1 && // two plus terminator
                src[1] == '>') // src.startsWith(`=>`)
            {
                exprs ~= Expr(Token(TOK.oneDirInference, src[0 .. 2]));
                src.popFrontN(2);
            }
            else
            {
                exprs ~= Expr(Token(TOK.equivalence, src[0 .. 1]));
                src.popFront();
            }
            break;
        case '<':
            src.popFront();
            if (src.front == '=')
            {
                src.popFront();
                if (src.front == '>')
                {
                    src.popFront();
                    exprs ~= Expr(Token(TOK.biDirInference, null));
                }
                else
                {
                    throw new Exception(`Parse error`);
                }
            }
            else
            {
                throw new Exception(`Parse error`);
            }
            break;
        case ',':
            src.popFront();
            exprs ~= Expr(Token(TOK.lispComma));
            break;
        case '`':
            src.popFront();
            exprs ~= Expr(Token(TOK.lispBackQuote));
            break;
        case '\'':
            src.popFront();
            exprs ~= Expr(Token(TOK.lispQuote));
            break;
        case '?':
            src.popFront();
            const variableSymbol = getSymbol(src);
            exprs ~= Expr(Token(TOK.variable, variableSymbol));
            break;
        case '@':
            src.popFront();
            const variableListSymbol = getSymbol(src);
            exprs ~= Expr(Token(TOK.variableList, variableListSymbol));
            break;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case '-':
        case '+':
        case '.':
            const number = getNumber(src);
            exprs ~= Expr(Token(TOK.number, number));
            break;
            // from std.ascii.isWhite:
        case ' ':
        case 0x09:
        case 0x10:
        case 0x0A:
        case 0x0B:
        case 0x0C:
        case 0x0D:
            assert(src.front.isWhite);
            getWhitespace(src);
            // skip whitespace for now: exprs ~= Expr(Token(TOK.whitespace, null));
            break;
        case '\0':
            goto nullFound;
        default:
            // other
            if (true// src.front.isAlpha
                )
            {
                const symbol = getSymbol(src); // TODO tokenize
                switch (symbol)
                {
                case `domain`: exprs ~= Expr(Token(TOK.domain_, symbol)); break;
                case `range`: exprs ~= Expr(Token(TOK.range_, symbol)); break;
                case `models`: exprs ~= Expr(Token(TOK.models_, symbol)); break;
                case `format`: exprs ~= Expr(Token(TOK.format_, symbol)); break;
                case `documentation`: exprs ~= Expr(Token(TOK.documentation_, symbol)); break;
                case `meronym`: exprs ~= Expr(Token(TOK.meronym_, symbol)); break;
                case `property`: exprs ~= Expr(Token(TOK.property_, symbol)); break;
                case `subAttribute`: exprs ~= Expr(Token(TOK.subAttribute_, symbol)); break;
                case `abbreviation`: exprs ~= Expr(Token(TOK.abbreviation_, symbol)); break;
                case `result`: exprs ~= Expr(Token(TOK.result_, symbol)); break;
                case `duration`: exprs ~= Expr(Token(TOK.duration_, symbol)); break;
                case `agent`: exprs ~= Expr(Token(TOK.agent_, symbol)); break;
                case `hasPurpose`: exprs ~= Expr(Token(TOK.hasPurpose_, symbol)); break;
                case `finishes`: exprs ~= Expr(Token(TOK.finishes_, symbol)); break;
                case `earlier`: exprs ~= Expr(Token(TOK.earlier_, symbol)); break;
                case `yield`: exprs ~= Expr(Token(TOK.yield_, symbol)); break;
                case `instrument`: exprs ~= Expr(Token(TOK.instrument_, symbol)); break;
                case `destination`: exprs ~= Expr(Token(TOK.destination_, symbol)); break;
                case `material`: exprs ~= Expr(Token(TOK.material_, symbol)); break;
                case `causes`: exprs ~= Expr(Token(TOK.causes_, symbol)); break;
                case `origin`: exprs ~= Expr(Token(TOK.origin_, symbol)); break;
                case `employs`: exprs ~= Expr(Token(TOK.employs_, symbol)); break;
                case `possesses`: exprs ~= Expr(Token(TOK.possesses_, symbol)); break;
                case `disjoint`: exprs ~= Expr(Token(TOK.disjoint_, symbol)); break;
                case `mother`: exprs ~= Expr(Token(TOK.mother_, symbol)); break;
                case `father`: exprs ~= Expr(Token(TOK.father_, symbol)); break;
                case `son`: exprs ~= Expr(Token(TOK.son_, symbol)); break;
                case `daughter`: exprs ~= Expr(Token(TOK.daughter_, symbol)); break;
                case `brother`: exprs ~= Expr(Token(TOK.brother_, symbol)); break;
                case `sister`: exprs ~= Expr(Token(TOK.sister_, symbol)); break;
                case `sibling`: exprs ~= Expr(Token(TOK.sibling_, symbol)); break;
                case `date`: exprs ~= Expr(Token(TOK.date_, symbol)); break;
                case `insured`: exprs ~= Expr(Token(TOK.insured_, symbol)); break;
                case `askPrice`: exprs ~= Expr(Token(TOK.askPrice_, symbol)); break;
                case `outOfTheMoney`: exprs ~= Expr(Token(TOK.outOfTheMoney_, symbol)); break;
                default:
                    import std.uni : isLower;
                    import std.algorithm : endsWith;
                    if (symbol.endsWith(`Fn`))
                    {
                        exprs ~= Expr(Token(TOK.functionName, symbol));
                    }
                    else
                    {
                        exprs ~= Expr(Token(TOK.symbol, symbol));
                    }
                    break;
                }
            }
            else
            {
                import std.conv : to;
                assert(false,
                       `Cannot handle character '` ~ src.front.to!string ~
                       `' at index:` ~ (&src[0] - &whole[0]).to!string);
            }
            break;
        }
        if (!exprs.empty)
        {
            import dbgio : dln;
            // dln(exprs.back);
        }
    }

nullFound:

    assert(leftParenDepth == 0);        // should be balanced

    return exprs;
}

@safe pure unittest
{
    const text = "(instance AttrFn BinaryFunction)\0";
    const exprs = parseSUOKIF(text);

    assert(exprs.length == 1);

    assert(exprs[0].token.tok == TOK.symbol);
    assert(exprs[0].token.src == `instance`);

    assert(exprs[0].subs[0].token.tok == TOK.functionName);
    assert(exprs[0].subs[0].token.src == "AttrFn");

    assert(exprs[0].subs[1].token.tok == TOK.symbol);
    assert(exprs[0].subs[1].token.src == "BinaryFunction");
}

version(none)
unittest
{
    import std.path : expandTilde;
    import std.file : readText;
    const text = `~/elisp/mine/relangs.el`.expandTilde.readText;
    const ctext = text ~ '\0'; // null at the end to enable sentinel-based search in parser
    assert(ctext[$ - 1] == '\0');
    const exprs = ctext.parseSUOKIF();
}

unittest
{
    readSUOKIFs(`~/Work/sumo`);
}

/** Read all SUO-KIF files (.kif) located under `rootDirPath`.
 */
Exprs readSUOKIFs(string rootDirPath)
{
    typeof(return) allExprs;

    import std.stdio : write, writeln;
    import std.path : expandTilde;

    import std.file: dirEntries, SpanMode;
    auto entries = dirEntries(rootDirPath.expandTilde, SpanMode.breadth, false); // false: skip symlinks
    foreach (dent; entries)
    {
        const filePath = dent.name;
        import std.algorithm : endsWith, canFind;

        import std.path : baseName, pathSplitter;
        immutable basename = dent.name.baseName;

        import std.utf;
        import std.algorithm : among;
        try
        {
            if (filePath.endsWith(`.kif`) &&
                !filePath.pathSplitter.canFind(`.git`)) // invalid UTF-8 encodings
            {
                write(`Lexing SUO-KIF `, filePath, ` ... `);

                import std.file : readText;
                // file.readText.lexSUOKIF2();

                import std.datetime : StopWatch, AutoStart, Duration;
                auto sw = StopWatch(AutoStart.yes);

                // TODO move this logic to readText(bool nullTerminated = false) by .capacity += 1
                const text = filePath.readText;
                const ctext = text ~ '\0'; // null at the end to enable sentinel-based search in parser
                assert(ctext[$ - 1] == '\0');
                allExprs ~= ctext.parseSUOKIF()[];
                sw.stop;
                import std.conv : to;
                writeln(`took `, sw.peek().to!Duration);
            }
        }
        catch (std.utf.UTFException e)
        {
            import std.file : read;
            writeln(" failed because of invalid UTF-8 encoding starting with ", filePath.read(16));
        }
    }

    return allExprs;
}

// void lexSUOKIF2(R)(R src)
// {
//     import std.experimental.lexer;

//     static immutable TokOperators = [ `(`, `)`, `=>` ];
//     static immutable TokDynamic = [ `stringLiteral`, `comment`, `identifier`, `numberLiteral`, `whitespace` ];
//     static immutable TokKeywords = [ `and`, `exists`, `or`, `not` ];
//     import std.meta : AliasSeq;

//     alias Toks = AliasSeq!(TokOperators, TokDynamic, TokKeywords);
//     alias TokID = TokenIdType!Toks;
//     alias tokToString = tokenStringRepresentation!(TokID, Toks);
//     alias tok(string symbol) = TokenId!(TokID, LuaTokens, symbol);

//     static immutable tokenHandlers = [
//         "\"", "lexStringLiteral",
//         ";", "lexComment",
//         " ",  "lexWhitespace",
//         "\t", "lexWhitespace",
//         "\r", "lexWhitespace",
//         "\n", "lexWhitespace",
//         ];
// }
