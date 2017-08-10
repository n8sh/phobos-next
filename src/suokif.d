/** SUO-KIF File Format.

    See: https://en.wikipedia.org/wiki/Knowledge_Interchange_Format
    See: http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/sigma/suo-kif.pdf
*/
module suokif;

// import std.range : isInputRange;
import dbgio : dln;
import array_ex : UniqueArray, Ordering;
import vary : VaryN;

/** SUO-KIF Token Type. */
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

    variable,
    varParams,                  // one or more parameter

    whitespace,

    number,

    comment,

    className,
    functionName,

    // keywords
    and_,
    or_,
    not_,
    exists_,
    instance_,
    domain_,
    lexicon_,
    range_,
    subrelation_,
    models_,
    format_,
    subclass_,
    documentation_,
    meronym_,
    property_,
    attribute_,
    subAttribute_,
    equal_,
    abbreviation_,
    result_,
    duration_,
    agent_,
    member_,
    hasPurpose_,
    finishes_,
    earlier_,
    yield_,
    instrument_,
    destination_,
    material_,
    causes_,
    origin_,
    located_,
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

    lessThan_,
    greaterThan_,
    lessThanOrEqualTo_,
    greaterThanOrEqualTo_,

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
    Token token;               // token
    // UniqueArray!(Expr*) subs;   // sub-expressions
    // import std.array : Appender;
    // Appender!(Expr[]) subs;
    Expr[] subs;
}

pragma(inline)
bool isSymbolChar(char x)
    @safe pure nothrow @nogc
{
    import std.uni : isAlphaNum;
    import std.algorithm : among;
    return x.isAlphaNum || x.among!('_', '-');
}

/// Returns: true if `s` is terminated with a zero character (null).
pragma(inline, true)
bool isNullTerminated(const(char)[] s)
    @safe pure nothrow @nogc
{
    return s[$ - 1] == '\0';
}

/** Parse SUO-KIF from `src` into returned array of expressions (`Expr`). */
UniqueArray!Expr parseSUOKIF(string src) @safe pure
{
    assert(src.isNullTerminated);

    import std.range : empty, front, popFront, popFrontN;
    import std.uni : isWhite, isAlpha;
    import std.ascii : isDigit;
    import std.algorithm : among, skipOver, move;

    UniqueArray!Expr exprs;           // expression stack

    size_t leftParenDepth = 0;

    const whole = src;

    src.skipOver(x"EFBBBF");    // skip magic? header for some files

    /// Skip comment.
    static void skipComment(ref string src) @safe pure
    {
        assert(src.isNullTerminated);
        while (!src.empty &&
               !src.front.among('\r', '\n')) // until end of line
        {
            src.popFront();
        }
    }

    static string skipN(ref string src, size_t n) @safe pure nothrow @nogc
    {
        const part = src[0 .. n];
        src = src[n .. $];
        return part;
    }

    /// Get symbol.
    static string getSymbol(ref string src) @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        size_t i = 0;
        while (src[i].isSymbolChar) { ++i; }
        return skipN(src, i);
    }

    /// Get numeric literal (number) in integer or decimal forma.
    static string getNumber(ref string src) @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        size_t i = 0;
        while (src[i].isDigit ||
               src[i].among!('+', '-', '.')) { ++i; } // TODO merge to single call to among
        return skipN(src, i);
    }

    /// Get string literal.
    static string getStringLiteral(ref string src) @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        src.popFront();         // pop leading double quote
        size_t i = 0;
        while (!src[i].among('"', '\0')) { ++i; }
        const literal = src[0 .. i]; src = src[i .. $]; // TODO functionize
        src.popFront();         // pop ending double quote
        return literal;
    }

    /// Skip whitespace.
    static string getWhitespace(ref string src) @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        size_t i = 0;
        while (src[i].isWhite) { ++i; }
        return skipN(src, i);
    }

    bool[string] lowerSymbols;

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

            Expr newExpr;

            foreach (const argIx; 0 .. count)
            {
                // dln(argIx, ":", exprs[$ - count + argIx]);
            }

            // copy parameters to expression
            switch (count)
            {
            case 0:
                break;          // do nothing
            case 1:
                newExpr.subs = [exprs[$ - count + 0]];
                break;
            case 2:
                newExpr.subs = [exprs[$ - count + 0],
                                exprs[$ - count + 1]];
                break;
            case 3:
                newExpr.subs = [exprs[$ - count + 0],
                                exprs[$ - count + 1],
                                exprs[$ - count + 2]];
                if (newExpr.subs[0].token.tok == TOK.subclass_)
                {
                    // dln(newExpr.subs);
                }
                break;
            case 4:
                newExpr.subs = [exprs[$ - count + 0],
                                exprs[$ - count + 1],
                                exprs[$ - count + 2],
                                exprs[$ - count + 3]];
                break;
            case 5:
                newExpr.subs = [exprs[$ - count + 0],
                                exprs[$ - count + 1],
                                exprs[$ - count + 2],
                                exprs[$ - count + 3],
                                exprs[$ - count + 4]];
                break;
            case 6:
                newExpr.subs = [exprs[$ - count + 0],
                                exprs[$ - count + 1],
                                exprs[$ - count + 2],
                                exprs[$ - count + 3],
                                exprs[$ - count + 4],
                                exprs[$ - count + 5]];
                break;
            case 7:
                newExpr.subs = [exprs[$ - count + 0],
                                exprs[$ - count + 1],
                                exprs[$ - count + 2],
                                exprs[$ - count + 3],
                                exprs[$ - count + 4],
                                exprs[$ - count + 5],
                                exprs[$ - count + 6]];
                break;
            default:
                foreach (const argIx; 0 .. count)
                {
                    newExpr.subs ~= exprs[$ - count + argIx];
                }
                // dln(newExpr);
                break;
            }

            exprs.popBackN(count + 1); // forget tokens plus match leftParen
            // dln("newExpr:", newExpr, " count:", count);

            exprs ~= newExpr;

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
        case '?':
            src.popFront();
            const variableSymbol = getSymbol(src);
            exprs ~= Expr(Token(TOK.variable, variableSymbol));
            break;
        case '@':
            src.popFront();
            const varParamsSymbol = getSymbol(src);
            exprs ~= Expr(Token(TOK.varParams, varParamsSymbol));
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
            if (src.front.isAlpha)
            {
                const symbol = getSymbol(src); // TODO tokenize
                switch (symbol)
                {
                case `and`: exprs ~= Expr(Token(TOK.and_, symbol)); break;
                case `or`: exprs ~= Expr(Token(TOK.or_, symbol)); break;
                case `not`: exprs ~= Expr(Token(TOK.not_, symbol)); break;
                case `exists`: exprs ~= Expr(Token(TOK.exists_, symbol)); break;
                case `instance`: exprs ~= Expr(Token(TOK.instance_, symbol)); break;
                case `domain`: exprs ~= Expr(Token(TOK.domain_, symbol)); break;
                case `lexicon`: exprs ~= Expr(Token(TOK.lexicon_, symbol)); break;
                case `range`: exprs ~= Expr(Token(TOK.range_, symbol)); break;
                case `subrelation`: exprs ~= Expr(Token(TOK.subrelation_, symbol)); break;
                case `models`: exprs ~= Expr(Token(TOK.models_, symbol)); break;
                case `format`: exprs ~= Expr(Token(TOK.format_, symbol)); break;
                case `subclass`: exprs ~= Expr(Token(TOK.subclass_, symbol)); break;
                case `documentation`: exprs ~= Expr(Token(TOK.documentation_, symbol)); break;
                case `meronym`: exprs ~= Expr(Token(TOK.meronym_, symbol)); break;
                case `property`: exprs ~= Expr(Token(TOK.property_, symbol)); break;
                case `attribute`: exprs ~= Expr(Token(TOK.attribute_, symbol)); break;
                case `subAttribute`: exprs ~= Expr(Token(TOK.subAttribute_, symbol)); break;
                case `equal`: exprs ~= Expr(Token(TOK.equal_, symbol)); break;
                case `abbreviation`: exprs ~= Expr(Token(TOK.abbreviation_, symbol)); break;
                case `result`: exprs ~= Expr(Token(TOK.result_, symbol)); break;
                case `duration`: exprs ~= Expr(Token(TOK.duration_, symbol)); break;
                case `agent`: exprs ~= Expr(Token(TOK.agent_, symbol)); break;
                case `member`: exprs ~= Expr(Token(TOK.member_, symbol)); break;
                case `hasPurpose`: exprs ~= Expr(Token(TOK.hasPurpose_, symbol)); break;
                case `finishes`: exprs ~= Expr(Token(TOK.finishes_, symbol)); break;
                case `earlier`: exprs ~= Expr(Token(TOK.earlier_, symbol)); break;
                case `yield`: exprs ~= Expr(Token(TOK.yield_, symbol)); break;
                case `instrument`: exprs ~= Expr(Token(TOK.instrument_, symbol)); break;
                case `destination`: exprs ~= Expr(Token(TOK.destination_, symbol)); break;
                case `material`: exprs ~= Expr(Token(TOK.material_, symbol)); break;
                case `causes`: exprs ~= Expr(Token(TOK.causes_, symbol)); break;
                case `origin`: exprs ~= Expr(Token(TOK.origin_, symbol)); break;
                case `located`: exprs ~= Expr(Token(TOK.located_, symbol)); break;
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
                case `lessThan`: exprs ~= Expr(Token(TOK.lessThan_, symbol)); break;
                case `lessThanOrEqualTo`: exprs ~= Expr(Token(TOK.lessThanOrEqualTo_, symbol)); break;
                case `greaterThan`: exprs ~= Expr(Token(TOK.greaterThan_, symbol)); break;
                case `greaterThanOrEqualTo`: exprs ~= Expr(Token(TOK.greaterThanOrEqualTo_, symbol)); break;
                case `date`: exprs ~= Expr(Token(TOK.date_, symbol)); break;
                case `insured`: exprs ~= Expr(Token(TOK.insured_, symbol)); break;
                case `askPrice`: exprs ~= Expr(Token(TOK.askPrice_, symbol)); break;
                case `outOfTheMoney`: exprs ~= Expr(Token(TOK.outOfTheMoney_, symbol)); break;
                default:
                    import std.uni : isLower;
                    import std.algorithm : endsWith;
                    if (symbol.front.isLower)
                    {
                        if (symbol !in lowerSymbols)
                        {
                            lowerSymbols[symbol] = true;
                        }
                    }
                    else if (symbol.endsWith(`Fn`))
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
                dln(`Cannot handle character '`, src.front, `' at index:`, &src[0] - &whole[0]);
                assert(false);
            }
            break;
        }
        // dln("back:", exprs.back);
    }

nullFound:

    assert(leftParenDepth == 0);        // should be balanced

    return exprs;
}

@safe pure unittest
{
    const text = "(instance AttrFn BinaryFunction)\0";
    const exprs = parseSUOKIF(text);
}

unittest
{
    readSUOKIFs(`~/Work/sumo`);
}

/** Read all SUO-KIF files (.kif) located under `rootDirPath`.
 */
void readSUOKIFs(string rootDirPath)
{
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
                ctext.parseSUOKIF();
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

    // const file = `~/Work/phobos-next/src/emotion.kif`.expandTilde;
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
