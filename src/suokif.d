/** SUO-KIF File Format.

    See: https://en.wikipedia.org/wiki/Knowledge_Interchange_Format
    See: http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/sigma/suo-kif.pdf
*/
module suokif;

// import std.range : isInputRange;
import dbgio : dln;
import array_ex : Array, Ordering;
import vary : VaryN;

/** SUO-KIF Token Type. */
enum TOK
{
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
    this(TOK tok, string src)
    {
        this.tok = tok;
        this.src = src;
    }
    string src;                 // optional source slice
    TOK tok;
}

/** SUO_KIF Expression.
    TODO use vary.FastVariant
 */
struct Expr
{
    Token token;                // token
    Expr[] subs;                // sub-expressions
}

bool isLispSymbolChar(char x)
    @safe pure nothrow @nogc
{
    import std.uni : isAlphaNum;
    import std.algorithm : among;
    return x.isAlphaNum || x.among!('_', '-');
}

/** Parse SUO-KIF from `src`. */
void lexSUOKIF(string src) @safe pure
{
    import std.range : empty, front, popFront, popFrontN;
    import std.uni : isWhite, isAlpha;
    import std.ascii : isDigit;
    import std.algorithm : among, skipOver;
    import std.array : Appender;

    Array!Token tokens;         // token stack
    Array!Token exprs;          // expression stack
    // Appender!(Token[]) tokens;

    size_t leftParenDepth = 0;

    const whole = src;

    src.skipOver(x"EFBBBF");    // skip magic? header for some files

    /// Skip comment.
    static void skipComment(ref string src) @safe pure
    {
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
        size_t i = 0;
        while (i != src.length &&
               src[i].isLispSymbolChar) { ++i; }
        return skipN(src, i);
    }

    /// Get numeric literal (number) in integer or decimal forma.
    static string getNumber(ref string src) @safe pure nothrow @nogc
    {
        size_t i = 0;
        while (i != src.length &&
               (src[i].isDigit ||
                src[i].among!('+', '-', '.'))) { ++i; }
        return skipN(src, i);
    }

    /// Get string literal.
    static string getStringLiteral(ref string src) @safe pure nothrow @nogc
    {
        src.popFront();         // pop leading double quote
        size_t i = 0;
        while (i != src.length &&
               src[i] != '"') { ++i; }
        const literal = src[0 .. i]; src = src[i .. $]; // TODO functionize
        src.popFront();         // pop ending double quote
        return literal;
    }

    /// Skip whitespace.
    static string getWhitespace(ref string src) @safe pure nothrow @nogc
    {
        size_t i = 0;
        while (i != src.length &&
               src[i].isWhite) { ++i; }
        return skipN(src, i);
    }

    bool[string] lowerSymbols;

    while (!src.empty)
    {
        switch (src.front)
        {
        case ';':
            skipComment(src);   // TODO store comment in Token
            tokens ~= Token(TOK.comment, src[0 .. 1]);
            break;
        case '(':
            tokens ~= Token(TOK.leftParen, src[0 .. 1]);
            src.popFront();
            ++leftParenDepth;
            break;
        case ')':
            tokens ~= Token(TOK.rightParen, src[0 .. 1]);
            src.popFront();
            --leftParenDepth;

            tokens.popBack();   // pop right paren
            size_t j = 0;
            while (tokens.back.tok != TOK.leftParen)
            {
                if (leftParenDepth == 0) // is top-level
                {
                    dln(tokens.back);
                }
                tokens.popBack();
                ++j;
            }
            tokens.popBack();   // pop matching leftParen

            break;
        case '"':
            const stringLiteral = getStringLiteral(src); // TODO tokenize
            tokens ~= Token(TOK.stringLiteral, stringLiteral);
            break;
        case '=':
            if (src.length >= 2 && src[1] == '>') // src.startsWith(`=>`)
            {
                tokens ~= Token(TOK.oneDirInference, src[0 .. 2]);
                src.popFrontN(2);
            }
            else
            {
                tokens ~= Token(TOK.equivalence, src[0 .. 1]);
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
                    tokens ~= Token(TOK.biDirInference, null);
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
            tokens ~= Token(TOK.variable, variableSymbol);
            break;
        case '@':
            src.popFront();
            const varParamsSymbol = getSymbol(src);
            tokens ~= Token(TOK.varParams, varParamsSymbol);
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
            tokens ~= Token(TOK.number, number);
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
            // skip whitespace for now: tokens ~= Token(TOK.whitespace, null);
            break;
        default:
            // other
            if (src.front.isAlpha)
            {
                const symbol = getSymbol(src); // TODO tokenize
                switch (symbol)
                {
                case `and`: tokens ~= Token(TOK.and_, symbol); break;
                case `or`: tokens ~= Token(TOK.or_, symbol); break;
                case `not`: tokens ~= Token(TOK.not_, symbol); break;
                case `exists`: tokens ~= Token(TOK.exists_, symbol); break;
                case `instance`: tokens ~= Token(TOK.instance_, symbol); break;
                case `domain`: tokens ~= Token(TOK.domain_, symbol); break;
                case `lexicon`: tokens ~= Token(TOK.lexicon_, symbol); break;
                case `range`: tokens ~= Token(TOK.range_, symbol); break;
                case `subrelation`: tokens ~= Token(TOK.subrelation_, symbol); break;
                case `models`: tokens ~= Token(TOK.models_, symbol); break;
                case `format`: tokens ~= Token(TOK.format_, symbol); break;
                case `subclass`: tokens ~= Token(TOK.subclass_, symbol); break;
                case `documentation`: tokens ~= Token(TOK.documentation_, symbol); break;
                case `meronym`: tokens ~= Token(TOK.meronym_, symbol); break;
                case `property`: tokens ~= Token(TOK.property_, symbol); break;
                case `attribute`: tokens ~= Token(TOK.attribute_, symbol); break;
                case `subAttribute`: tokens ~= Token(TOK.subAttribute_, symbol); break;
                case `equal`: tokens ~= Token(TOK.equal_, symbol); break;
                case `abbreviation`: tokens ~= Token(TOK.abbreviation_, symbol); break;
                case `result`: tokens ~= Token(TOK.result_, symbol); break;
                case `duration`: tokens ~= Token(TOK.duration_, symbol); break;
                case `agent`: tokens ~= Token(TOK.agent_, symbol); break;
                case `member`: tokens ~= Token(TOK.member_, symbol); break;
                case `hasPurpose`: tokens ~= Token(TOK.hasPurpose_, symbol); break;
                case `finishes`: tokens ~= Token(TOK.finishes_, symbol); break;
                case `earlier`: tokens ~= Token(TOK.earlier_, symbol); break;
                case `yield`: tokens ~= Token(TOK.yield_, symbol); break;
                case `instrument`: tokens ~= Token(TOK.instrument_, symbol); break;
                case `destination`: tokens ~= Token(TOK.destination_, symbol); break;
                case `material`: tokens ~= Token(TOK.material_, symbol); break;
                case `causes`: tokens ~= Token(TOK.causes_, symbol); break;
                case `origin`: tokens ~= Token(TOK.origin_, symbol); break;
                case `located`: tokens ~= Token(TOK.located_, symbol); break;
                case `employs`: tokens ~= Token(TOK.employs_, symbol); break;
                case `possesses`: tokens ~= Token(TOK.possesses_, symbol); break;
                case `disjoint`: tokens ~= Token(TOK.disjoint_, symbol); break;
                case `mother`: tokens ~= Token(TOK.mother_, symbol); break;
                case `father`: tokens ~= Token(TOK.father_, symbol); break;
                case `son`: tokens ~= Token(TOK.son_, symbol); break;
                case `daughter`: tokens ~= Token(TOK.daughter_, symbol); break;
                case `brother`: tokens ~= Token(TOK.brother_, symbol); break;
                case `sister`: tokens ~= Token(TOK.sister_, symbol); break;
                case `sibling`: tokens ~= Token(TOK.sibling_, symbol); break;
                case `lessThan`: tokens ~= Token(TOK.lessThan_, symbol); break;
                case `lessThanOrEqualTo`: tokens ~= Token(TOK.lessThanOrEqualTo_, symbol); break;
                case `greaterThan`: tokens ~= Token(TOK.greaterThan_, symbol); break;
                case `greaterThanOrEqualTo`: tokens ~= Token(TOK.greaterThanOrEqualTo_, symbol); break;
                case `date`: tokens ~= Token(TOK.date_, symbol); break;
                case `insured`: tokens ~= Token(TOK.insured_, symbol); break;
                case `askPrice`: tokens ~= Token(TOK.askPrice_, symbol); break;
                case `outOfTheMoney`: tokens ~= Token(TOK.outOfTheMoney_, symbol); break;
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
                        tokens ~= Token(TOK.functionName, symbol);
                    }
                    else
                    {
                        tokens ~= Token(TOK.symbol, symbol);
                    }
                    break;
                }
            }
            else
            {
                dln(`Cannot handle character '`, src.front, `' at index:`, &src[0] - &whole[0]);
                // dln(tokens[]);
                assert(false);
            }
            break;
        }
    }

    assert(leftParenDepth == 0);        // should be balanced
}

unittest
{
    readSUOKIFs(`~/Work/sumo`);
}

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

                // TODO insert a null at the end to enable sentinel-based search
                filePath.readText.lexSUOKIF();
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
    // dln(tokens[]);
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
