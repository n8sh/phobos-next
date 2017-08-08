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
    @safe pure nothrow @nogc:
    Token token;                // token
    Expr[] subs;                // sub-expressions
}

pragma(inline)
bool isSymbolChar(char x)
    @safe pure nothrow @nogc
{
    import std.uni : isAlphaNum;
    import std.algorithm : among;
    return x.isAlphaNum || x.among!('_', '-');
}

/** Parse SUO-KIF from `src`. */
UniqueArray!Expr lexSUOKIF(string src) @safe pure
{
    import std.range : empty, front, popFront, popFrontN;
    import std.uni : isWhite, isAlpha;
    import std.ascii : isDigit;
    import std.algorithm : among, skipOver;
    import std.array : Appender;

    UniqueArray!Token tokenStack;         // token stack
    UniqueArray!Expr exprStack;           // expression stack

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
               src[i].isSymbolChar) { ++i; }
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
            tokenStack ~= Token(TOK.comment, src[0 .. 1]);
            break;
        case '(':
            tokenStack ~= Token(TOK.leftParen, src[0 .. 1]);
            src.popFront();
            ++leftParenDepth;
            break;
        case ')':
            tokenStack ~= Token(TOK.rightParen, src[0 .. 1]);
            src.popFront();
            --leftParenDepth;

            tokenStack.popBack();   // pop right paren
            assert(!tokenStack.empty);

            // TODO retroindexOf
            size_t argCount = 0; // last index
            while (tokenStack[$ - 1 - argCount].tok != TOK.leftParen)
            {
                ++argCount;
            }

            Expr newExpr;
            // copy parameters to expression
            foreach (argIx; 0 .. argCount)
            {
                newExpr.subs ~= Expr(tokenStack[$ - argCount + argIx]);
            }
            exprStack ~= newExpr;

            tokenStack.popBackN(argCount + 1); // forget tokens plus match leftParen

            break;
        case '"':
            const stringLiteral = getStringLiteral(src); // TODO tokenize
            tokenStack ~= Token(TOK.stringLiteral, stringLiteral);
            break;
        case '=':
            if (src.length >= 2 && src[1] == '>') // src.startsWith(`=>`)
            {
                tokenStack ~= Token(TOK.oneDirInference, src[0 .. 2]);
                src.popFrontN(2);
            }
            else
            {
                tokenStack ~= Token(TOK.equivalence, src[0 .. 1]);
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
                    tokenStack ~= Token(TOK.biDirInference, null);
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
            tokenStack ~= Token(TOK.variable, variableSymbol);
            break;
        case '@':
            src.popFront();
            const varParamsSymbol = getSymbol(src);
            tokenStack ~= Token(TOK.varParams, varParamsSymbol);
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
            tokenStack ~= Token(TOK.number, number);
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
            // skip whitespace for now: tokenStack ~= Token(TOK.whitespace, null);
            break;
        default:
            // other
            if (src.front.isAlpha)
            {
                const symbol = getSymbol(src); // TODO tokenize
                switch (symbol)
                {
                case `and`: tokenStack ~= Token(TOK.and_, symbol); break;
                case `or`: tokenStack ~= Token(TOK.or_, symbol); break;
                case `not`: tokenStack ~= Token(TOK.not_, symbol); break;
                case `exists`: tokenStack ~= Token(TOK.exists_, symbol); break;
                case `instance`: tokenStack ~= Token(TOK.instance_, symbol); break;
                case `domain`: tokenStack ~= Token(TOK.domain_, symbol); break;
                case `lexicon`: tokenStack ~= Token(TOK.lexicon_, symbol); break;
                case `range`: tokenStack ~= Token(TOK.range_, symbol); break;
                case `subrelation`: tokenStack ~= Token(TOK.subrelation_, symbol); break;
                case `models`: tokenStack ~= Token(TOK.models_, symbol); break;
                case `format`: tokenStack ~= Token(TOK.format_, symbol); break;
                case `subclass`: tokenStack ~= Token(TOK.subclass_, symbol); break;
                case `documentation`: tokenStack ~= Token(TOK.documentation_, symbol); break;
                case `meronym`: tokenStack ~= Token(TOK.meronym_, symbol); break;
                case `property`: tokenStack ~= Token(TOK.property_, symbol); break;
                case `attribute`: tokenStack ~= Token(TOK.attribute_, symbol); break;
                case `subAttribute`: tokenStack ~= Token(TOK.subAttribute_, symbol); break;
                case `equal`: tokenStack ~= Token(TOK.equal_, symbol); break;
                case `abbreviation`: tokenStack ~= Token(TOK.abbreviation_, symbol); break;
                case `result`: tokenStack ~= Token(TOK.result_, symbol); break;
                case `duration`: tokenStack ~= Token(TOK.duration_, symbol); break;
                case `agent`: tokenStack ~= Token(TOK.agent_, symbol); break;
                case `member`: tokenStack ~= Token(TOK.member_, symbol); break;
                case `hasPurpose`: tokenStack ~= Token(TOK.hasPurpose_, symbol); break;
                case `finishes`: tokenStack ~= Token(TOK.finishes_, symbol); break;
                case `earlier`: tokenStack ~= Token(TOK.earlier_, symbol); break;
                case `yield`: tokenStack ~= Token(TOK.yield_, symbol); break;
                case `instrument`: tokenStack ~= Token(TOK.instrument_, symbol); break;
                case `destination`: tokenStack ~= Token(TOK.destination_, symbol); break;
                case `material`: tokenStack ~= Token(TOK.material_, symbol); break;
                case `causes`: tokenStack ~= Token(TOK.causes_, symbol); break;
                case `origin`: tokenStack ~= Token(TOK.origin_, symbol); break;
                case `located`: tokenStack ~= Token(TOK.located_, symbol); break;
                case `employs`: tokenStack ~= Token(TOK.employs_, symbol); break;
                case `possesses`: tokenStack ~= Token(TOK.possesses_, symbol); break;
                case `disjoint`: tokenStack ~= Token(TOK.disjoint_, symbol); break;
                case `mother`: tokenStack ~= Token(TOK.mother_, symbol); break;
                case `father`: tokenStack ~= Token(TOK.father_, symbol); break;
                case `son`: tokenStack ~= Token(TOK.son_, symbol); break;
                case `daughter`: tokenStack ~= Token(TOK.daughter_, symbol); break;
                case `brother`: tokenStack ~= Token(TOK.brother_, symbol); break;
                case `sister`: tokenStack ~= Token(TOK.sister_, symbol); break;
                case `sibling`: tokenStack ~= Token(TOK.sibling_, symbol); break;
                case `lessThan`: tokenStack ~= Token(TOK.lessThan_, symbol); break;
                case `lessThanOrEqualTo`: tokenStack ~= Token(TOK.lessThanOrEqualTo_, symbol); break;
                case `greaterThan`: tokenStack ~= Token(TOK.greaterThan_, symbol); break;
                case `greaterThanOrEqualTo`: tokenStack ~= Token(TOK.greaterThanOrEqualTo_, symbol); break;
                case `date`: tokenStack ~= Token(TOK.date_, symbol); break;
                case `insured`: tokenStack ~= Token(TOK.insured_, symbol); break;
                case `askPrice`: tokenStack ~= Token(TOK.askPrice_, symbol); break;
                case `outOfTheMoney`: tokenStack ~= Token(TOK.outOfTheMoney_, symbol); break;
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
                        tokenStack ~= Token(TOK.functionName, symbol);
                    }
                    else
                    {
                        tokenStack ~= Token(TOK.symbol, symbol);
                    }
                    break;
                }
            }
            else
            {
                dln(`Cannot handle character '`, src.front, `' at index:`, &src[0] - &whole[0]);
                // dln(tokenStack[]);
                assert(false);
            }
            break;
        }
    }

    assert(leftParenDepth == 0);        // should be balanced

    return exprStack;
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
