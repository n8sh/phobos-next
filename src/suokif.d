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

    lispComma,
    lispBackQuote,
    lispQuote,

    variable,
    variableList,               // one or more variables (parameters)

    whitespace,

    number,

    comment,

    functionName,
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
    return s.length >= 1 && s[$ - 1] == '\0';
}

/** Parse SUO-KIF from `src` into returned array of expressions (`Expr`). */
Exprs parseSUOKIF(string src, bool includeComments = false)
    @safe pure
{
    import std.exception : enforce;
    enforce(src.isNullTerminated); // safest to do this check in non-debug mode aswell

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

    import std.meta : AliasSeq;
    alias whiteChars = AliasSeq!(' ', '\t', '\n', '\r'); // TODO use and benchmark
    alias digitChars = AliasSeq!('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'); // TODO use benchmark

    /// Get symbol.
    static Src getSymbol(ref Src src)
        @safe pure nothrow @nogc
    {
        assert(src.isNullTerminated);
        size_t i = 0;
        while ((!src[i].among!('\0', '(', ')')) &&
               (!src[i].isWhite)
            )
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
            if (includeComments)
            {
                exprs ~= Expr(Token(TOK.comment, src[0 .. 1]));
            }
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
    const text = ";;a comment\n(instance AttrFn BinaryFunction)\0";
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
                write(`Reading SUO-KIF `, filePath, ` ... `);

                import std.file : readText;
                import std.datetime : StopWatch, AutoStart, Duration;
                auto sw = StopWatch(AutoStart.yes);

                // TODO move this logic to readText(bool nullTerminated = false) by .capacity += 1
                const text = filePath.readText;
                const ctext = text ~ '\0'; // null at the end to enable sentinel-based search in parser
                assert(ctext[$ - 1] == '\0');
                allExprs ~= ctext.parseSUOKIF()[];
                sw.stop();
                import std.conv : to;
                writeln(`took `, sw.peek.to!Duration);
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
