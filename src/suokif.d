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
    functionName,

    number,

    comment,
    whitespace,
}

/** SUO-KIF Token. */
struct Token
{
    @safe pure nothrow @nogc:
    TOK tok;
    string src;                 // optional source slice
}

/** SUO_KIF Expression.
    TODO use vary.FastVariant instead of `Expr[]`
 */
struct Expr
{
    Token token;                // token
    Expr[] subs;
}

import array_ex : UniqueArray;
alias Exprs = UniqueArray!Expr; // TODO UniqueArray!(const(Expr))
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

/** Parse SUO-KIF from `src` into returned array of expressions (`Expr`).
 */
struct SUOKIFParser
{
    @safe pure:

    this(Src src,
         bool includeComments = false,
         bool includeWhitespace = false)
    {
        this.src = src;

        import std.algorithm : skipOver;
        this.src.skipOver(x"EFBBBF");    // skip magic? header for some files

        import std.exception : enforce;
        enforce(src.isNullTerminated); // safest to do this check in non-debug mode aswell

        this._whole = src;
        this._includeComments = includeComments;
        this._includeWhitespace = includeWhitespace;

        nextFront();
    }

    pragma(inline, true)
    @property bool empty() const nothrow @nogc
    {
        return _endOfFile;
    }

    pragma(inline, true)
    ref Expr front() return scope
    {
        assert(!empty);
        return exprs.back;
    }

    pragma(inline, true)
    void popFront()
    {
        assert(!empty);
        exprs.popBack();
        nextFront();
    }

    private void nextFront()
    {
        import std.range : empty, front, popFront, popFrontN;
        import std.uni : isWhite, isAlpha;
        import std.ascii : isDigit;

        import std.algorithm : among, move;

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
        // from std.ascii.isWhite. TODO use and benchmark:
        alias whiteChars = AliasSeq!(' ', // 0x20
                                     '\t', // (0x09)
                                     '\n', // (0x0a)
                                     '\v', // (0x0b)
                                     '\r', // (0x0c)
                                     '\f'); // (0x0d)
        alias digitChars = AliasSeq!('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'); // TODO use benchmark

        /// Get symbol.
        static Src getSymbol(ref Src src)
            @safe pure nothrow @nogc
        {
            assert(src.isNullTerminated);
            size_t i = 0;
            while ((!src[i].among!('\0', '(', ')')) &&
                   (!src[i].isWhite) // TODO use whiteChars instead
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
            while (src[i].isDigit ||                      // TODO use digitChars instead
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
                if (_includeComments)
                {
                    exprs ~= Expr(Token(TOK.comment, src[0 .. 1]));
                }
                break;
            case '(':
                exprs ~= Expr(Token(TOK.leftParen, src[0 .. 1]));
                src.popFront();
                ++_depth;
                break;
            case ')':
                exprs ~= Expr(Token(TOK.rightParen, src[0 .. 1]));
                src.popFront();
                --_depth;

                exprs.popBack();   // pop right paren
                assert(!exprs.empty);

                // TODO retroIndexOf
                size_t count = 0; // number of elements between parens
                while (exprs[$ - 1 - count].token.tok != TOK.leftParen)
                {
                    ++count;
                }
                assert(count != 0);

                if (count >= 1)
                {
                    Expr newExpr = Expr(exprs[$ - count].token,
                                        exprs[$ - count + 1 .. $].dup);
                    exprs.popBackN(1 + count); // forget tokens including leftParen
                    exprs ~= newExpr.move;
                }

                if (_depth == 0) // top-level expression done
                {
                    assert(exprs.length >= 1); // we should have at least one `Expr`
                    return;
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
                // std.ascii.isDigit:
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
            case '+':
            case '-':
            case '.':
                const number = getNumber(src);
                exprs ~= Expr(Token(TOK.number, number));
                break;
                // from std.ascii.isWhite
            case ' ':
            case '\t':
            case '\n':
            case '\v':
            case '\r':
            case '\f':
                assert(src.front.isWhite);
                getWhitespace(src);
                if (_includeWhitespace)
                {
                    exprs ~= Expr(Token(TOK.whitespace, null));
                }
                break;
            case '\0':
                assert(_depth == 0, "Unbalanced parenthesis at end of file");
                _endOfFile = true;
                return;
            default:
                // other
                if (true// src.front.isAlpha
                    )
                {
                    const symbol = getSymbol(src); // TODO tokenize
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
                           `' at index:` ~ (&src[0] - &_whole[0]).to!string);
                }
                break;
            }
        }
    }

    private:
    alias Src = string;

    Src src;                    // remaining input

    const Src _whole;           // whole input

    Exprs exprs;                // current

    size_t _depth = 0;

    bool _endOfFile;

    bool _includeComments = false;
    bool _includeWhitespace = false;
}


@safe pure unittest
{
    const text = ";;a comment\n(instance AttrFn BinaryFunction)\0";
    auto exprs = SUOKIFParser(text);
    assert(!exprs.empty);

    assert(exprs.front.token.tok == TOK.symbol);
    assert(exprs.front.token.src == `instance`);

    assert(exprs.front.subs[0].token.tok == TOK.functionName);
    assert(exprs.front.subs[0].token.src == "AttrFn");

    assert(exprs.front.subs[1].token.tok == TOK.symbol);
    assert(exprs.front.subs[1].token.src == "BinaryFunction");
}

version(none)
unittest
{
    import std.path : expandTilde;
    import std.file : readText;
    const text = `~/elisp/mine/relangs.el`.expandTilde.readText;
    const ctext = text ~ '\0'; // null at the end to enable sentinel-based search in parser
    assert(ctext[$ - 1] == '\0');
    foreach (const ref expr; SUOKIFParser(ctext))
    {
    }
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
                write(`Reading SUO-KIF `, filePath, ` ... `);

                import std.file : readText;
                import std.datetime : StopWatch, AutoStart, Duration;
                auto sw = StopWatch(AutoStart.yes);

                // TODO move this logic to readText(bool nullTerminated = false) by .capacity += 1
                const text = filePath.readText;
                const ctext = text ~ '\0'; // null at the end to enable sentinel-based search in parser
                assert(ctext[$ - 1] == '\0');
                foreach (topExpr; SUOKIFParser(ctext))
                {
                    // TOOD use topExpr
                }
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
